use c::{derive_declarator, expr, Env, FunctionTy, QualType, Struct, Type, TypeBuilder};
use std::cell::{Cell, RefCell};
use {ast, Alloc, Error, Node, Ref};

#[derive(Debug)]
pub struct Unit<'a> {
    pub items: Vec<Item<'a>>,
}

impl<'a> Unit<'a> {
    pub fn from_ast(alloc: &'a Alloc<'a>, unit: &ast::TranslationUnit) -> Result<Unit<'a>, Error> {
        let mut env = Env::new();
        let mut items = Vec::new();

        for ed in &unit.0 {
            match ed.node {
                ast::ExternalDeclaration::Declaration(ref decl) => {
                    items.extend(Item::from_ast(alloc, &mut env, true, decl)?)
                }
                _ => unimplemented!(),
            }
        }

        env.finalize(&mut items);

        Ok(Unit { items: items })
    }

    pub fn run_passes(&self) -> Result<(), Error> {
        self.add_static_initializers()?;
        self.add_static_casts()?;

        Ok(())
    }

    fn add_static_initializers(&self) -> Result<(), Error> {
        for item in &self.items {
            if let Item::Variable(ref v) = *item {
                let mut init = v.initial.borrow_mut();
                if init.is_none() {
                    *init = Some(expr::Expression::new_zero(v.ty.ty.clone())?)
                }
            }
        }

        Ok(())
    }

    fn add_static_casts(&self) -> Result<(), Error> {
        for item in &self.items {
            if let Item::Variable(ref v) = *item {
                if let Some(ref mut expr) = *v.initial.borrow_mut() {
                    if expr.ty() != v.ty.ty {
                        *expr = expr::Expression::new_cast(*expr.clone(), v.ty.ty.clone())?;
                    }
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Item<'a> {
    Variable(Ref<'a, Variable<'a>>),
    Function(Ref<'a, Function<'a>>),
    Struct(Ref<'a, Struct<'a>>),
}

impl<'a> Item<'a> {
    fn from_ast(
        alloc: &'a Alloc<'a>,
        env: &mut Env<'a>,
        file_scope: bool,
        decl: &Node<ast::Declaration>,
    ) -> Result<Vec<Item<'a>>, Error> {
        let mut builder = TypeBuilder::new();
        let mut storage = None;
        let mut inline = false;
        let mut noreturn = false;
        let mut is_typedef = false;

        for spec in &decl.node.specifiers {
            let mut new_storage = None;
            match spec.node {
                ast::DeclarationSpecifier::StorageClass(ref s) => match s.node {
                    ast::StorageClassSpecifier::ThreadLocal => {
                        return Err("_Thread_local is not supported")
                    }
                    ast::StorageClassSpecifier::Typedef => is_typedef = true,
                    ref other => new_storage = Some(other),
                },
                ast::DeclarationSpecifier::TypeSpecifier(ref s) => {
                    builder.visit_specifier(alloc, env, &s.node)?
                }
                ast::DeclarationSpecifier::TypeQualifier(ref s) => {
                    builder.visit_qualifier(&s.node)?
                }
                ast::DeclarationSpecifier::Function(ref s) => match s.node {
                    ast::FunctionSpecifier::Inline => inline = true,
                    ast::FunctionSpecifier::Noreturn => noreturn = true,
                },
                ast::DeclarationSpecifier::Alignment(_) => {
                    return Err("alignment specifiers not supported")
                }
                ast::DeclarationSpecifier::Extension(_) => (), // ignored for now
            }

            if new_storage.is_some() {
                if storage.is_none() {
                    storage = new_storage;
                } else {
                    return Err("multiple storage specifiers in a declaration");
                }
            }
        }

        let linkage = match (file_scope, storage) {
            (true, Some(&ast::StorageClassSpecifier::Static)) => Linkage::Internal,
            (true, _) => Linkage::External,
            (false, _) => Linkage::None,
        };

        let inline = inline && storage.is_none();

        let base_qty = builder.build(env)?;

        let mut res = vec![];

        for init_declarator in &decl.node.declarators {
            let (qty, name) = derive_declarator(
                alloc,
                env,
                base_qty.clone(),
                &init_declarator.node.declarator,
            )?;

            let name = name.expect("declaration with abstract declartor");

            let value = match init_declarator.node.initializer.as_ref() {
                Some(v) => Some(expr::Expression::from_initializer(&v.node)?),
                None => None,
            };

            match (&qty.ty, is_typedef, value) {
                (_, true, None) => env.add_typedef(&name, qty.clone()),
                (_, true, Some(_)) => return Err("typedef is initialized"),

                (&Type::Function(ref fun_ty), false, None) => {
                    let fun = match env.lookup_function(&name)? {
                        Some(fun) => {
                            fun.inline.set(fun.inline.get() && inline);
                            fun.noreturn.set(fun.noreturn.get() || noreturn);
                            fun
                        }
                        None => {
                            let fun = alloc.new_function(Function {
                                linkage: linkage,
                                name: name.clone(),
                                ty: fun_ty.clone(),
                                inline: inline.into(),
                                noreturn: noreturn.into(),
                            });
                            env.add_function(&name, fun);
                            fun
                        }
                    };

                    res.push(Item::Function(fun));
                }

                (&Type::Function(_), false, Some(_)) => return Err("function is initialized"),

                (_, false, value) => {
                    let is_extern = storage == Some(&ast::StorageClassSpecifier::Extern);

                    let var = match is_extern || file_scope {
                        true => env.lookup_variable(&name)?,
                        false => None,
                    };

                    let var = match var {
                        Some(var) => {
                            var.ty.merge(&qty);
                            var
                        }
                        None => {
                            let var = alloc.new_variable(Variable {
                                linkage: linkage,
                                defined: false.into(),
                                name: name.clone(),
                                ty: qty.clone(),
                                initial: None.into(),
                            });
                            env.add_variable(&name, var);
                            var
                        }
                    };

                    var.defined
                        .set(var.defined.get() || !is_extern || value.is_some());
                    *var.initial.borrow_mut() = value;

                    res.push(Item::Variable(var));
                }
            }
        }

        Ok(res)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable<'a> {
    pub linkage: Linkage,
    pub name: String,
    pub defined: Cell<bool>,
    pub ty: QualType<'a>,
    pub initial: RefCell<Option<Box<expr::Expression<'a>>>>,
}

impl<'a> Variable<'a> {
    pub fn is_defined(&self) -> bool {
        self.defined.get()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function<'a> {
    pub name: String,
    pub linkage: Linkage,
    pub ty: Box<FunctionTy<'a>>,
    pub noreturn: Cell<bool>,
    pub inline: Cell<bool>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Linkage {
    None,
    Internal,
    External,
}

#[cfg(test)]
use {c::Parameter, lang_c};

#[cfg(test)]
fn interpret_decl_str<'a>(alloc: &'a Alloc<'a>, decl_str: &str) -> Result<Vec<Item<'a>>, Error> {
    let conf = &Default::default();
    let parse =
        lang_c::driver::parse_preprocessed(conf, decl_str.to_owned()).expect("syntax error");
    Unit::from_ast(alloc, &parse.unit).map(|u| u.items)
}

#[test]
fn test_decl() {
    let alloc = &Alloc::new();

    assert_eq!(
        interpret_decl_str(alloc, "extern int x, * const y;"),
        Ok(vec![
            Item::Variable(alloc.new_variable(Variable {
                linkage: Linkage::External,
                defined: false.into(),
                initial: None.into(),
                name: "x".into(),
                ty: QualType {
                    volatile: false,
                    constant: false,
                    ty: Type::SInt,
                },
            })),
            Item::Variable(alloc.new_variable(Variable {
                linkage: Linkage::External,
                defined: false.into(),
                initial: None.into(),
                name: "y".into(),
                ty: QualType {
                    volatile: false,
                    constant: true,
                    ty: Type::Pointer(Box::new(QualType {
                        volatile: false,
                        constant: false,
                        ty: Type::SInt,
                    })),
                },
            })),
        ])
    );
}

#[test]
fn test_external_def() {
    let alloc = &Alloc::new();

    let x = alloc.new_variable(Variable {
        linkage: Linkage::Internal,
        defined: true.into(),
        initial: None.into(),
        name: "x".into(),
        ty: QualType {
            volatile: false,
            constant: false,
            ty: Type::SInt,
        },
    });

    let y = alloc.new_variable(Variable {
        linkage: Linkage::External,
        defined: false.into(),
        initial: None.into(),
        name: "y".into(),
        ty: QualType {
            volatile: false,
            constant: false,
            ty: Type::SInt,
        },
    });

    let z = alloc.new_variable(Variable {
        linkage: Linkage::External,
        defined: true.into(),
        initial: None.into(),
        name: "z".into(),
        ty: QualType {
            volatile: false,
            constant: false,
            ty: Type::SInt,
        },
    });

    assert_eq!(
        interpret_decl_str(
            alloc,
            "static int x; extern int x; \
             extern int y; extern int y; \
             extern int z; int z;"
        ),
        Ok(vec![
            Item::Variable(x),
            Item::Variable(x),
            Item::Variable(y),
            Item::Variable(y),
            Item::Variable(z),
            Item::Variable(z),
        ])
    );
}

#[test]
fn test_extern_init() {
    let alloc = &Alloc::new();
    assert_eq!(
        interpret_decl_str(alloc, "extern int a = 1;"),
        Ok(vec![Item::Variable(
            alloc.new_variable(Variable {
                name: "a".into(),
                linkage: Linkage::External,
                defined: true.into(),
                initial: Some(
                    expr::Expression::Constant(
                        expr::Constant::Integer(expr::Integer {
                            base: expr::IntegerBase::Decimal,
                            ty: Type::SInt,
                            number: "1".into(),
                        }).into(),
                    ).into(),
                ).into(),
                ty: QualType {
                    volatile: false,
                    constant: false,
                    ty: Type::SInt,
                },
            }),
        )])
    );
}

#[test]
fn test_typedef() {
    let alloc = &Alloc::new();
    assert_eq!(
        interpret_decl_str(alloc, "volatile int typedef a, *b; a c; const b d;"),
        Ok(vec![
            Item::Variable(alloc.new_variable(Variable {
                linkage: Linkage::External,
                defined: true.into(),
                initial: None.into(),
                name: "c".into(),
                ty: QualType {
                    volatile: true,
                    constant: false,
                    ty: Type::SInt,
                },
            })),
            Item::Variable(alloc.new_variable(Variable {
                linkage: Linkage::External,
                defined: true.into(),
                initial: None.into(),
                name: "d".into(),
                ty: QualType {
                    volatile: false,
                    constant: true,
                    ty: Type::Pointer(Box::new(QualType {
                        volatile: true,
                        constant: false,
                        ty: Type::SInt,
                    })),
                },
            })),
        ])
    );
}

#[test]
fn test_typedef_fn() {
    let alloc = &Alloc::new();
    assert_eq!(
        interpret_decl_str(alloc, "typedef int foo(); foo a;"),
        Ok(vec![Item::Function(alloc.new_function(Function {
            linkage: Linkage::External,
            name: "a".into(),
            inline: false.into(),
            noreturn: false.into(),
            ty: Box::new(FunctionTy {
                return_type: Type::SInt.into(),
                parameters: Vec::new(),
                variadic: false,
            }),
        }))])
    )
}

#[test]
fn test_struct() {
    let alloc = &Alloc::new();

    let decls = interpret_decl_str(alloc, "struct x { struct x (*next); } head;").unwrap();
    assert!(decls.len() == 2);
    let decl = decls.get(0).unwrap();
    let head = match *decl {
        Item::Variable(v) => v,
        _ => panic!("not a variable"),
    };

    assert_eq!(head.name, "head");

    let head_sty = match head.ty.ty {
        Type::Struct(s) => s,
        _ => panic!("head has wrong type: {:#?}", head.ty),
    };

    let fields_ref = head_sty.fields.borrow();
    let fields = fields_ref.as_ref().unwrap();
    assert_eq!(fields.len(), 1);

    let next = fields.get(0).unwrap();
    match next.ty.ty {
        Type::Pointer(ref qty) => match qty.ty {
            Type::Struct(next_sty) => assert_eq!(Ref::eq(&next_sty, &head_sty), true),
            _ => panic!("next has wrong type: {:#?}", next.ty),
        },
        _ => panic!("next has wrong type: {:#?}", next.ty),
    }
}

#[test]
fn test_function_ptr() {
    let alloc = &Alloc::new();
    assert_eq!(
        interpret_decl_str(alloc, "int (*p)(int, ...);"),
        Ok(vec![Item::Variable(
            alloc.new_variable(Variable {
                name: "p".into(),
                defined: true.into(),
                linkage: Linkage::External,
                initial: None.into(),
                ty: Type::Pointer(
                    FunctionTy {
                        variadic: true,
                        return_type: Type::SInt.into(),
                        parameters: vec![Parameter {
                            name: None,
                            ty: Type::SInt.into(),
                        }],
                    }.into(),
                ).into(),
            }),
        )])
    );
}
