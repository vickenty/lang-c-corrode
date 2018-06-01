use std::collections::HashMap;
use std::mem;
use std::ops::RangeFrom;

use c::{Function, Item, QualType, Struct, Variable};
use {Error, Ref};

pub struct Scope<'a> {
    names: HashMap<String, NameDef<'a>>,
    tags: HashMap<String, TagDef<'a>>,
}

impl<'a> Scope<'a> {
    fn new() -> Scope<'a> {
        Scope {
            names: HashMap::new(),
            tags: HashMap::new(),
        }
    }

    fn add_typedef(&mut self, name: &str, qty: QualType<'a>) {
        self.names.insert(name.into(), NameDef::Typedef(qty));
    }

    fn add_variable(&mut self, name: &str, var: Ref<'a, Variable<'a>>) {
        self.names.insert(name.into(), NameDef::Variable(var));
    }

    fn add_function(&mut self, name: &str, fun: Ref<'a, Function<'a>>) {
        self.names.insert(name.into(), NameDef::Function(fun));
    }

    fn lookup_name(&self, name: &str) -> Option<&NameDef<'a>> {
        self.names.get(name)
    }

    fn add_struct(&mut self, tag: &str, s: Ref<'a, Struct<'a>>) {
        self.tags.insert(tag.into(), TagDef::Struct(s));
    }

    fn lookup_tag(&self, tag: &str) -> Option<&TagDef<'a>> {
        self.tags.get(tag)
    }
}

pub struct Env<'a> {
    scopes: Vec<Scope<'a>>,
    struct_defs: Vec<Ref<'a, Struct<'a>>>,
    name_seq: RangeFrom<usize>,
}

impl<'a> Env<'a> {
    pub fn new() -> Env<'a> {
        Env {
            scopes: vec![Scope::new()],
            struct_defs: Vec::new(),
            name_seq: 0..,
        }
    }

    fn top(&mut self) -> &mut Scope<'a> {
        self.scopes.last_mut().expect("empty scope stack")
    }

    pub fn add_typedef(&mut self, name: &str, qty: QualType<'a>) {
        self.top().add_typedef(name, qty);
    }

    pub fn add_variable(&mut self, name: &str, var: Ref<'a, Variable<'a>>) {
        self.top().add_variable(name, var);
    }

    pub fn add_function(&mut self, name: &str, fun: Ref<'a, Function<'a>>) {
        self.top().add_function(name, fun);
    }

    pub fn add_struct(&mut self, s: Ref<'a, Struct<'a>>) {
        self.struct_defs.push(s);

        if let Some(ref tag) = s.tag {
            self.top().add_struct(tag, s);
        }
    }

    fn lookup_name(&self, name: &str) -> Option<&NameDef<'a>> {
        for scope in self.scopes.iter().rev() {
            if let Some(def) = scope.lookup_name(name) {
                return Some(def);
            }
        }
        None
    }

    pub fn lookup_typedef(&self, name: &str) -> Result<&QualType<'a>, Error> {
        match self.lookup_name(name) {
            Some(&NameDef::Typedef(ref qty)) => Ok(qty),
            Some(_) => Err("not a type"),
            None => Err("unknown type"),
        }
    }

    pub fn lookup_variable(&self, name: &str) -> Result<Option<Ref<'a, Variable<'a>>>, Error> {
        match self.lookup_name(name) {
            Some(&NameDef::Variable(var)) => Ok(Some(var)),
            Some(_) => Err("not a varaible"),
            None => Ok(None),
        }
    }

    pub fn lookup_function(&self, name: &str) -> Result<Option<Ref<'a, Function<'a>>>, Error> {
        match self.lookup_name(name) {
            Some(&NameDef::Function(fun)) => Ok(Some(fun)),
            Some(_) => Err("not a function"),
            None => Ok(None),
        }
    }

    fn lookup_tag(&self, tag: &str, top_only: bool) -> Option<&TagDef<'a>> {
        for scope in self.scopes.iter().rev() {
            if let Some(def) = scope.lookup_tag(tag) {
                return Some(def);
            }
            if top_only {
                break;
            }
        }
        None
    }

    pub fn lookup_struct(
        &self,
        tag: &str,
        top_only: bool,
    ) -> Option<Result<Ref<'a, Struct<'a>>, Error>> {
        match self.lookup_tag(tag, top_only) {
            Some(&TagDef::Struct(s)) => Some(Ok(s)),
            Some(_) => Some(Err("not a struct or a union")),
            None => None,
        }
    }

    pub fn mangle_name(&self, name: &mut String) {
        while self.lookup_name(&name).is_some() {
            name.push('_');
        }
    }

    pub fn generate_name(&mut self) -> String {
        loop {
            let name = format!("Generated_{}", self.name_seq.next().unwrap());
            if self.lookup_name(&name).is_none() {
                return name;
            }
        }
    }

    pub fn finalize(mut self, items: &mut Vec<Item<'a>>) {
        let defs = mem::replace(&mut self.struct_defs, Vec::new());

        for s in defs {
            s.postprocess(&mut self);
            items.push(Item::Struct(s));
        }
    }
}

enum NameDef<'a> {
    Variable(Ref<'a, Variable<'a>>),
    Typedef(QualType<'a>),
    Function(Ref<'a, Function<'a>>),
}

enum TagDef<'a> {
    Struct(Ref<'a, Struct<'a>>),
}
