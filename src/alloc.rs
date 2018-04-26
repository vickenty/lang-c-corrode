use std::{fmt, ops};
use std::cell::RefCell;
use std::collections::HashMap;

use dynamic_arena;
use {Variable, Field, Struct, Function};

pub struct Alloc<'a>(dynamic_arena::DynamicArena<'a>);

impl<'a> Alloc<'a> {
    pub fn new() -> Alloc<'a> {
        Alloc(dynamic_arena::DynamicArena::new_bounded())
    }
    pub fn new_variable(&'a self, v: Variable<'a>) -> Ref<'a, Variable<'a>> {
        Ref(self.0.alloc(v))
    }
    pub fn new_struct(&'a self, v: Struct<'a>) -> Ref<'a, Struct<'a>> {
        Ref(self.0.alloc(v))
    }
    pub fn new_field(&'a self, v: Field<'a>) -> Ref<'a, Field<'a>> {
        Ref(self.0.alloc(v))
    }
    pub fn new_function(&'a self, v: Function<'a>) -> Ref<'a, Function<'a>> {
        Ref(self.0.alloc(v))
    }
}

#[derive(PartialEq)]
pub struct Ref<'a, T: 'a>(pub &'a T);

pub type RefId = usize;

impl<'a, T: PartialEq> Ref<'a, T> {
    pub fn same_as(&self, other: &Self) -> bool {
        self.0 as *const _ == other.0 as *const _
    }

    pub fn id(&self) -> RefId {
        self.0 as *const _ as usize
    }
}

impl<'a, T: fmt::Debug> fmt::Debug for Ref<'a, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        thread_local! {
            static SEEN: RefCell<HashMap<usize, (usize, bool)>> = RefCell::new(HashMap::new());
        }

        let addr = self.0 as *const _ as usize;
        SEEN.with(|seen_map| {
            let seen = seen_map.borrow().get(&addr).cloned();
            match seen {
                Some((id, _)) => {
                    seen_map.borrow_mut().get_mut(&addr).unwrap().1 = true;
                    write!(fmt, "\\{}", id)
                }
                None => {
                    let id = seen_map.borrow().len();
                    seen_map.borrow_mut().insert(addr, (id, false));
                    let repr = match fmt.alternate() {
                        true => format!("{:#?}", self.0),
                        false => format!("{:?}", self.0),
                    };
                    let used = seen_map
                        .borrow()
                        .get(&addr)
                        .map(|&(_, used)| used)
                        .unwrap_or(false);
                    if used {
                        write!(fmt, "<{}> {}", id, repr)?;
                    } else {
                        write!(fmt, "{}", repr)?;
                    }
                    seen_map.borrow_mut().remove(&addr);
                    Ok(())
                }
            }
        })
    }
}

impl<'a, T> Clone for Ref<'a, T> {
    fn clone(&self) -> Ref<'a, T> {
        Ref(self.0)
    }
}

impl<'a, T> Copy for Ref<'a, T> {}

impl<'a, T> ops::Deref for Ref<'a, T> {
    type Target = T;
    fn deref(&self) -> &T {
        &*self.0
    }
}
