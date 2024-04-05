use std::{collections::HashMap, hash::Hash};

use slotmap::{DefaultKey, SlotMap};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Zero,
    Var(usize),
    App(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
#[allow(non_camel_case_types)]
pub struct Many_ExprMap<V> {
    zero: Option<V>,
    var: HashMap<usize, V>,
    app: ExprMap<DefaultKey>,

    // store all `ExprMap<V>`s here to avoid recursive type weirdness
    app_store: SlotMap<DefaultKey, ExprMap<V>>,
}

impl<V> Many_ExprMap<V> {
    pub fn new() -> Self {
        Self {
            zero: None,
            var: HashMap::new(),
            app: ExprMap::Empty,
            app_store: SlotMap::new(),
        }
    }

    pub fn get(&self, key: &Expr) -> Option<&V> {
        match key {
            Expr::Zero => self.zero.as_ref(),
            Expr::Var(id) => self.var.get(id),
            Expr::App(f, x) => self
                .app
                .get(f)
                .and_then(|store_key| self.app_store[*store_key].get(x)),
        }
    }

    pub fn insert(&mut self, key: Expr, value: V) {
        match key {
            Expr::Zero => {
                self.zero = Some(value);
            }
            Expr::Var(id) => {
                self.var.insert(id, value);
            }
            Expr::App(f, x) => match self.app.get(&f) {
                Some(store_key) => {
                    self.app_store[*store_key].insert(*x, value);
                }
                None => {
                    let app_key = self.app_store.insert(ExprMap::One(*x, value));
                    self.app.insert(*f, app_key);
                }
            },
        }
    }

    pub fn remove(&mut self, key: &Expr) -> Option<V> {
        match key {
            Expr::Zero => self.zero.take(),
            Expr::Var(id) => self.var.remove(id),
            Expr::App(f, x) => self
                .app
                .remove(&f)
                .and_then(|store_key| self.app_store[store_key].remove(x)),
        }
    }
}

impl<V> MergeWith<V> for Many_ExprMap<V> {
    fn merge_with<F>(&mut self, mut that: Self, func: &mut F)
    where
        F: FnMut(&mut V, V),
    {
        self.var.merge_with(that.var, func);
        self.app.merge_with(that.app, &mut |v, w| {
            let em = that.app_store.remove(w).unwrap();
            self.app_store[*v].merge_with(em, func);
        });
    }
}

#[derive(Debug, Clone)]
pub enum ExprMap<V> {
    Empty,
    // TODO: consider putting `Box`es in here
    One(Expr, V),
    Many(Box<Many_ExprMap<V>>),
}

impl<V> ExprMap<V> {
    pub fn new() -> Self {
        Self::Empty
    }

    pub fn get(&self, key: &Expr) -> Option<&V> {
        match self {
            ExprMap::Empty => None,
            ExprMap::One(k, value) => {
                if k == key {
                    Some(value)
                } else {
                    None
                }
            }
            ExprMap::Many(em) => em.get(key),
        }
    }

    pub fn insert(&mut self, key: Expr, value: V) {
        // an offering to the Borrow Checker
        let mut old_self = ExprMap::Empty;
        std::mem::swap(self, &mut old_self);

        match old_self {
            ExprMap::Empty => {
                *self = ExprMap::One(key, value);
            }
            ExprMap::One(k, v) => {
                let mut em = Box::new(Many_ExprMap::new());
                em.insert(k, v);
                em.insert(key, value);
                *self = ExprMap::Many(em);
            }
            ExprMap::Many(mut em) => {
                em.insert(key, value);
                *self = ExprMap::Many(em);
            }
        }
    }

    pub fn remove(&mut self, key: &Expr) -> Option<V> {
        // an offering to the Borrow Checker
        let mut old_self = ExprMap::Empty;
        std::mem::swap(self, &mut old_self);

        match old_self {
            ExprMap::Empty => {
                *self = ExprMap::Empty;
                None
            }
            ExprMap::One(k, value) => {
                if k == *key {
                    *self = ExprMap::Empty;
                    Some(value)
                } else {
                    *self = ExprMap::One(k, value);
                    None
                }
            }
            ExprMap::Many(mut em) => {
                let value = em.remove(key);
                // TODO: collapse into One when possible
                *self = ExprMap::Many(em);
                value
            }
        }
    }
}

impl<V> MergeWith<V> for ExprMap<V> {
    fn merge_with<F>(&mut self, that: Self, func: &mut F)
    where
        F: FnMut(&mut V, V),
    {
        // an offering to the Borrow Checker
        let mut old_self = ExprMap::Empty;
        std::mem::swap(self, &mut old_self);

        match old_self {
            ExprMap::Empty => {
                *self = that;
            }
            ExprMap::One(key, value) => {
                *self = that;
                self.insert(key, value);
            }
            ExprMap::Many(mut em) => match that {
                ExprMap::Empty => {}
                ExprMap::One(key, value) => {
                    em.insert(key, value);
                    *self = ExprMap::Many(em);
                }
                ExprMap::Many(em_that) => {
                    em.merge_with(*em_that, func);
                    *self = ExprMap::Many(em);
                }
            },
        }
    }
}

impl<K, V> MergeWith<V> for HashMap<K, V>
where
    K: Eq + Hash,
{
    fn merge_with<F>(&mut self, that: Self, func: &mut F)
    where
        F: FnMut(&mut V, V),
    {
        for (k2, v2) in that {
            match self.entry(k2) {
                std::collections::hash_map::Entry::Occupied(mut entry) => {
                    func(entry.get_mut(), v2);
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(v2);
                }
            }
        }
    }
}

trait MergeWith<V> {
    fn merge_with<F>(&mut self, that: Self, func: &mut F)
    where
        F: FnMut(&mut V, V);
}

#[cfg(test)]
mod test {
    use super::{Expr, ExprMap};

    #[test]
    fn test_var_key() {
        let mut em = ExprMap::<String>::new();
        let key = Expr::Var(42);
        let value = String::from("hello");
        em.insert(key.clone(), value.clone());
        dbg!(&em);

        let result = em.get(&key);

        assert_eq!(result, Some(&value));
    }

    #[test]
    fn test_app_key() {
        let mut em = ExprMap::<String>::new();
        let key = Expr::App(Box::new(Expr::Var(5)), Box::new(Expr::Var(73)));
        let value = String::from("hello");
        em.insert(key.clone(), value.clone());
        dbg!(&em);

        let result = em.get(&key);

        assert_eq!(result, Some(&value));
    }
}
