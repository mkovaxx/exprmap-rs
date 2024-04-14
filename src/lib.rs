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
    // factor `ExprMap<ExprMap<V>>` into two parts to prevent recursive type weirdness
    app: (ExprMap<DefaultKey>, SlotMap<DefaultKey, ExprMap<V>>),
}

impl<V> Many_ExprMap<V> {
    pub fn new() -> Self {
        Self {
            zero: None,
            var: HashMap::new(),
            app: (ExprMap::Empty, SlotMap::new()),
        }
    }

    pub fn get(&self, key: &Expr) -> Option<&V> {
        match key {
            Expr::Zero => self.zero.as_ref(),
            Expr::Var(id) => self.var.get(id),
            Expr::App(f, x) => {
                let store_key = self.app.0.get(f)?;
                self.app.1[*store_key].get(x)
            }
        }
    }

    pub fn remove(&mut self, key: &Expr) -> Option<V> {
        match key {
            Expr::Zero => self.zero.take(),
            Expr::Var(id) => self.var.remove(id),
            Expr::App(f, x) => {
                let store_key = self.app.0.remove(&f)?;
                self.app.1[store_key].remove(x)
            }
        }
    }
}

impl<V> MergeWith<Expr, V> for Many_ExprMap<V> {
    fn insert_with<F>(&mut self, key: Expr, value: V, func: &mut F)
    where
        F: FnMut(&mut V, V),
    {
        match key {
            Expr::Zero => match self.zero.as_mut() {
                Some(mut v) => func(&mut v, value),
                None => self.zero = Some(value),
            },
            Expr::Var(var_key) => {
                self.var.insert_with(var_key, value, func);
            }
            Expr::App(f, x) => {
                todo!();
            }
        }
    }

    fn merge_with<F>(&mut self, mut that: Self, func: &mut F)
    where
        F: FnMut(&mut V, V),
    {
        self.var.merge_with(that.var, func);
        self.app.0.merge_with(that.app.0, &mut |v, w| {
            let em = that.app.1.remove(w).unwrap();
            self.app.1[*v].merge_with(em, func);
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
        self.merge_with(ExprMap::One(key, value), &mut |v, w| *v = w);
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

impl<V> MergeWith<Expr, V> for ExprMap<V> {
    fn insert_with<F>(&mut self, key: Expr, value: V, func: &mut F)
    where
        F: FnMut(&mut V, V),
    {
        todo!()
    }

    fn merge_with<F>(&mut self, mut that: Self, func: &mut F)
    where
        F: FnMut(&mut V, V),
    {
        // an offering to the Borrow Checker
        let mut old_self = ExprMap::Empty;
        std::mem::swap(self, &mut old_self);

        match (old_self, that) {
            (ExprMap::Empty, that) => {
                *self = that;
            }
            (old_self, ExprMap::Empty) => {
                *self = old_self;
            }
            (ExprMap::One(k1, v1), ExprMap::One(k2, v2)) => {
                let mut m = Box::new(Many_ExprMap::new());
                m.insert_with(k1, v1, func);
                m.insert_with(k2, v2, func);
                *self = ExprMap::Many(m);
            }
            (ExprMap::Many(mut m1), ExprMap::One(k2, v2)) => {
                m1.insert_with(k2, v2, func);
                *self = ExprMap::Many(m1);
            }
            (ExprMap::One(k1, v1), ExprMap::Many(mut m2)) => {
                m2.insert_with(k1, v1, func);
                *self = ExprMap::Many(m2);
            }
            (ExprMap::Many(mut m1), ExprMap::Many(m2)) => {
                m1.merge_with(*m2, func);
                *self = ExprMap::Many(m1);
            }
        }
    }
}

impl<K, V> MergeWith<K, V> for HashMap<K, V>
where
    K: Eq + Hash,
{
    fn insert_with<F>(&mut self, key: K, value: V, func: &mut F)
    where
        F: FnMut(&mut V, V),
    {
        match self.entry(key) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                func(entry.get_mut(), value);
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(value);
            }
        }
    }

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

trait MergeWith<K, V> {
    fn insert_with<F>(&mut self, key: K, value: V, func: &mut F)
    where
        F: FnMut(&mut V, V);

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
