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
}

impl<V> MapApi for Many_ExprMap<V> {
    type K = Expr;
    type V = V;

    fn single(key: Expr, value: V) -> Self {
        let mut m = Self::new();

        match key {
            Expr::Zero => {
                m.zero = MapApi::single((), value);
            }
            Expr::Var(k) => {
                m.var = MapApi::single(k, value);
            }
            Expr::App(f, x) => {
                m.app = MapApi::single(*f, MapApi::single(*x, value));
            }
        }

        m
    }

    fn get(&self, key: &Expr) -> Option<&V> {
        match key {
            Expr::Zero => self.zero.get(&()),
            Expr::Var(id) => {
                let v0 = &self.var;
                let v1 = v0.get(id)?;
                Some(v1)
            }
            Expr::App(f, x) => {
                let v0 = &self.app;
                let v1 = v0.get(&f)?;
                let v2 = v1.get(&x)?;
                Some(v2)
            }
        }
    }

    fn remove(&mut self, key: &Expr) -> Option<V> {
        match key {
            Expr::Zero => self.zero.remove(&()),
            Expr::Var(id) => {
                let v0 = &mut self.var;
                let mut v1 = v0.remove(id)?;
                Some(v1)
            }
            Expr::App(f, x) => {
                let v0 = &mut self.app;
                let mut v1 = v0.remove(&f)?;
                let mut v2 = v1.remove(&x)?;
                Some(v2)
            }
        }
    }

    fn merge_with(&mut self, that: Self, func: &mut dyn FnMut(&mut V, V)) {
        self.zero.merge_with(that.zero, func);
        self.var.merge_with(that.var, func);
        self.app
            .merge_with(that.app, &mut |m1, m2| m1.merge_with(m2, func));
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
}

impl<V> MapApi for ExprMap<V> {
    type K = Expr;
    type V = V;

    fn single(key: Expr, value: V) -> Self {
        Self::One(key, value)
    }

    fn get(&self, key: &Expr) -> Option<&V> {
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

    fn remove(&mut self, key: &Expr) -> Option<V> {
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

    fn merge_with(&mut self, that: Self, func: &mut dyn FnMut(&mut V, V)) {
        // an offering to the Borrow Checker
        let mut old_self = Self::Empty;
        std::mem::swap(self, &mut old_self);

        match (old_self, that) {
            (Self::Empty, that) => {
                *self = that;
            }
            (old_self, Self::Empty) => {
                *self = old_self;
            }
            (Self::One(k1, v1), Self::One(k2, v2)) => {
                let mut m1 = Many_ExprMap::single(k1, v1);
                let m2 = Many_ExprMap::single(k2, v2);
                m1.merge_with(m2, func);
                *self = Self::Many(Box::new(m1));
            }
            (Self::Many(mut m1), Self::One(k2, v2)) => {
                let m2 = Many_ExprMap::single(k2, v2);
                m1.merge_with(m2, func);
                *self = Self::Many(m1);
            }
            (Self::One(k1, v1), Self::Many(m2)) => {
                let mut m1 = Many_ExprMap::single(k1, v1);
                m1.merge_with(*m2, func);
                *self = Self::Many(Box::new(m1));
            }
            (Self::Many(mut m1), Self::Many(m2)) => {
                m1.merge_with(*m2, func);
                *self = Self::Many(m1);
            }
        }
    }
}

impl<M, V> MapApi for (M, SlotMap<DefaultKey, V>)
where
    M: MapApi<V = DefaultKey>,
{
    type K = M::K;
    type V = V;

    fn single(key: M::K, value: V) -> Self {
        let mut slotmap = SlotMap::new();
        let slot_k = slotmap.insert(value);
        (M::single(key, slot_k), slotmap)
    }

    fn get(&self, key: &M::K) -> Option<&V> {
        let slot_k = self.0.get(key)?;
        self.1.get(*slot_k)
    }

    fn remove(&mut self, key: &M::K) -> Option<V> {
        let slot_k = self.0.remove(key)?;
        let value = self.1.remove(slot_k).unwrap();
        Some(value)
    }

    fn merge_with(&mut self, mut that: Self, func: &mut dyn FnMut(&mut V, V)) {
        self.0.merge_with(that.0, &mut |slot_k1, slot_k2| {
            let value1 = &mut self.1[*slot_k1];
            let value2 = that.1.remove(slot_k2).unwrap();
            func(value1, value2);
        });
    }
}

impl<K, V> MapApi for HashMap<K, V>
where
    K: Eq + Hash,
{
    type K = K;
    type V = V;

    fn single(key: K, value: V) -> Self {
        Self::from_iter([(key, value)])
    }

    fn get(&self, key: &K) -> Option<&V> {
        self.get(key)
    }

    fn remove(&mut self, key: &K) -> Option<V> {
        self.remove(key)
    }

    fn merge_with(&mut self, that: Self, func: &mut dyn FnMut(&mut V, V)) {
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

impl<V> MapApi for Option<V> {
    type K = ();
    type V = V;

    fn single(key: Self::K, value: Self::V) -> Self {
        Some(value)
    }

    fn get(&self, key: &Self::K) -> Option<&Self::V> {
        self.as_ref()
    }

    fn remove(&mut self, key: &Self::K) -> Option<Self::V> {
        self.take()
    }

    fn merge_with(&mut self, that: Self, func: &mut dyn FnMut(&mut Self::V, Self::V)) {
        todo!()
    }
}

trait MapApi {
    type K;
    type V;

    fn single(key: Self::K, value: Self::V) -> Self;
    fn get(&self, key: &Self::K) -> Option<&Self::V>;
    fn remove(&mut self, key: &Self::K) -> Option<Self::V>;
    fn merge_with(&mut self, that: Self, func: &mut dyn FnMut(&mut Self::V, Self::V));

    fn insert(&mut self, key: Self::K, value: Self::V)
    where
        Self: Sized,
    {
        self.merge_with(Self::single(key, value), &mut |v, w| *v = w);
    }
}

#[cfg(test)]
mod test {
    use super::{Expr, ExprMap, MapApi};

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
