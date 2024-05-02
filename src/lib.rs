use std::{any::Any, collections::HashMap, hash::Hash};

trait AnySized: Any + Sized {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Zero,
    Var(usize),
    App(Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub struct Many_ExprMap<V> {
    zero: Option<V>,
    var: HashMap<usize, V>,
    // ExprMap<ExprMap<V>>
    app: ExprMap<Box<dyn Any>>,
}

impl<V: 'static> MapApi for Many_ExprMap<V> {
    type K = Expr;
    type V = V;

    fn empty() -> Self {
        Self {
            zero: MapApi::empty(),
            var: MapApi::empty(),
            app: MapApi::empty(),
        }
    }

    fn one(key: Expr, value: V) -> Self {
        let mut m = Self::empty();

        match key {
            Expr::Zero => {
                m.zero = MapApi::one((), value);
            }
            Expr::Var(k) => {
                m.var = MapApi::one(k, value);
            }
            Expr::App(f, x) => {
                let value: Box<dyn Any> = Box::new(ExprMap::one(*x, value));
                m.app = MapApi::one(*f, value);
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
                let v1 = v0.get(f)?;
                let v2 = v1.downcast_ref::<ExprMap<V>>().unwrap().get(x)?;
                Some(v2)
            }
        }
    }

    fn remove(&mut self, key: &Expr) -> Option<V> {
        match key {
            Expr::Zero => self.zero.remove(&()),
            Expr::Var(id) => {
                let v0 = &mut self.var;
                let v1 = v0.remove(id)?;
                Some(v1)
            }
            Expr::App(f, x) => {
                let v0 = &mut self.app;
                let v1 = v0.remove(f)?;
                let v2 = v1.downcast::<ExprMap<V>>().unwrap().remove(x)?;
                Some(v2)
            }
        }
    }

    fn for_each(&mut self, func: &mut dyn FnMut(&mut V)) {
        self.zero.for_each(func);
        self.var.for_each(func);
        self.app
            .for_each(&mut |m| m.downcast_mut::<ExprMap<V>>().unwrap().for_each(func));
    }

    fn insert_with(
        &mut self,
        key: Self::K,
        value: Self::V,
        func: &mut dyn FnMut(&mut Self::V, Self::V),
    ) {
        match key {
            Expr::Zero => self.zero.insert_with((), value, func),
            Expr::Var(k) => self.var.insert_with(k, value, func),
            Expr::App(f, x) => {
                self.app
                    .insert_with(*f, Box::new(ExprMap::one(*x, value)), &mut |m1, m2| {
                        let m1 = m1.downcast_mut::<ExprMap<V>>().unwrap();
                        let m2 = m2.downcast::<ExprMap<V>>().unwrap();
                        m1.merge_with(*m2, func)
                    })
            }
        }
    }

    fn merge_with(&mut self, that: Self, func: &mut dyn FnMut(&mut V, V)) {
        self.zero.merge_with(that.zero, func);
        self.var.merge_with(that.var, func);
        self.app.merge_with(that.app, &mut |m1, m2| {
            let m1 = m1.downcast_mut::<ExprMap<V>>().unwrap();
            let m2 = m2.downcast::<ExprMap<V>>().unwrap();
            m1.merge_with(*m2, func);
        });
    }
}

#[derive(Debug)]
pub enum ExprMap<V> {
    Empty,
    // TODO: consider putting `Box`es in here
    One(Expr, V),
    Many(Box<Many_ExprMap<V>>),
}

impl<V: 'static> MapApi for ExprMap<V> {
    type K = Expr;
    type V = V;

    fn empty() -> Self {
        Self::Empty
    }

    fn one(key: Expr, value: V) -> Self {
        Self::One(key, value)
    }

    fn get(&self, key: &Expr) -> Option<&V> {
        match self {
            Self::Empty => None,
            Self::One(k, value) => {
                if k == key {
                    Some(value)
                } else {
                    None
                }
            }
            Self::Many(m) => m.get(key),
        }
    }

    fn remove(&mut self, key: &Expr) -> Option<V> {
        // a humble offering to the Borrow Checker, Keeper of Lifetimes
        let mut old_self = Self::empty();
        std::mem::swap(self, &mut old_self);

        match old_self {
            Self::Empty => None,
            Self::One(k, value) => {
                if k == *key {
                    Some(value)
                } else {
                    *self = Self::One(k, value);
                    None
                }
            }
            Self::Many(mut m) => {
                let value = m.remove(key);
                // TODO: collapse into One when possible
                *self = Self::Many(m);
                value
            }
        }
    }

    fn for_each(&mut self, func: &mut dyn FnMut(&mut V)) {
        match self {
            ExprMap::Empty => {}
            ExprMap::One(_, value) => func(value),
            ExprMap::Many(m) => m.for_each(func),
        }
    }

    fn insert_with(&mut self, key: Expr, value: V, func: &mut dyn FnMut(&mut V, V)) {
        // a humble offering to the Borrow Checker, Keeper of Lifetimes
        let mut old_self = Self::empty();
        std::mem::swap(self, &mut old_self);

        match old_self {
            Self::Empty => {
                *self = Self::One(key, value);
            }
            Self::One(k, v) => {
                let mut m = Box::new(Many_ExprMap::one(k, v));
                m.insert_with(key, value, func);
                *self = Self::Many(m);
            }
            Self::Many(mut m) => {
                m.insert_with(key, value, func);
                *self = Self::Many(m);
            }
        }
    }

    fn merge_with(&mut self, that: Self, func: &mut dyn FnMut(&mut V, V)) {
        // a humble offering to the Borrow Checker, Keeper of Lifetimes
        let mut old_self = Self::empty();
        std::mem::swap(self, &mut old_self);

        match (old_self, that) {
            (Self::Empty, that) => {
                *self = that;
            }
            (old_self, Self::Empty) => {
                *self = old_self;
            }
            (Self::One(k1, v1), Self::One(k2, v2)) => {
                let mut m1 = Box::new(Many_ExprMap::one(k1, v1));
                m1.insert_with(k2, v2, func);
                *self = Self::Many(m1);
            }
            (Self::Many(mut m1), Self::One(k2, v2)) => {
                m1.insert_with(k2, v2, func);
                *self = Self::Many(m1);
            }
            (Self::One(k1, v1), Self::Many(m2)) => {
                let mut m1 = Box::new(Many_ExprMap::one(k1, v1));
                m1.merge_with(*m2, func);
                *self = Self::Many(m1);
            }
            (Self::Many(mut m1), Self::Many(m2)) => {
                m1.merge_with(*m2, func);
                *self = Self::Many(m1);
            }
        }
    }
}

impl<K, V> MapApi for HashMap<K, V>
where
    K: Eq + Hash,
{
    type K = K;
    type V = V;

    fn empty() -> Self {
        HashMap::new()
    }

    fn one(key: K, value: V) -> Self {
        Self::from_iter([(key, value)])
    }

    fn get(&self, key: &K) -> Option<&V> {
        self.get(key)
    }

    fn remove(&mut self, key: &K) -> Option<V> {
        self.remove(key)
    }

    fn for_each(&mut self, func: &mut dyn FnMut(&mut V)) {
        for (_, value) in self {
            func(value);
        }
    }

    fn insert_with(
        &mut self,
        key: Self::K,
        value: Self::V,
        func: &mut dyn FnMut(&mut Self::V, Self::V),
    ) {
        match self.entry(key) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                func(entry.get_mut(), value);
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(value);
            }
        }
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

    fn empty() -> Self {
        None
    }

    fn one(_key: (), value: V) -> Self {
        Some(value)
    }

    fn get(&self, _key: &()) -> Option<&V> {
        self.as_ref()
    }

    fn remove(&mut self, _key: &()) -> Option<V> {
        self.take()
    }

    fn for_each(&mut self, func: &mut dyn FnMut(&mut V)) {
        match self {
            Some(value) => func(value),
            None => {}
        }
    }

    fn insert_with(&mut self, _key: (), value: V, func: &mut dyn FnMut(&mut V, V)) {
        match self {
            Some(old_value) => func(old_value, value),
            None => *self = Some(value),
        }
    }

    fn merge_with(&mut self, that: Self, func: &mut dyn FnMut(&mut V, V)) {
        // a humble offering to the Borrow Checker, Keeper of Lifetimes
        let mut old_self = Self::empty();
        std::mem::swap(self, &mut old_self);

        match (old_self, that) {
            (None, None) => {}
            (None, Some(v)) => *self = Some(v),
            (Some(v), None) => *self = Some(v),
            (Some(mut v), Some(w)) => {
                func(&mut v, w);
                *self = Some(v);
            }
        }
    }
}

trait MapApi {
    type K;
    type V;

    fn empty() -> Self;
    fn one(key: Self::K, value: Self::V) -> Self;

    fn get(&self, key: &Self::K) -> Option<&Self::V>;
    fn remove(&mut self, key: &Self::K) -> Option<Self::V>;

    fn for_each(&mut self, func: &mut dyn FnMut(&mut Self::V));

    fn insert_with(
        &mut self,
        key: Self::K,
        value: Self::V,
        func: &mut dyn FnMut(&mut Self::V, Self::V),
    );
    fn merge_with(&mut self, that: Self, func: &mut dyn FnMut(&mut Self::V, Self::V));

    fn insert(&mut self, key: Self::K, value: Self::V) {
        self.insert_with(key, value, &mut |v, w| *v = w);
    }
}

#[cfg(test)]
mod test {
    use super::{Expr, ExprMap, MapApi};

    #[test]
    fn test_var_key() {
        let mut em = ExprMap::<String>::empty();
        let key = Expr::Var(42);
        let value = String::from("hello");
        em.insert(key.clone(), value.clone());
        dbg!(&em);

        let result = em.get(&key);

        assert_eq!(result, Some(&value));
    }

    #[test]
    fn test_app_key() {
        let mut em = ExprMap::<String>::empty();
        let key = Expr::App(Box::new(Expr::Var(5)), Box::new(Expr::Var(73)));
        let value = String::from("hello");
        em.insert(key.clone(), value.clone());
        dbg!(&em);

        let result = em.get(&key);

        assert_eq!(result, Some(&value));
    }
}
