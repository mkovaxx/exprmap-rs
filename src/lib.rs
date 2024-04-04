use std::{collections::HashMap, hash::Hash};

use slotmap::{DefaultKey, SlotMap};

#[derive(Debug, Clone)]
pub enum Expr {
    Var(usize),
    App(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum ExprMap<V> {
    Empty,
    Many(Many_ExprMap<V>),
}

#[derive(Debug, Clone)]
#[allow(non_camel_case_types)]
pub struct Many_ExprMap<V> {
    var: HashMap<usize, V>,
    app: Box<ExprMap<DefaultKey>>,

    // store all `ExprMap<V>`s here to avoid recursive type weirdness
    app_store: SlotMap<DefaultKey, ExprMap<V>>,
}

impl<V> ExprMap<V> {
    pub fn new() -> Self {
        Self::Empty
    }

    pub fn one(key: Expr, value: V) -> Self {
        match key {
            Expr::Var(key_var) => ExprMap::Many(Many_ExprMap {
                var: HashMap::from_iter([(key_var, value)]),
                app: Box::new(ExprMap::Empty),
                app_store: SlotMap::new(),
            }),
            Expr::App(f, x) => {
                let mut app_store = SlotMap::new();
                let app_key = app_store.insert(ExprMap::one(*x, value));
                ExprMap::Many(Many_ExprMap {
                    var: HashMap::new(),
                    app: Box::new(ExprMap::one(*f, app_key)),
                    app_store,
                })
            }
        }
    }

    pub fn get(&self, key: &Expr) -> Option<&V> {
        match self {
            ExprMap::Empty => None,
            ExprMap::Many(em) => match key {
                Expr::Var(id) => em.var.get(id),
                Expr::App(f, x) => em
                    .app
                    .get(f)
                    .and_then(|store_key| em.app_store[*store_key].get(x)),
            },
        }
    }

    pub fn insert(&mut self, key: Expr, value: V) {
        match self {
            ExprMap::Empty => *self = ExprMap::one(key, value),
            ExprMap::Many(em) => match key {
                Expr::Var(id) => {
                    em.var.insert(id, value);
                }
                Expr::App(f, x) => match em.app.get(&f) {
                    Some(store_key) => {
                        em.app_store[*store_key].insert(*x, value);
                    }
                    None => {
                        let app_key = em.app_store.insert(ExprMap::one(*x, value));
                        em.app.insert(*f, app_key);
                    }
                },
            },
        }
    }
}

impl<V> MergeWith<V> for ExprMap<V> {
    fn merge_with<F>(&mut self, that: Self, func: &mut F)
    where
        F: FnMut(&mut V, V),
    {
        if let ExprMap::Empty = that {
            return;
        }

        if let ExprMap::Empty = self {
            *self = that;
            return;
        }

        if let (ExprMap::Many(em1), ExprMap::Many(mut em2)) = (self, that) {
            em1.var.merge_with(em2.var, func);
            em1.app.merge_with(*em2.app, &mut |v, w| {
                let em = em2.app_store.remove(w).unwrap();
                em1.app_store[*v].merge_with(em, func);
            });
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
