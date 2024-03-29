use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Expr {
    Var(usize),
    App(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum ExprMap<T> {
    Empty,
    Many {
        var: HashMap<usize, T>,
        app: Box<ExprMap<usize>>,

        // store all `ExprMap<T>`s here to avoid recursive type weirdness
        app_store: Vec<ExprMap<T>>,
    },
}

impl<T> ExprMap<T> {
    pub fn new() -> Self {
        Self::Empty
    }

    pub fn one(key: Expr, value: T) -> Self {
        match key {
            Expr::Var(key_var) => ExprMap::Many {
                var: HashMap::from_iter([(key_var, value)]),
                app: Box::new(ExprMap::Empty),
                app_store: vec![],
            },
            Expr::App(f, x) => ExprMap::Many {
                var: HashMap::new(),
                app: Box::new(ExprMap::one(*f, 0)),
                app_store: vec![ExprMap::one(*x, value)],
            },
        }
    }

    pub fn get(&self, key: &Expr) -> Option<&T> {
        match self {
            ExprMap::Empty => None,
            ExprMap::Many {
                var: vars,
                app: apps,
                app_store: expr_maps,
            } => match key {
                Expr::Var(id) => vars.get(id),
                Expr::App(f, x) => apps
                    .get(f)
                    .and_then(|index: &usize| expr_maps[*index].get(x)),
            },
        }
    }

    pub fn insert(&mut self, key: Expr, value: T) {
        match self {
            ExprMap::Empty => *self = ExprMap::one(key, value),
            ExprMap::Many {
                var,
                app,
                app_store,
            } => match key {
                Expr::Var(id) => {
                    var.insert(id, value);
                }
                Expr::App(f, x) => match app.get(&f) {
                    Some(index) => {
                        app_store[*index].insert(*x, value);
                    }
                    None => {
                        let index = app_store.len();
                        app_store.push(ExprMap::one(*x, value));
                        app.insert(*f, index);
                    }
                },
            },
        }
    }
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
