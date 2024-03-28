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
        app: Box<ExprMap<ExprMapRef>>,

        // store all `ExprMap<T>`s here to avoid recursive type weirdness
        expr_maps: Vec<ExprMap<T>>,
    },
}

#[derive(Debug, Clone)]
struct ExprMapRef(usize);

impl<T> ExprMap<T> {
    pub fn new() -> Self {
        Self::Empty
    }

    pub fn one(key: Expr, value: T) -> Self {
        match key {
            Expr::Var(key_var) => ExprMap::Many {
                var: HashMap::from_iter([(key_var, value)]),
                app: Box::new(ExprMap::Empty),
                expr_maps: vec![],
            },
            Expr::App(f, x) => ExprMap::Many {
                var: HashMap::new(),
                app: Box::new(ExprMap::one(*f, ExprMapRef(0))),
                expr_maps: vec![ExprMap::one(*x, value)],
            },
        }
    }

    pub fn get(&self, key: &Expr) -> Option<&T> {
        match self {
            ExprMap::Empty => None,
            ExprMap::Many {
                var: vars,
                app: apps,
                expr_maps,
            } => match key {
                Expr::Var(id) => vars.get(id),
                Expr::App(f, x) => apps.get(f).and_then(|em_ref: &ExprMapRef| {
                    let em = &expr_maps[em_ref.0];
                    em.get(x)
                }),
            },
        }
    }

    pub fn insert(&mut self, key: Expr, value: T) {
        match self {
            ExprMap::Empty => *self = ExprMap::one(key, value),
            ExprMap::Many {
                var: vars,
                app: apps,
                expr_maps,
            } => match key {
                Expr::Var(id) => {
                    vars.insert(id, value);
                }
                Expr::App(f, x) => match apps.get(&f) {
                    Some(em_ref) => {
                        expr_maps[em_ref.0].insert(*x, value);
                    }
                    None => {
                        let em_ref: ExprMapRef = ExprMapRef(expr_maps.len());
                        let mut only_x: ExprMap<T> = ExprMap::new();
                        only_x.insert(*x, value);
                        expr_maps.push(only_x);
                        apps.insert(*f, em_ref);
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
