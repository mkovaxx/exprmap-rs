use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Expr {
    Var(usize),
    App(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct ExprMap<T> {
    vars: HashMap<usize, T>,
    apps: Option<Box<ExprMap<ExprMapRef>>>,

    // store all `ExprMap<T>`s here to avoid recursive type weirdness
    expr_maps: Vec<ExprMap<T>>,
}

#[derive(Debug, Clone)]
struct ExprMapRef(usize);

impl<T> ExprMap<T> {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            // being able to set this simply to None avoids unbounded recursion
            apps: None,
            expr_maps: vec![],
        }
    }

    pub fn get(&self, key: &Expr) -> Option<&T> {
        match key {
            Expr::Var(id) => self.vars.get(id),
            Expr::App(f, x) => self.apps.as_ref().and_then(|apps| {
                apps.get(f).and_then(|em_ref: &ExprMapRef| {
                    let em = &self.expr_maps[em_ref.0];
                    em.get(x)
                })
            }),
        }
    }

    pub fn insert(&mut self, key: Expr, value: T) {
        match key {
            Expr::Var(id) => {
                self.vars.insert(id, value);
            }
            Expr::App(f, x) => {
                // lazily initialize to avoid unbounded recursion
                if self.apps.is_none() {
                    self.apps = Some(Box::new(ExprMap::new()));
                }

                let apps: &mut ExprMap<ExprMapRef> = self.apps.as_mut().unwrap();

                match apps.get(&f) {
                    Some(em_ref) => {
                        self.expr_maps[em_ref.0].insert(*x, value);
                    }
                    None => {
                        let em_ref: ExprMapRef = ExprMapRef(self.expr_maps.len());
                        let mut only_x: ExprMap<T> = ExprMap::new();
                        only_x.insert(*x, value);
                        self.expr_maps.push(only_x);
                        apps.insert(*f, em_ref);
                    }
                }
            }
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
