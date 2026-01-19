use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::typ::{Type, Variable, gather_unbounds};

#[derive(Default)]
pub struct CustomTypes {
    variant_to_constructors: HashMap<String, Vec<String>>,
    variant_to_num_of_params: HashMap<String, usize>,
    constructor_to_variant: HashMap<String, String>,
    constructor_to_argument: HashMap<String, Option<Rc<RefCell<Type>>>>,
}

pub struct Variant {
    name: String,
    params: Vec<usize>,
    constructors: Vec<Constructor>,
}

pub struct Constructor {
    name: String,
    argument: Option<Rc<RefCell<Type>>>,
}

impl CustomTypes {
    pub fn add_variant(&mut self, variant: Variant) {
        if self.variant_to_constructors.contains_key(&variant.name) {
            panic!("Cannot re-define type")
        }
        self.variant_to_num_of_params
            .insert(variant.name.clone(), variant.params.len());
        let constructor_names = variant
            .constructors
            .iter()
            .map(|c| c.name.clone())
            .collect();
        self.variant_to_constructors
            .insert(variant.name.clone(), constructor_names);
        variant.constructors.into_iter().for_each(|c| {
            if self.constructor_to_variant.contains_key(&c.name) {
                panic!("Cannot re-use constructor name")
            }
            self.constructor_to_variant
                .insert(c.name.clone(), variant.name.clone());
            if let Some(arg) = &c.argument {
                Self::rename_unbounds_in_arg_to_variant_param_positions(
                    arg.clone(),
                    &variant.params,
                );
            }
            self.constructor_to_argument.insert(c.name, c.argument);
        });
    }

    pub fn get_constructors(&self, name: &str) -> &Vec<String> {
        &self.variant_to_constructors[name]
    }

    pub fn get_constructor_typ(&self, name: &str) -> Option<Rc<RefCell<Type>>> {
        self.constructor_to_variant.get(name).map(|variant_name| {
            let args = self.create_args(variant_name);
            Rc::new(RefCell::new(Type::Custom(variant_name.clone(), args)))
        })
    }

    pub fn get_constructor_arg(&self, name: &str) -> Option<Rc<RefCell<Type>>> {
        self.constructor_to_argument
            .get(name)
            .expect("Only call this method after verifying constructor exists")
            .clone()
            .map(instantiate_typ)
    }

    pub fn get_constructor_idx(&self, name: &str) -> usize {
        self.constructor_to_variant
            .get(name)
            .map(|variant_name| {
                self.variant_to_constructors[variant_name]
                    .iter()
                    .position(|c| c == name)
                    .unwrap()
            })
            .expect("Only call this method after verifying constructor exists")
    }

    fn create_args(&self, variant_name: &str) -> Vec<Rc<RefCell<Type>>> {
        (0..self.variant_to_num_of_params[variant_name])
            .map(|i| Rc::new(RefCell::new(Type::Variable(Variable::Unbound(i)))))
            .collect()
    }

    fn rename_unbounds_in_arg_to_variant_param_positions(arg: Rc<RefCell<Type>>, params: &[usize]) {
        let unbounds = gather_unbounds(arg.clone());
        for unbound in unbounds {
            if let Type::Variable(Variable::Unbound(var)) = &mut *unbound.borrow_mut() {
                *var = params
                    .iter()
                    .position(|p| p == var)
                    .expect("Unbound not in parameter");
            }
        }
    }
}

impl Variant {
    pub fn new(name: String, params: Vec<usize>, constructors: Vec<Constructor>) -> Self {
        Self {
            name,
            params,
            constructors,
        }
    }
}

impl Constructor {
    pub fn new(name: String, argument: Option<Rc<RefCell<Type>>>) -> Self {
        Self { name, argument }
    }
}

fn instantiate_typ(typ: Rc<RefCell<Type>>) -> Rc<RefCell<Type>> {
    match &*typ.borrow() {
        Type::Primitive(_) => typ.clone(),
        Type::Variable(Variable::Link(typ)) => instantiate_typ(typ.clone()),
        Type::Fun(typs) => {
            let typs = typs.iter().map(|t| instantiate_typ(t.clone())).collect();
            Rc::new(RefCell::new(Type::Fun(typs)))
        }
        Type::Tuple(typs) => {
            let typs = typs.iter().map(|t| instantiate_typ(t.clone())).collect();
            Rc::new(RefCell::new(Type::Tuple(typs)))
        }
        Type::Custom(name, args) => {
            let args = args.iter().map(|t| instantiate_typ(t.clone())).collect();
            Rc::new(RefCell::new(Type::Custom(name.clone(), args)))
        }
        Type::Variable(Variable::Unbound(i)) => {
            Rc::new(RefCell::new(Type::Variable(Variable::Unbound(*i))))
        }
    }
}
