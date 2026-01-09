use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::typ::Type;

#[derive(Default)]
pub struct CustomTypes {
    variant_to_constructors: HashMap<String, Vec<String>>,
    constructor_to_variant: HashMap<String, String>,
    constructor_to_argument: HashMap<String, Option<Rc<RefCell<Type>>>>,
}

pub struct Variant {
    name: String,
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
            self.constructor_to_argument.insert(c.name, c.argument);
        });
    }

    pub fn get_constructor_typ(&self, name: &str) -> Option<Rc<RefCell<Type>>> {
        self.constructor_to_variant
            .get(name)
            .map(|variant_name| Rc::new(RefCell::new(Type::Custom(variant_name.to_string()))))
    }

    pub fn get_constructor_arg(&self, name: &str) -> Option<Rc<RefCell<Type>>> {
        self.constructor_to_argument
            .get(name)
            .expect("Only call this method after verifying constructor exists")
            .clone()
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
}

impl Variant {
    pub fn new(name: String, constructors: Vec<Constructor>) -> Self {
        Self { name, constructors }
    }
}

impl Constructor {
    pub fn new(name: String, argument: Rc<RefCell<Type>>) -> Self {
        Self {
            name,
            argument: Some(argument),
        }
    }

    pub fn no_arg(name: String) -> Self {
        Self {
            name,
            argument: None,
        }
    }
}
