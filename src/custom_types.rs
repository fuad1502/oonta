use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::typ::Type;

#[derive(Default)]
pub struct CustomTypes {
    variant_to_constructors: HashMap<String, Vec<String>>,
    constructor_to_variant: HashMap<String, String>,
    constructor_to_argument: HashMap<String, Option<Rc<RefCell<Type>>>>,
}

pub struct Variant {
    constructors: Vec<Constructor>,
}

pub struct Constructor {
    name: String,
    argument: Option<Rc<RefCell<Type>>>,
}

impl CustomTypes {
    pub fn add_variant(&mut self, variant: Variant) {
        todo!()
    }
}

impl Variant {
    pub fn new(constructors: Vec<Constructor>) -> Self {
        Self { constructors }
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
