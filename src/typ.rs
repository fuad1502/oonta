use std::cell::RefCell;
use std::fmt::Formatter;
use std::rc::Rc;

pub mod custom_types;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Fun(Vec<Rc<RefCell<Type>>>),
    Tuple(Vec<Rc<RefCell<Type>>>),
    Custom(String, Vec<Rc<RefCell<Type>>>),
    Primitive(Primitive),
    Variable(Variable),
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Primitive {
    Integer,
    Bool,
    Unit,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Variable {
    Unbound(usize),
    Link(Rc<RefCell<Type>>),
}

impl Type {
    pub fn poly_arg_str(&self) -> String {
        self.to_string()
            .replace(" ", "")
            .replace("(", "$l")
            .replace(")", "$r")
            .replace("*", "$s")
            .replace("->", "$a")
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Type::Fun(typs) => {
                write!(fmt, "(")?;
                for (i, typ) in typs.iter().enumerate() {
                    if i == typs.len() - 1 {
                        write!(fmt, "{}", typ.borrow())?;
                    } else {
                        write!(fmt, "{} -> ", typ.borrow())?;
                    }
                }
                write!(fmt, ")")
            }
            Type::Tuple(typs) => {
                write!(fmt, "(")?;
                for (i, typ) in typs.iter().enumerate() {
                    if i == typs.len() - 1 {
                        write!(fmt, "{}", typ.borrow())?;
                    } else {
                        write!(fmt, "{} * ", typ.borrow())?;
                    }
                }
                write!(fmt, ")")
            }
            Type::Variable(Variable::Unbound(var)) => {
                write!(
                    fmt,
                    "\'{}",
                    char::from_u32(*var as u32 + 'a' as u32).unwrap()
                )
            }
            Type::Primitive(primitive) => match primitive {
                Primitive::Integer => write!(fmt, "int"),
                Primitive::Bool => write!(fmt, "bool"),
                Primitive::Unit => write!(fmt, "unit"),
            },
            Type::Custom(name, typs) => {
                if typs.len() > 1 {
                    write!(fmt, "(")?;
                    for (i, typ) in typs.iter().enumerate() {
                        if i == typs.len() - 1 {
                            write!(fmt, "{}", typ.borrow())?;
                        } else {
                            write!(fmt, "{}, ", typ.borrow())?;
                        }
                    }
                    write!(fmt, ") ")?;
                } else if typs.len() == 1 {
                    write!(fmt, "{} ", typs[0].borrow())?;
                }
                write!(fmt, "{name}")
            }
            Type::Variable(Variable::Link(typ)) => write!(fmt, "{}", typ.borrow()),
        }
    }
}

pub fn bind(from: Rc<RefCell<Type>>, to: Rc<RefCell<Type>>) {
    *from.borrow_mut() = Type::Variable(Variable::Link(to));
}

pub fn gather_unbounds(typ: Rc<RefCell<Type>>) -> Vec<Rc<RefCell<Type>>> {
    match &*typ.borrow() {
        Type::Fun(typs) | Type::Tuple(typs) | Type::Custom(_, typs) => {
            let mut unbounds = vec![];
            for typ in typs {
                unbounds.append(&mut gather_unbounds(typ.clone()));
            }
            unbounds
        }
        Type::Variable(Variable::Unbound(_)) => vec![typ.clone()],
        Type::Variable(Variable::Link(to)) => gather_unbounds(to.clone()),
        Type::Primitive(_) => vec![],
    }
}

pub fn normalize_typ(typ: Rc<RefCell<Type>>) -> Type {
    let typ = typ.borrow().clone();
    match typ {
        Type::Variable(Variable::Link(typ)) => normalize_typ(typ),
        _ => typ,
    }
}

pub fn extract_fun_typs(typ: Rc<RefCell<Type>>) -> Option<Vec<Rc<RefCell<Type>>>> {
    if let Type::Fun(typs) = normalize_typ(typ) {
        Some(typs.clone())
    } else {
        None
    }
}

pub fn extract_tuple_typs(typ: Rc<RefCell<Type>>) -> Option<Vec<Rc<RefCell<Type>>>> {
    if let Type::Tuple(typs) = normalize_typ(typ) {
        Some(typs.clone())
    } else {
        None
    }
}

pub fn extract_variant_args(typ: Rc<RefCell<Type>>) -> Option<Vec<Rc<RefCell<Type>>>> {
    if let Type::Custom(_, args) = normalize_typ(typ) {
        Some(args.clone())
    } else {
        None
    }
}

pub fn is_polymorphic(typ: Rc<RefCell<Type>>) -> bool {
    match &*typ.borrow() {
        Type::Fun(typs) | Type::Tuple(typs) | Type::Custom(_, typs) => {
            typs.iter().cloned().any(is_polymorphic)
        }
        Type::Variable(Variable::Link(typ)) => is_polymorphic(typ.clone()),
        Type::Variable(Variable::Unbound(_)) => true,
        Type::Primitive(_) => false,
    }
}

pub fn link_unbounds(typ: Rc<RefCell<Type>>, to_typs: &[Rc<RefCell<Type>>]) {
    let unbounds_from_typ = gather_unbounds(typ);
    for unbound in unbounds_from_typ {
        bind(unbound.clone(), to_typs[unbound_id(&unbound)].clone());
    }
}

pub fn does_returns_fun(typ: Rc<RefCell<Type>>) -> bool {
    if let Type::Fun(typs) = &*typ.borrow() {
        let ret_typ = typs.last().unwrap();
        matches!(&*ret_typ.borrow(), Type::Fun(_))
    } else {
        false
    }
}

fn unbound_id(typ: &Rc<RefCell<Type>>) -> usize {
    if let Type::Variable(Variable::Unbound(id)) = &*typ.borrow() {
        *id
    } else {
        panic!("Only use this function on unbound types")
    }
}
