// inference package

use std::collections::HashMap;
use std::collections::HashSet;

use super::util::indexed::Indexed;

#[cfg(test)]
mod test;

struct Inference {
    kinds: Indexed<Kind>,
    types: Indexed<Type>,
    strings: Indexed<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct KindRef(usize);

/// Distinct from ast::TypeRef
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeRef(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SRef(usize);

impl Inference {
    fn new() -> Self {
        let mut kinds = Indexed::new();
        // Reserve the zeroth ref for Star
        let r = kinds.store(Kind::Star);
        debug_assert!(r == 0);

        Inference {
            kinds,
            types: Indexed::new(),
            strings: Indexed::new(),
        }
    }

    pub fn star_kind(&self) -> KindRef {
        debug_assert!(self.kinds.get(0) == &Kind::Star);
        KindRef(0)
    }

    pub fn apply_kinds(&mut self, left: KindRef, right: KindRef) -> KindRef {
        // `left` and `right` *should* already exist as defined kinds because
        // they should be values returned by either this method or by
        // `star_kind()`.
        let resulting_kind = Kind::App(left, right);
        KindRef(self.kinds.store(resulting_kind))
    }

    pub fn get_kind(&self, KindRef(r): KindRef) -> &Kind {
        self.kinds.get(r)
    }

    pub fn save_string(&mut self, s: String) -> SRef {
        SRef(self.strings.store(s))
    }

    pub fn get_string(&self, TypeRef(r): TypeRef) -> &str {
        self.strings.get(r).as_ref()
    }

    pub fn save_type(&mut self, t: Type) -> TypeRef {
        TypeRef(self.types.store(t))
    }

    pub fn get_type(&self, TypeRef(r): TypeRef) -> &Type {
        self.types.get(r)
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
enum Kind {
    Star,
    App(KindRef, KindRef),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Type {
    Con(SRef, KindRef),
    Func(u32, KindRef),
    App(TypeRef, TypeRef),
    Var(SRef, KindRef),
    Gen(u32, KindRef),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TypeVar {
    name: SRef,
    kind: KindRef,
}

#[derive(Debug, PartialEq)]
struct Substitution {
    substitutions: HashMap<TypeRef, TypeRef>,
}

impl Substitution {
    fn compose(&self, other: &Substitution, inference: &mut Inference) -> Substitution {
	let mut substitutions = HashMap::<TypeRef, TypeRef>::new();

	for (key, val) in self.substitutions.iter() {
	    substitutions.insert(*key, val.apply(other, inference));
	}
	for (key, val) in other.substitutions.iter() {
	    substitutions.insert(*key, *val);
	}

	Substitution{substitutions}
    }

    fn merge(&self, other: &Substitution) -> Option<Substitution> {
	// Do the two substitutions agree on all points they share in common?
	for (key, val1) in self.substitutions.iter() {
	    if let Some(val2) = other.substitutions.get(key) {
		if *val1 != *val2 {
		    return None
		}
	    }
	}

	// Since the results agree, they can be safely merged
	let mut result = Substitution::empty();
	for (key, val) in self.substitutions.iter() {
	    result.add(*key, *val);
	}
	for (key, val) in other.substitutions.iter() {
	    result.add(*key, *val);
	}
	Some(result)
    }

    fn empty() -> Self {
	Substitution{substitutions: HashMap::new()}
    }

    fn singleton(original: TypeRef, replacement: TypeRef) -> Self {
	let mut sub = Substitution::empty();
	sub.add(original, replacement);
	sub
    }

    fn add(&mut self, original: TypeRef, replacement: TypeRef) {
	self.substitutions.insert(original, replacement);
    }
}

// e.g. (Num a) -- class="Num", type="a"
struct Predicate {
    class: SRef,
    typ: TypeRef,
}

impl Predicate {
    fn matches(&self, other: &Predicate, inference: &Inference) -> Option<Substitution> {
	if self.class == other.class {
	    self.typ.matches(other.typ, inference)
	} else {
	    None
	}
    }
}

impl TypeRef {
    fn matches(&self, other: TypeRef, inference: &Inference) -> Option<Substitution> {
	use Type::*;

	if *self == other {
	    return Some(Substitution::empty())
	}

	let other_type = inference.get_type(other).clone();

	match inference.get_type(*self).clone() {
	    App(left1, right1) => match other_type {
		App(left2, right2) => {
		    let sub1 = left1.matches(left2, inference)?;
		    let sub2 = right1.matches(right2, inference)?;
		    sub1.merge(&sub2)
		},
		_ => None,
	    },

	    Con(c1, k1) => match other_type {
		Con(c2, k2) if c1 == c2 && k1 == k2 => {
		    Some(Substitution::empty())
		},
		_ => None,
	    },

	    Func(n1, k1) => match other_type {
		Func(n2, k2) if n1 == n2 && k1 == k2 => {
		    Some(Substitution::empty())
		},
		_ => None,
	    },

	    Var(_, _) => Some(Substitution::singleton(*self, other)),

	    Gen(_, _) => panic!("compiler bug: LHS of match is a generic"),
	}
    }
}

struct Qualified<T> {
    predicates: Vec<Predicate>,
    t: T,
}

type QualType = Qualified<Type>;
type Instance = Qualified<Predicate>;

struct Scheme {
    kinds: Vec<KindRef>,
    typ: QualType,
}

struct Class {
    superclasses: Vec<SRef>,
    instances: Vec<Instance>,
}

struct ClassEnv {
    classes: HashMap<SRef, Class>,
    // should I add a list of default types?
}

impl ClassEnv {
    // It is an error to call this with an SRef that isn't a class.
    // This returns true if parent == child.
    fn is_super_class(&self, parent: SRef, child: SRef) -> bool {
	if parent == child {
	    return true;
	}

	let child_class = self.classes.get(&child).unwrap();
	child_class.superclasses.iter()
	    .any(|sup| self.is_super_class(parent, *sup))
    }
}

trait HasKind {
    fn kind(&self, inference: &Inference) -> KindRef;
}

impl HasKind for Type {
    fn kind(&self, inference: &Inference) -> KindRef {
        use Type::*;
        match *self {
            Con(_, k) => k,
            Func(_, k) => k,
            App(l, _r) => {
                let left_type = inference.get_type(l);
                let left_kind = inference.get_kind(left_type.kind(inference));
                match *left_kind {
                    Kind::Star => panic!("compiler bug: LHS of type application has kind Star"),
                    Kind::App(_, r) => r,
                }
            }
            Var(_, k) => k,
            Gen(_, k) => k,
        }
    }
}

impl HasKind for TypeVar {
    fn kind(&self, _: &Inference) -> KindRef {
	self.kind
    }
}

trait Types {
    fn apply(&self, sub: &Substitution, inference: &mut Inference) -> Self;

    fn free_type_vars(&self, inference: &mut Inference, out: &mut HashSet<TypeVar>);
}

impl Types for TypeRef {
    fn apply(&self, sub: &Substitution, inference: &mut Inference) -> Self {
	use Type::*;
	if let Some(tref) = sub.substitutions.get(self) {
	    return *tref;
	}

	match inference.get_type(*self).clone() {
	    App(left, right) => {
		let result = App(left.apply(sub, inference), right.apply(sub, inference));
		inference.save_type(result)
	    },
	    Con(_, _) => *self,
	    Func(_, _) => *self,
	    Var(_, _) => *self,
	    Gen(_, _) => *self,
	}
    }

    fn free_type_vars(&self, inference: &mut Inference, out: &mut HashSet<TypeVar>) {
	use Type::*;
	match inference.get_type(*self).clone() {
	    Con(_, _) => {},
	    Func(_, _) => {},
	    App(left, right) => {
		left.free_type_vars(inference, out);
		right.free_type_vars(inference, out);
	    },
	    Var(name, kind) => {
		out.insert(TypeVar{name, kind});
	    },
	    Gen(_, _) => {},
	}
    }
}
