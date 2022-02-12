// inference package

use std::collections::HashMap;
use std::collections::HashSet;

use anyhow::{bail, Result};
use itertools::Itertools;

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

    fn most_general_unifier(&mut self, a: TypeRef, b: TypeRef) -> Result<Substitution> {
        use Type::*;

        let a_type = self.get_type(a).clone();
        let b_type = self.get_type(b).clone();
        match (a_type, b_type) {
            (Func(n1, k1), Func(n2, k2)) =>
                self.mismatch_unless(a, b, n1 == n2 && k1 == k2),

            (Con(t1, k1), Con(t2, k2)) =>
                self.mismatch_unless(a, b, t1 == t2 && k1 == k2),

            (App(l1, r1), App(l2, r2)) => {
                let sub1 = self.most_general_unifier(l1, l2)?;
                let r1_subbed = r1.apply(&sub1, self);
                let r2_subbed = r2.apply(&sub1, self);
                let sub2 = self.most_general_unifier(r1_subbed, r2_subbed)?;
                Ok(sub1.compose(&sub2, self))
            },

            (Gen(_, _), _) =>
                panic!("a generic variable shouldn't be passed to most_general_unifier"),
            (_, Gen(_, _)) =>
                panic!("a generic variable shouldn't be passed to most_general_unifier"),

            (Var(v, k), _) =>
                self.bind_variable(v, k, b),
            (_, Var(v, k)) =>
                self.bind_variable(v, k, a),

            _ =>
                self.mismatch_error(a, b),
	}
    }

    fn bind_variable(&mut self, name: SRef, kind: KindRef, other: TypeRef) -> Result<Substitution> {
	let typ = Type::Var(name, kind);
	if *self.get_type(other) == typ {
	    return Ok(Substitution::empty());
	}

	if self.free_type_variables(other).contains(&TypeVar{name, kind}) {
	    bail!("infinite type");
	}

	if kind != other.kind(self) {
	    bail!("kind mismatch");
	}

	let typ_ref = self.save_type(typ);
	Ok(Substitution::singleton(typ_ref, other))
    }

    fn free_type_variables(&mut self, typ: TypeRef) -> HashSet<TypeVar> {
	let mut out = HashSet::new();
	typ.free_type_vars(self, &mut out);
	out
    }

    fn mismatch_unless(&self, a: TypeRef, b: TypeRef, condition: bool) -> Result<Substitution> {
	if condition {
	    Ok(Substitution::empty())
	} else {
	    self.mismatch_error(a, b)
	}
    }

    fn mismatch_error(&self, a: TypeRef, b: TypeRef) -> Result<Substitution> {
	bail!("mismatch between types {:?} and {:?}", self.get_type(a), self.get_type(b))
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
#[derive(Clone)]
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

    fn in_head_normal_form(&self, inference: &Inference) -> bool {
	self.typ.in_head_normal_form(inference)
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

    fn in_head_normal_form(&self, inference: &Inference) -> bool {
        use Type::*;

        match inference.get_type(*self).clone() {
	    Var(_, _) => true,
	    App(l, _) => l.in_head_normal_form(inference),
	    _ => false,
	}
    }
}

#[derive(Clone)]
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
    // Converts predicates to head normal form and simplifies them
    fn reduce(&self, predicates: &[Predicate], inference: &mut Inference) -> Result<Vec<Predicate>> {
        let preds_in_hnf = self.preds_to_head_normal_form(predicates, inference)?;
        Ok(self.simplify_predicates(&preds_in_hnf, inference))
    }

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

    /// Look through the instances of class `class` and see if any match the type `typ`.
    /// (There shouldn't be more that one match)
    /// If one matches, return that instance's predicates (after tweaking them so that the types
    /// match up).
    fn find_matching_instance(&self, class: SRef, typ: TypeRef, inference: &mut Inference)
                              -> Option<Vec<Predicate>> {
        let p = Predicate{class, typ};

        let instances = &self.classes.get(&class).unwrap().instances;
        instances.iter().find_map(|inst| {
            // if the instance's type matches the passed-in type, the result of matches will be
            // Some(Substitution)
            inst.t.matches(&p, inference)
                // when the type matches, take that substitution and apply it to the predicates,
                // then return those predicates
		.map(|sub| inst.predicates.apply(&sub, inference))
	})
    }

    // Assuming all of `given` hold, does that imply that `p` also holds?
    fn entail(&self, given: &[Predicate], p: &Predicate, inference: &mut Inference) -> bool {
        // If p is a superclass of any of the classes in the given predicates, then you know the
        // type must also implement p.
        // Example: if p is (Eq a) and given predicates are (Ord a, Show a), you know that p must
        // also be available since it's a superclass of Ord.
        let is_super_class = given.iter().any(|given| self.is_super_class(p.class, given.class));
        if is_super_class {
            return true;
        }

        // Check if p is given by some instance whose predicates all hold.
        if let Some(instance_predicates) = self.find_matching_instance(p.class, p.typ, inference) {
            // if that instance has any predicates of its own (e.g. (Show a) => Show [a]), ensure
            // that those predicates are all entailed.
            return instance_predicates.iter()
                .all(|inst_pred| self.entail(given, inst_pred, inference));
        }

        // those are the only two ways the predicate could be entailed
        false
    }

    // remove redundant predicates, e.g. [Eq a, Ord a, Ord a] --> [Ord a]
    fn simplify_predicates(&self, predicates: &[Predicate], inference: &mut Inference) -> Vec<Predicate> {
	let mut source: Vec<Predicate> = predicates.to_owned();
	let mut keep = Vec::new();

	while let Some(p) = source.pop() {
	    let given = keep.iter().chain(source.iter()).cloned().collect_vec();
	    if !self.entail(&given, &p, inference) {
		keep.push(p);
	    }
	}

	keep
    }

    fn pred_to_head_normal_form(&self, predicate: &Predicate, inference: &mut Inference)
				-> Result<Vec<Predicate>> {
	// If it's already in HNF, there's no more work to do
	if predicate.in_head_normal_form(inference) {
	    return Ok(vec![predicate.clone()]);
	}

	// Otherwise, there needs to be an instance for the concrete type.
	match self.find_matching_instance(predicate.class, predicate.typ, inference) {
	    Some(new_predicates) =>
		self.preds_to_head_normal_form(&new_predicates, inference),
	    None =>
		bail!("context reduction failure"),
	}
    }

    fn preds_to_head_normal_form(&self, predicates: &[Predicate], inference: &mut Inference)
				 -> Result<Vec<Predicate>> {
	let mut results = Vec::new();
	for predicate in predicates.iter() {
	    let new_predicates = self.pred_to_head_normal_form(predicate, inference)?;
	    results.extend(new_predicates.into_iter());
	}
	Ok(results)
    }
}

trait HasKind {
    fn kind(&self, inference: &Inference) -> KindRef;
}

impl HasKind for TypeRef {
    fn kind(&self, inference: &Inference) -> KindRef {
	inference.get_type(*self).kind(inference)
    }
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

impl Types for Predicate {
    fn apply(&self, sub: &Substitution, inference: &mut Inference) -> Self {
	Predicate{
	    class: self.class,
	    typ: self.typ.apply(sub, inference),
	}
    }

    fn free_type_vars(&self, inference: &mut Inference, out: &mut HashSet<TypeVar>) {
	self.typ.free_type_vars(inference, out);
    }
}

impl<T> Types for Vec<T> where T: Types {
    fn apply(&self, sub: &Substitution, inference: &mut Inference) -> Self {
	self.iter().map(|t| t.apply(sub, inference)).collect()
    }

    fn free_type_vars(&self, inference: &mut Inference, out: &mut HashSet<TypeVar>) {
	for t in self.iter() {
	    t.free_type_vars(inference, out);
	}
    }
}
