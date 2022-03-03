// inference package

use std::collections::HashMap;
use std::collections::HashSet;

use anyhow::{bail, Result};
use itertools::Itertools;

use crate::parser::ast;

use super::package::{ParsedPackage, TypedPackage};

#[cfg(test)]
mod test;

pub struct KnownTypes {
    // TODO
}

impl KnownTypes {
    pub fn new() -> Self {
	KnownTypes{}
    }
}

#[allow(unused_variables)]
pub fn infer_package_group(
    group: &[ParsedPackage],
    known_types: &mut KnownTypes,
) -> Result<Vec<TypedPackage>> {
    Ok(vec![]) // TODO
}

trait InferType {
    fn infer_type(&self, inference: &mut Inference, environment: &Environment) -> Result<InferResult>;
}

impl InferType for ast::Literal {
    fn infer_type(&self, inference: &mut Inference, _environment: &Environment) -> Result<InferResult> {
	use ast::Literal::*;
	let result = match self {
	    Integer(_) => {
		let type_var = inference.new_type_variable(Kind::Star);
		let t = type_var.to_type();
		let num_pred = Predicate{
		    class: "Num".into(),
		    typ: t.clone(),
		};
		let preds = vec![num_pred];
		InferResult{t, preds}
	    },
	    Float(_) => {
		let preds = vec![];
		let t = Type::Con("Float".into(), Kind::Star);
		InferResult{t, preds}
	    },
	    String(_) => {
		let preds = vec![];
		let t = Type::Con("String".into(), Kind::Star);
		InferResult{t, preds}
	    },
	};
	Ok(result)
    }
}

struct InferResult {
    t: Type,
    preds: Vec<Predicate>,
}

struct Environment {
    // TODO: Bindings
}

struct Inference<'a> {
    syntax: &'a ast::Syntax,
    n_type_variables_used: u64,
}

impl<'a> Inference<'a> {
    fn new(syntax: &'a ast::Syntax) -> Self {
        Inference {
	    syntax,
	    n_type_variables_used: 0,
        }
    }

    fn new_type_variable(&mut self, kind: Kind) -> TypeVar {
	self.n_type_variables_used += 1;
	let name = format!("tv_{}", self.n_type_variables_used);
	TypeVar{name, kind}
    }

    fn most_general_unifier(&mut self, a: &Type, b: &Type) -> Result<Substitution> {
        use Type::*;

        match (a, b) {
            (Func(n1, k1), Func(n2, k2)) =>
                self.mismatch_unless(a, b, n1 == n2 && k1 == k2),

            (Con(t1, k1), Con(t2, k2)) =>
                self.mismatch_unless(a, b, t1 == t2 && k1 == k2),

            (App(l1, r1), App(l2, r2)) => {
                let sub1 = self.most_general_unifier(l1, l2)?;
                let r1_subbed = r1.apply(&sub1);
                let r2_subbed = r2.apply(&sub1);
                let sub2 = self.most_general_unifier(&r1_subbed, &r2_subbed)?;
                Ok(sub1.compose(&sub2))
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

    fn bind_variable(&mut self, name: &str, kind: &Kind, other: &Type) -> Result<Substitution> {
	let typ = Type::Var(name.to_owned(), kind.clone());
	if other == &typ {
	    return Ok(Substitution::empty());
	}

	let this_type = TypeVar{
	    name: name.to_owned(),
	    kind: kind.clone(),
	};
	if self.free_type_variables(other).contains(&this_type) {
	    bail!("infinite type");
	}

	if kind != &other.kind() {
	    bail!("kind mismatch");
	}

	Ok(Substitution::singleton(&typ, other))
    }

    fn free_type_variables(&mut self, typ: &Type) -> HashSet<TypeVar> {
	let mut out = HashSet::new();
	typ.free_type_vars(&mut out);
	out
    }

    fn mismatch_unless(&self, a: &Type, b: &Type, condition: bool) -> Result<Substitution> {
	if condition {
	    Ok(Substitution::empty())
	} else {
	    self.mismatch_error(a, b)
	}
    }

    fn mismatch_error(&self, a: &Type, b: &Type) -> Result<Substitution> {
	bail!("mismatch between types {:?} and {:?}", a, b);
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
enum Kind {
    Star,
    App(Box<Kind>, Box<Kind>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Type {
    Con(String, Kind),
    Func(u32, Kind),
    App(Box<Type>, Box<Type>),
    Var(String, Kind),
    Gen(u32, Kind),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TypeVar {
    name: String,
    kind: Kind,
}

impl TypeVar {
    fn to_type(&self) -> Type {
	Type::Var(self.name.clone(), self.kind.clone())
    }
}

#[derive(Debug, PartialEq)]
struct Substitution {
    substitutions: HashMap<Type, Type>,
}

impl Substitution {
    fn compose(&self, other: &Substitution) -> Substitution {
        let mut substitutions = HashMap::<Type, Type>::new();

        for (key, val) in self.substitutions.iter() {
            substitutions.insert(key.clone(), val.apply(other));
        }
        for (key, val) in other.substitutions.iter() {
            substitutions.insert(key.clone(), val.clone());
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
            result.add(key, val);
        }
        for (key, val) in other.substitutions.iter() {
            result.add(key, val);
        }
        Some(result)
    }

    fn empty() -> Self {
        Substitution{substitutions: HashMap::new()}
    }

    fn singleton(original: &Type, replacement: &Type) -> Self {
        let mut sub = Substitution::empty();
        sub.add(original, replacement);
        sub
    }

    fn add(&mut self, original: &Type, replacement: &Type) {
        self.substitutions.insert(original.clone(), replacement.clone());
    }
}

// e.g. (Num a) -- class="Num", type="a"
#[derive(Clone)]
struct Predicate {
    class: String,
    typ: Type,
}

impl Predicate {
    fn matches(&self, other: &Predicate) -> Option<Substitution> {
        if self.class == other.class {
            self.typ.matches(&other.typ)
        } else {
            None
        }
    }

    fn in_head_normal_form(&self) -> bool {
	self.typ.in_head_normal_form()
    }
}

impl Type {
    fn matches(&self, other: &Type) -> Option<Substitution> {
        use Type::*;

        if self == other {
            return Some(Substitution::empty())
        }

        match self {
            App(left1, right1) => match other {
                App(left2, right2) => {
                    let sub1 = left1.matches(left2)?;
                    let sub2 = right1.matches(right2)?;
                    sub1.merge(&sub2)
                },
                _ => None,
            },

            Con(c1, k1) => match other {
                Con(c2, k2) if c1 == c2 && k1 == k2 => {
                    Some(Substitution::empty())
                },
                _ => None,
            },

            Func(n1, k1) => match other {
                Func(n2, k2) if n1 == n2 && k1 == k2 => {
                    Some(Substitution::empty())
                },
                _ => None,
            },

            Var(_, _) => Some(Substitution::singleton(self, other)),

            Gen(_, _) => panic!("compiler bug: LHS of match is a generic"),
        }
    }

    fn in_head_normal_form(&self) -> bool {
        use Type::*;

        match self {
	    Var(_, _) => true,
	    App(l, _) => l.in_head_normal_form(),
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
    kinds: Vec<Kind>,
    typ: QualType,
}

struct Class {
    superclasses: Vec<String>,
    instances: Vec<Instance>,
}

struct ClassEnv {
    classes: HashMap<String, Class>,
    // should I add a list of default types?
}

impl ClassEnv {
    // Converts predicates to head normal form and simplifies them
    fn reduce(&self, predicates: &[Predicate]) -> Result<Vec<Predicate>> {
        let preds_in_hnf = self.preds_to_head_normal_form(predicates)?;
        Ok(self.simplify_predicates(&preds_in_hnf))
    }

    // It is an error to call this with an name that isn't a class.
    // This returns true if parent == child.
    fn is_super_class(&self, parent: &str, child: &str) -> bool {
        if parent == child {
            return true;
        }

        let child_class = self.classes.get(child).unwrap();
        child_class.superclasses.iter()
            .any(|sup| self.is_super_class(parent, sup))
    }

    /// Look through the instances of class `class` and see if any match the type `typ`.
    /// (There shouldn't be more that one match)
    /// If one matches, return that instance's predicates (after tweaking them so that the types
    /// match up).
    fn find_matching_instance(&self, class: &str, typ: &Type)
                              -> Option<Vec<Predicate>> {
        let p = Predicate{
	    class: class.to_owned(),
	    typ: typ.clone(),
	};

        let instances = &self.classes.get(class).unwrap().instances;
        instances.iter().find_map(|inst| {
            // if the instance's type matches the passed-in type, the result of matches will be
            // Some(Substitution)
            inst.t.matches(&p)
                // when the type matches, take that substitution and apply it to the predicates,
                // then return those predicates
		.map(|sub| inst.predicates.apply(&sub))
	})
    }

    // Assuming all of `given` hold, does that imply that `p` also holds?
    fn entail(&self, given: &[Predicate], p: &Predicate) -> bool {
        // If p is a superclass of any of the classes in the given predicates, then you know the
        // type must also implement p.
        // Example: if p is (Eq a) and given predicates are (Ord a, Show a), you know that p must
        // also be available since it's a superclass of Ord.
        let is_super_class = given.iter().any(|given| self.is_super_class(&p.class, &given.class));
        if is_super_class {
            return true;
        }

        // Check if p is given by some instance whose predicates all hold.
        if let Some(instance_predicates) = self.find_matching_instance(&p.class, &p.typ) {
            // if that instance has any predicates of its own (e.g. (Show a) => Show [a]), ensure
            // that those predicates are all entailed.
            return instance_predicates.iter()
                .all(|inst_pred| self.entail(given, inst_pred));
        }

        // those are the only two ways the predicate could be entailed
        false
    }

    // remove redundant predicates, e.g. [Eq a, Ord a, Ord a] --> [Ord a]
    fn simplify_predicates(&self, predicates: &[Predicate]) -> Vec<Predicate> {
	let mut source: Vec<Predicate> = predicates.to_owned();
	let mut keep = Vec::new();

	while let Some(p) = source.pop() {
	    let given = keep.iter().chain(source.iter()).cloned().collect_vec();
	    if !self.entail(&given, &p) {
		keep.push(p);
	    }
	}

	keep
    }

    fn pred_to_head_normal_form(&self, predicate: &Predicate)
				-> Result<Vec<Predicate>> {
	// If it's already in HNF, there's no more work to do
	if predicate.in_head_normal_form() {
	    return Ok(vec![predicate.clone()]);
	}

	// Otherwise, there needs to be an instance for the concrete type.
	match self.find_matching_instance(&predicate.class, &predicate.typ) {
	    Some(new_predicates) =>
		self.preds_to_head_normal_form(&new_predicates),
	    None =>
		bail!("context reduction failure"),
	}
    }

    fn preds_to_head_normal_form(&self, predicates: &[Predicate])
				 -> Result<Vec<Predicate>> {
	let mut results = Vec::new();
	for predicate in predicates.iter() {
	    let new_predicates = self.pred_to_head_normal_form(predicate)?;
	    results.extend(new_predicates.into_iter());
	}
	Ok(results)
    }
}

trait HasKind {
    fn kind(&self) -> Kind;
}

impl HasKind for Type {
    fn kind(&self) -> Kind {
        use Type::*;
        match self {
            Con(_, k) => k,
            Func(_, k) => k,
            App(l, _r) => {
                match l.kind() {
                    Kind::Star => panic!("compiler bug: LHS of type application has kind Star"),
                    Kind::App(_, r) => return (*r).clone(),
                }
            }
            Var(_, k) => k,
            Gen(_, k) => k,
        }.clone()
    }
}

impl HasKind for TypeVar {
    fn kind(&self) -> Kind {
        self.kind.clone()
    }
}

trait Types {
    fn apply(&self, sub: &Substitution) -> Self;

    fn free_type_vars(&self, out: &mut HashSet<TypeVar>);
}

impl Types for Type {
    fn apply(&self, sub: &Substitution) -> Self {
        use Type::*;
        if let Some(t) = sub.substitutions.get(self) {
            return t.clone();
        }

        match self {
            App(left, right) => {
                return App(Box::new(left.apply(sub)), Box::new(right.apply(sub)));
            },
            Con(_, _) => self,
            Func(_, _) => self,
            Var(_, _) => self,
            Gen(_, _) => self,
        }.clone()
    }

    fn free_type_vars(&self, out: &mut HashSet<TypeVar>) {
        use Type::*;
        match self {
            Con(_, _) => {},
            Func(_, _) => {},
            App(left, right) => {
                left.free_type_vars(out);
                right.free_type_vars(out);
            },
            Var(name, kind) => {
                out.insert(TypeVar{name: name.clone(), kind: kind.clone()});
            },
            Gen(_, _) => {},
        }
    }
}

impl Types for Predicate {
    fn apply(&self, sub: &Substitution) -> Self {
	Predicate{
	    class: self.class.clone(),
	    typ: self.typ.apply(sub),
	}
    }

    fn free_type_vars(&self, out: &mut HashSet<TypeVar>) {
	self.typ.free_type_vars(out);
    }
}

impl<T> Types for Vec<T> where T: Types {
    fn apply(&self, sub: &Substitution) -> Self {
	self.iter().map(|t| t.apply(sub)).collect()
    }

    fn free_type_vars(&self, out: &mut HashSet<TypeVar>) {
	for t in self.iter() {
	    t.free_type_vars(out);
	}
    }
}
