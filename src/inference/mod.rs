// inference package

use std::collections::HashMap;

#[cfg(test)]
mod test;

struct Inference {
    kinds: Vec<Kind>,
    types: Vec<Type>,
    strings: Vec<String>,

    kind_to_ref: HashMap<Kind, KindRef>,
    type_to_ref: HashMap<Type, TypeRef>,
    string_to_ref: HashMap<String, SRef>,
}


#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct KindRef(usize);

// Distinct from ast::TypeRef
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeRef(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SRef(usize);

impl Inference {
    fn new() -> Self {
        let mut kinds = Vec::new();
        let mut kind_to_ref = HashMap::new();
        // Reserve the zeroth ref for Star
        kinds.push(Kind::Star);
        kind_to_ref.insert(Kind::Star, KindRef(0));


        Inference{
            kinds: kinds,
            types: Vec::new(),
            strings: Vec::new(),
            kind_to_ref: kind_to_ref,
            type_to_ref: HashMap::new(),
            string_to_ref: HashMap::new(),
        }
    }

    pub fn star_kind(&self) -> KindRef {
        debug_assert!(self.kind_to_ref.contains_key(&Kind::Star));
        KindRef(0)
    }

    pub fn apply_kinds(&mut self, left: KindRef, right: KindRef) -> KindRef {
        // `left` and `right` *should* already exist as defined kinds because
        // they should be values returned by either this method or by
        // `star_kind()`.
        let resulting_kind = Kind::App(left, right);
        // Which means that if this kind is already defined, it will appear in
        // this map with the exact same left and right references
        if let Some(kref) = self.kind_to_ref.get(&resulting_kind) {
            return *kref;
        }

        let next_ref = KindRef(self.kinds.len());
        self.kinds.push(resulting_kind.clone());
        self.kind_to_ref.insert(resulting_kind, next_ref);

        next_ref
    }

    // TODO: This structure could be extracted to a shared type (likely useful in parsing, too)
    pub fn save_string(&mut self, s: &String) -> SRef {
        if let Some(sref) = self.string_to_ref.get(s) {
            return *sref;
        }

        let next_ref = SRef(self.strings.len());
        self.strings.push(s.clone());
        self.string_to_ref.insert(s.clone(), next_ref);

        next_ref
    }

    pub fn save_type(&mut self, t: Type) -> TypeRef {
        if let Some(tref) = self.type_to_ref.get(&t) {
            return *tref;
        }

        let next_ref = TypeRef(self.types.len());
        self.types.push(t.clone());
        self.type_to_ref.insert(t, next_ref);

        next_ref
    }

    pub fn get_kind(&self, KindRef(r): KindRef) -> &Kind {
        &self.kinds[r]
    }

    pub fn get_type(&self, TypeRef(r): TypeRef) -> &Type {
        &self.types[r]
    }

    pub fn get_string(&self, TypeRef(r): TypeRef) -> &String {
        &self.strings[r]
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
            },
            Var(_, k) => k,
            Gen(_, k) => k,
        }
    }
}
