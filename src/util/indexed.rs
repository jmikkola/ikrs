use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

/// Attempts to assign references (of type usize) to unique values of type Val.
/// The idea is that you can compare two refs to tell if the values are equal.
/// This also acts as the owner for the values, which sidesteps ownership issues
/// in complicated code.
pub struct Indexed<Val> {
    values: Vec<Rc<Val>>,
    value_to_ref: HashMap<Rc<Val>, usize>,
}

impl<Val: Eq + Hash + Clone> Indexed<Val> {
    pub fn new() -> Self {
        Indexed {
            values: Vec::new(),
            value_to_ref: HashMap::new(),
        }
    }

    pub fn get(&self, reference: usize) -> &Val {
        &*self.values[reference]
    }

    pub fn store(&mut self, value: Val) -> usize {
        let rc_val = Rc::new(value);
        if let Some(reference) = self.value_to_ref.get(&rc_val) {
            return *reference;
        }

        let next_ref = self.values.len();
        self.values.push(rc_val.clone());
        self.value_to_ref.insert(rc_val, next_ref);

        next_ref
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_storing_values() {
        let mut indexed = Indexed::new();
        assert!(0 == indexed.store("foo".to_string()));
        assert!(0 == indexed.store("foo".to_string()));
        assert!("foo" == *indexed.get(0));

        assert!(1 == indexed.store("baz".to_string()));
        assert!(2 == indexed.store("zap".to_string()));
        assert!(3 == indexed.store("asdf".to_string()));
        assert!(1 == indexed.store("baz".to_string()));
        assert!(2 == indexed.store("zap".to_string()));
        assert!(3 == indexed.store("asdf".to_string()));
    }
}
