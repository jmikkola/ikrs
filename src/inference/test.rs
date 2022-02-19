use super::*;

fn simple_var(name: &str) -> Type {
    Type::Var(name.to_owned(), Kind::Star)
}

fn type_app(left: Type, right: Type) -> Type {
    Type::App(Box::new(left), Box::new(right))
}

#[test]
fn test_composing_substitutions() {
    let empty = Substitution::empty();

    assert!(empty == empty.compose(&empty));

    let a = simple_var("a");
    let b = simple_var("b");
    let a2b = Substitution::singleton(&a, &b);

    // Composing a (valid) substitution with itself doesn't change anything
    assert!(a2b == a2b.compose(&a2b));
    // Composing a substitution with the empty substitution does nothing
    assert!(a2b == a2b.compose(&empty));
    assert!(a2b == empty.compose(&a2b));

    let c = simple_var("c");
    let b2c = Substitution::singleton(&b, &c);

    let mut expected = Substitution::empty();
    expected.add(&a, &c);
    expected.add(&b, &c);

    assert!(expected == a2b.compose(&b2c));
}

#[test]
fn test_merging_subsitutions() {
    let empty = Substitution::empty();

    assert!(empty == empty.merge(&empty).unwrap());

    let a = simple_var("a");
    let b = simple_var("b");
    let a2b = Substitution::singleton(&a, &b);

    // Composing a (valid) substitution with itself doesn't change anything
    assert!(a2b == a2b.merge(&a2b).unwrap());
    // Composing a substitution with the empty substitution does nothing
    assert!(a2b == a2b.merge(&empty).unwrap());
    assert!(a2b == empty.merge(&a2b).unwrap());

    let c = simple_var("c");
    let d = simple_var("d");
    let c2d = Substitution::singleton(&c, &d);

    let mut expected = Substitution::empty();
    expected.add(&a, &b);
    expected.add(&c, &d);

    // Test that the fields from the two are added together
    assert!(expected == a2b.merge(&c2d).unwrap());
    assert!(expected == c2d.merge(&a2b).unwrap());

    let a2c = Substitution::singleton(&a, &c);

    // Test a case where the two disagree
    assert!(a2b.merge(&a2c).is_none());
}

#[test]
fn test_matching_types() {
    let a = simple_var("a");
    let b = simple_var("b");
    let c = simple_var("c");

    let t_aa = type_app(a.clone(), a.clone());
    let t_bc = type_app(b.clone(), c.clone());

    // Replacing two vars with one thing is OK
    let mut expected = Substitution::empty();
    expected.add(&b, &a);
    expected.add(&c, &a);
    assert!(expected == t_bc.matches(&t_aa).unwrap());

    // Replacing one var with two things isn't
    assert!(t_aa.matches(&t_bc).is_none());
}
