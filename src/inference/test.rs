use super::*;

fn simple_var(inference: &mut Inference, name: &str) -> TypeRef {
    let s = inference.save_string(name.to_owned());
    let t = Type::Var(s, inference.star_kind());
    inference.save_type(t)
}

fn type_app(inference: &mut Inference, left: TypeRef, right: TypeRef) -> TypeRef {
    let t = Type::App(left, right);
    inference.save_type(t)
}

#[test]
fn test_storing_kinds() {
    let mut inf = Inference::new();

    let star_ref = inf.star_kind();
    assert!(KindRef(0) == star_ref);
    assert!(&Kind::Star == inf.get_kind(star_ref));

    let inner1 = inf.apply_kinds(star_ref, star_ref);
    let k1 = inf.apply_kinds(inner1, star_ref);

    let inner2 = inf.apply_kinds(star_ref, star_ref);
    let k2 = inf.apply_kinds(inner2, star_ref);

    assert!(inner1 == inner2);
    assert!(k1 == k2);

    assert!(inf.get_kind(inner1) == &Kind::App(star_ref, star_ref));
    assert!(inf.get_kind(k1) == &Kind::App(inner1, star_ref));
}

#[test]
fn test_composing_substitutions() {
    let mut inference = Inference::new();

    let empty = Substitution::empty();

    assert!(empty == empty.compose(&empty, &mut inference));

    let a = simple_var(&mut inference, "a");
    let b = simple_var(&mut inference, "b");
    let a2b = Substitution::singleton(a, b);

    // Composing a (valid) substitution with itself doesn't change anything
    assert!(a2b == a2b.compose(&a2b, &mut inference));
    // Composing a substitution with the empty substitution does nothing
    assert!(a2b == a2b.compose(&empty, &mut inference));
    assert!(a2b == empty.compose(&a2b, &mut inference));

    let c = simple_var(&mut inference, "c");
    let b2c = Substitution::singleton(b, c);

    let mut expected = Substitution::empty();
    expected.add(a, c);
    expected.add(b, c);

    assert!(expected == a2b.compose(&b2c, &mut inference));
}

#[test]
fn test_merging_subsitutions() {
    let mut inference = Inference::new();

    let empty = Substitution::empty();

    assert!(empty == empty.merge(&empty).unwrap());

    let a = simple_var(&mut inference, "a");
    let b = simple_var(&mut inference, "b");
    let a2b = Substitution::singleton(a, b);

    // Composing a (valid) substitution with itself doesn't change anything
    assert!(a2b == a2b.merge(&a2b).unwrap());
    // Composing a substitution with the empty substitution does nothing
    assert!(a2b == a2b.merge(&empty).unwrap());
    assert!(a2b == empty.merge(&a2b).unwrap());

    let c = simple_var(&mut inference, "c");
    let d = simple_var(&mut inference, "d");
    let c2d = Substitution::singleton(c, d);

    let mut expected = Substitution::empty();
    expected.add(a, b);
    expected.add(c, d);

    // Test that the fields from the two are added together
    assert!(expected == a2b.merge(&c2d).unwrap());
    assert!(expected == c2d.merge(&a2b).unwrap());

    let a2c = Substitution::singleton(a, c);

    // Test a case where the two disagree
    assert!(a2b.merge(&a2c).is_none());
}

#[test]
fn test_matching_types() {
    let mut inference = Inference::new();
    let a = simple_var(&mut inference, "a");
    let b = simple_var(&mut inference, "b");
    let c = simple_var(&mut inference, "c");

    let t_aa = type_app(&mut inference, a, a);
    let t_bc = type_app(&mut inference, b, c);

    // Replacing two vars with one thing is OK
    let mut expected = Substitution::empty();
    expected.add(b, a);
    expected.add(c, a);
    assert!(expected == t_bc.matches(t_aa, &inference).unwrap());

    // Replacing one var with two things isn't
    assert!(t_aa.matches(t_bc, &inference).is_none());
}
