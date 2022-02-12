use super::*;

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

    let type_a = Type::Var(inference.save_string("a".to_owned()), inference.star_kind());
    let type_b = Type::Var(inference.save_string("b".to_owned()), inference.star_kind());
    let a = inference.save_type(type_a);
    let b = inference.save_type(type_b);
    let a2b = Substitution::singleton(a, b);

    // Composing a (valid) substitution with itself doesn't change anything
    assert!(a2b == a2b.compose(&a2b, &mut inference));
    // Composing a substitution with the empty substitution does nothing
    assert!(a2b == a2b.compose(&empty, &mut inference));
    assert!(a2b == empty.compose(&a2b, &mut inference));

    let type_c = Type::Var(inference.save_string("c".to_owned()), inference.star_kind());
    let c = inference.save_type(type_c);
    let b2c = Substitution::singleton(b, c);

    let mut expected = Substitution::empty();
    expected.add(a, c);
    expected.add(b, c);

    assert!(expected == a2b.compose(&b2c, &mut inference));
}
