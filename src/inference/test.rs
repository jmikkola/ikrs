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
