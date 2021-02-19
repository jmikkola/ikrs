fn are_same(a t, b t) Bool where t: Eq:
    return a == b

fn max(a t, b t) t where t: Ord:
    if b > a:
        return b
    return a


fn main():
    print(to_string(are_same("x", "y")))
    print("\n")

    print(to_string(are_same(False, False)))
    print("\n")

    print(to_string(max(42, 6*7)))
    print("\n")

    print(to_string(max(2.78, 3.14)))
    print("\n")
