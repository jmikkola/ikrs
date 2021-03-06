type Pair<a, b> struct:
    first a
    second b

type Maybe<val> enum:
    Nothing
    Just:
        value val

fn isJust(m Maybe<v>) Bool:
    match m:
        Nothing():
            return False
        Just(_):
            return True

fn flip(p Pair<a, b>) Pair<b, a>:
    return Pair{
        first: p.second,
        second: p.first,
    }

fn foo(p Pair<a, b>) Pair<a, b>:
    return p

fn main():
    let p1 = Pair{
        first: 123,
        second: "foo",
    }
    print(to_string(p1))
    print("\n")
    print(to_string(flip(p1)))
    print("\n")
    let m2 Maybe<Pair<Int, Int>> = Nothing{}
    print(to_string(isJust(m2)))
    print("\n")
