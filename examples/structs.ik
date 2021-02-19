type Bar struct:
    x Int
    y Float


type Foo struct:
    asdf Int
    xyz Bar


fn main():
    let foo = Foo{
        asdf: 123 + 345,
        xyz: Bar{
            x: 1,
            y: 2.333,
        },
    }
    let b = foo.xyz
    // print(to_string(foo))
    print("Hello, world\n")
    foo.xyz.x = 10000
    print("an Int: ")
    print(to_string(foo.xyz.x + 9))
    //print("\na Float: ")
    //print(to_string(foo.xyz.y * 0.6))
    print("\n")
