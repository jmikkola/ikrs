fn main():
    let a = 1
    let b = 0
    while a < 200:
        print(to_string(a))
        print("\n")
        let temp = a
        a = a + b
        b = temp
