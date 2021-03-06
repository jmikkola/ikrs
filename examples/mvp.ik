// MVP features in ikko

fn main():
    // All variables are currently mutable
    let a = 123
    // Only one size of integer and floating types
    let b = 345.234
    // Strings are also allowed
    let c = "a quoted string \"escaped quote\" \\ newline->\n"
    // As are booleans.
    let d = False

    // Print is a temporary function to be replaced by standard library
    // functions later.
    print(c)

    // Numbers can be cast to strings
    // TODO print(to_string(b) + " " + to_string(a))

    print(do_stuff())

fn do_stuff():
   // basic math operations are supported
   let int  = 5 + (2 * 10) / 3 % 4

   //let fl Float = 99.0e-3 ** 2
   let fl  = 1.23 ** 2.73

   // Comparison operators
   let x  = fl < to_float(int)
   // let x = True

   // Bitwise operators (on Ints)
   //let y  = (~1024) & (15 << 2)
   // let y  = (~1024).bitand(15.lshift(2))

   // Boolean operators also exist
   if 1 == 1 && 2 < 3:
       print(recursive(10, 4))
   else if False:
       x = !!False

   let i  = 0
   while i < 10:
         print(to_string(i))
         i = i + 1

   return "foo"

// recursive functions are of course allowed
fn recursive(a, b):
   if b == 0:
      return to_string(a)
   return recursive(b, a % b)
