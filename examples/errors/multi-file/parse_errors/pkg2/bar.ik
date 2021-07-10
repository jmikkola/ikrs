package pkg2

// Two files in a package can use mutually recursive definitions
fn bar(n Int):
  if n > 0:
    foo(n - 1)
