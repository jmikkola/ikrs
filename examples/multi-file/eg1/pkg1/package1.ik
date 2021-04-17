package pkg1

// 'self' is a keyword indicating this package
import self.inner1

// otherwise, imports are relative to the project (as the first priority, before
// installed packages)
import pkg2

fn do_stuff():
  println("Hi")
