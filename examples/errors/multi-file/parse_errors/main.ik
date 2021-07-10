// "package main" is optional

import pkg1
import pkg1.inner1

// package 2 exists even though it is not imported

fn main():
  say_hello()
  pkg1.do_stuff()
  inner1.do_stuff(
