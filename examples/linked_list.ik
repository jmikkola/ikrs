type LinkedList enum:
  End
  Link:
    value String
    next LinkedList

fn length(l):
  return length_(l, 0)

fn length_(l, size):
  match l:
    End():
      return size
    Link(_, End()):
      return 1 + size
    Link(_, next):
      return length_(next, 1 + size)

  return 0

fn main():
  let a1 = Link{
    value: "last",
    next: End{
    },
  }
  let a2 = Link{
    value: "middle",
    next: a1,
  }
  let a3 = Link{
    value: "first",
    next: a2,
  }

  print(to_string(length(a3)))
  print("\n")
  print(to_string(a3))
  print("\n")
