type Color enum:
  Red
  Blue
  Green


type Channel struct:
  color Color
  value Int


fn main():
  let a Color = Red{}
  a = Blue{}
  let c Channel = Channel{
    color: a,
    value: 123,
  }
  print(to_string(a))
  print("\n")
  print(to_string(c))
  print("\n")
