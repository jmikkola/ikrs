type ASTIsh enum:
  TInt
  TFloat
  TString
  TNamed:
    name String
  TFunc:
    arg ASTIsh
    ret ASTIsh

fn printEnum(a ASTIsh):
    print(to_string(a))

fn main():
  let a = TNamed{
    name: "some name",
  }
  print(to_string(a))
  print("\n")
  printEnum(a)
  print("\n")
