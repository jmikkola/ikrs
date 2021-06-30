Inference stage

# Interface

What should the interface to the inference stage look like?

One issue that complicates the matter is the fact that a single module is broken into multiple
files, each of which currently have their own `ast::Syntax` structure. It could work to pass tuples
of declarations and the `ast::Syntax` they belong to. Walking the syntax tree during inference would
then need to pass the reference to the syntax down the stack. Actually, that is probably necessary
anyway just to be able to read the AST, so perhaps that is not such a huge issue.

Another issue is finding the location of nodes in the file they came from for the sake of error
reporting. Right now, `ast::Syntax` doesn't store the locations of nodes, but that could easily be
added.

* Input: AST with fully-qualified names (types, functions, constants)
* Input: Types of imported names (maybe just a global list of all known public types?)
* The input should also contain the types of pre-defined "prelude" functions.
* Return value: schemes for functions, a sample instantiation of that scheme, and then concrete
  types for the nodes that reference that sample instantiation?
* Types can be attached to ast nodes by mentioning their Ref types.
* Resulting types should then be grouped by source file so that the ref types can be mapped to the
  concrete syntax again.
* Error handling: Ideally when encountering an error, it would record the error, pick some neutral
  resolution of it (e.g. just ignore the fact that two types cannot unify) and then continue
  on. This means that the result value should also contain a list of these errors.


## Questions

How should it return the methods inside of type classes?

I'm starting to think that I might want to break this into two pieces: a "typing" pass that types
the whole program, and an "inference" system that handles just type inference for a specific
module. The typing pass would handle keeping track of things like typeclasses (meaning that the
first pass wouldn't have to do it) but would also allow the inference system to be more focused on
just the issues related to type inference (method dependency resolution (which maybe could be yet
another package), all the inference logic, etc).

## Input type

``` rust
struct InferenceJob {
	package_name: String,
	global_names: HashMap<FullyQualifiedName, Scheme>,
	declared_classes: HashMap<FullyQualifiedName, ClassDefinition>,
	files: Vec<InferenceFile>,
}

struct InferenceFile {
    syntax: &'a ast::Syntax,
	// Do I need anything else?
}
```


## Return type

```rust
struct InferenceResult {
    indexed_types: Indexed<Type>, // should this borrow a global set of types? It is probably necessary to compare two types across modules...
    function_types: HashMap<String, Scheme>,
    files: Vec<FileResult>,
}

struct FileResult {
    file_name: String,
	syntax: &'a ast::Syntax, // ?
	functions: HashMap<DeclarationRef, (Scheme, Instantiation)>, // ?
	expressions: HashMap<ExpressionRef, TypeRef>,
	// typeclasses? Or does that come directly out of the first_pass
	// does functions include functions from the implementations of typeclasses,
	// probably with name mangling?
}
```
