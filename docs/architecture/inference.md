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

**How should it return the methods inside of type classes?**

I'm starting to think that I might want to break this into two pieces: a "typing" pass that types
the whole program, and an "inference" system that handles just type inference for a specific
module. The typing pass would handle keeping track of things like typeclasses (meaning that the
first pass wouldn't have to do it) but would also allow the inference system to be more focused on
just the issues related to type inference (method dependency resolution (which maybe could be yet
another package), all the inference logic, etc).

**Would it work to combine AST for a module into a single structure?**

That would make some things more convenient, but it would be hard to deal with before type inference
because (a) import statements only apply to a file, not to a module and (b) it should be possible to
go from an expression reference back to the position and file it came from (this would add another
field for the file to every expression ref).

Hmm, what if references use some of their bits to encode which file they came from? Maybe allow 2^8
files and 2^24 expressions in a file? Or, what if each file got ranges of expression refs?
(e.g. this file starts at 0, that file starts at 2549, etc).

## Typing module

This module is what the compiler module calls directly. It takes the entire program, and calls the
inference module multiple times to do the work.

This module also:

* Keeps track of what classes have been declared.
* Ensures that instance declarations conform to class definitions.
* Invent constructor functions for structs/enums.

There will be a separate lowering pass that removes typeclasses via dictionary passing.

Should this also expect that imported names in the input ast have been renamed? (and would that
allow collapsing the ast for the module into a single package? - it might be hard to keep track of
which file a given part of the ast came from...).


**Input**

```rust

pub fn typecheck(
  ordered_packages: Vec<Package>,
) -> TypeResult {}
```


**Result**

```rust
struct TypeResult {
	declared_classes: HashMap<FullyQualifiedName, ClassDefinition>,
	indexed_types: Indexed<Type>,
	modules: Vec<ModuleResult>,
}

struct ModuleResult {
	functions: HashMap<FullyQualifiedName, Scheme>,
	// Right now, this is incorrect because ExpressionRef isn't unique in a module
    expressions: HashMap<ExpressionRef, TypeRef>,
	instances: Vec<Instance>,
}

struct Instance {
    class: TypeRef,
	type: Scheme,
    methods: HashMap<DeclarationRef, FunctionResult>,
}
```

TODO: Does this result format correctly deal with instantiation of generics?


## Inference module

This module handles a single input package at a time, though it does work with some global state.

Names in the input syntax have been re-written to reference fully-qualified names where they
reference imported names instead of local ones.

**Input**

``` rust
struct InferenceState {
	global_names: HashMap<FullyQualifiedName, Scheme>,
	declared_classes: HashMap<FullyQualifiedName, ClassDefinition>,
	indexed_types: Indexed<Type>,
}

struct ClassDefinition {
    name: TypeRef,
	constraints: Scheme,
	methods: HashMap<String, Scheme>,
}

pub fn infer(
  package_name: &str,
  files: &[&ast::Syntax],
  state: &mut InferenceState,
) -> InferenceResult {
}
```

**Result**


```rust
struct InferenceResult {
    function_types: HashMap<String, Scheme>,
    files: Vec<FileResult>,
}

struct FileResult {
    file_name: String,
	functions: HashMap<DeclarationRef, FunctionResult>,
	class_instances: HashMap<DeclarationRef, Instance>,
	expressions: HashMap<ExpressionRef, TypeRef>,
}

struct FunctionResult {
	name: String, // for debugging
	type: Scheme,
	instantiation: Substitution,
}

struct Instance {
    class: TypeRef,
	type: Scheme,
    methods: HashMap<DeclarationRef, FunctionResult>,
}
```



I'm not a fan of how the output has to be keyed to the input ast (and how the input ast needs to be
rewritten just to deal with imports).


# Other thoughts

* What would it take to allow different modules to be typechecked in parallel?
* Would it be possible (or even useful) to allow re-checking just the parts that are downstream of a
  changed file?

These are probably things I can defer worrying about until the compiler actually works.
