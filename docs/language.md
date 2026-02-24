# Language

ImpLab includes a small imperative toy language designed for interpreter and debugger experiments.

## DSL entrypoint

- `imp%[...] : ImpLab.ProgramInfo`
- A program is written as a list of top-level declarations:
  - mutable globals: `global g := N`
  - function declarations: `def name(params) := { ... }`
- A `main()` function (zero params) is required.
- Duplicate function names and duplicate global names are rejected.

## Top-level declarations

```lean
global counter := 0
def bump(x) := { ... }
def main() := { ... }
```

Global declarations create heap cells initialized once at launch.

## Statements

```lean
let v := N
let v := get g
let v := add v1 v2
let v := sub v1 v2
let v := mul v1 v2
let v := div v1 v2
let v := call f(a, b, ...)
set g := v
return v
```

`get` reads from the global heap into a local variable. `set` writes a local value into the global heap.

Both `get` and `set` require `g` to be declared with a top-level `global` declaration.
Using undeclared globals is a runtime error.

## Example

```lean
def p : ImpLab.ProgramInfo := imp%[
  global counter := 10,
  def addMul(x, y) := {
    let s := add x y,
    let z := mul s y,
    return z
  },
  def main() := {
    let before := get counter,
    let a := 2,
    let b := 5,
    let out := call addMul(a, b),
    set counter := out,
    let after := get counter
  }
]
```

## Source mapping

`ProgramInfo.located` stores source locations with function context (`func`, `stmtLine`, `span`).
This powers function-aware breakpoints and stack traces.
Top-level `global` declarations do not contribute statement locations.
