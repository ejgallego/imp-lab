# Language

ImpLab includes a small function-based toy language designed for interpreter and debugger experiments.

## DSL entrypoint

- `imp%[...] : ImpLab.ProgramInfo`
- A program is written as a list of function declarations (`def name(params) := { ... }`).
- A `main()` function (zero params) is required.

## Statements

```lean
let v := N
let v := add v1 v2
let v := sub v1 v2
let v := mul v1 v2
let v := div v1 v2
let v := call f(a, b, ...)
return v
```

## Example

```lean
def p : ImpLab.ProgramInfo := imp%[
  def addMul(x, y) := {
    let s := add x y,
    let z := mul s y,
    return z
  },
  def main() := {
    let a := 2,
    let b := 5,
    let out := call addMul(a, b)
  }
]
```

## Source mapping

`ProgramInfo.located` stores source locations with function context (`func`, `stmtLine`, `span`).
This powers function-aware breakpoints and stack traces.
