# OCaml Environment Model Interpreter

## Overview

This project implements an interpreter for a subset of OCaml, developed as part of the CAS CS 320: Principles of Programming Languages course at Boston University (Spring 2025). The interpreter supports parsing, type checking, and evaluation using the environment model and big-step operational semantics. It handles constructs such as literals, conditionals, operators, functions, and recursive let-expressions, with error handling for type errors and runtime exceptions.

Written in OCaml, the interpreter is built using a Dune project structure, with core logic in `interp2/lib/interp2.ml`. This project demonstrates proficiency in functional programming, type checking, and the environment model for evaluation.

## Features

- **Parsing**: Converts input programs into an internal `expr` representation via desugaring, respecting operator precedence.
- **Type Checking**: Verifies expressions against typing rules, ensuring type safety with detailed error reporting.
- **Evaluation**: Executes expressions to produce values (e.g., integers, booleans, closures) using big-step semantics.
- **Error Handling**: Manages runtime exceptions like division by zero and assertion failures.
- **Testing**: Includes a test suite in `test_interp2.ml` to validate functionality.

## Supported Constructs

The interpreter supports the following OCaml constructs:
- **Literals**: `()`, `true`, `false`, integers
- **Variables**: Variable references with type annotations
- **Conditionals**: `if e1 then e2 else e3`
- **Operators**: Arithmetic (`+`, `-`, `*`, `/`, `mod`), comparisons (`<`, `<=`, `>`, `>=`, `=`, `<>`), logical (`&&`, `||`)
- **Functions**: Anonymous functions (`fun x : ty -> e`) and function application
- **Let-Expressions**: `let [rec] x : ty = e1 in e2` for non-recursive and recursive bindings
- **Assertions**: `assert e`

**Operator Precedence** (highest to lowest):
| Operators         | Associativity |
|-------------------|---------------|
| Function application | Left       |
| `*`, `/`, `mod`   | Left          |
| `+`, `-`          | Left          |
| `<`, `<=`, `>`, `>=`, `=`, `<>` | Left |
| `&&`              | Right         |
| `||`              | Right         |

## Implementation Details

The project is implemented in `interp2/lib/interp2.ml`, with types and utilities in `lib/utils/utils.ml`.

### Parsing
- Uses `Parser.prog` and `Lexer.read` to parse input strings into a `prog` structure.
- The `desugar` function translates surface syntax (`prog`) to abstract syntax (`expr`), handling:
  - Empty programs as `Unit`.
  - Top-level let-statements as nested `Let` expressions ending in the last variable.
  - Multi-argument functions and let-expressions as curried `Fun` expressions.

### Type Checking
Implements `type_of` to verify expressions against typing rules:
- **Literals**: `Unit` (`UnitTy`), `Bool` (`BoolTy`), `Num` (`IntTy`).
- **Variables**: Checks for `Var x` in the context (`Env.find_opt`).
- **Operators**: Ensures correct types (e.g., `IntTy` for arithmetic, `BoolTy` for `&&`).
- **Conditionals**: Verifies `If` condition is `BoolTy` and branches have matching types.
- **Functions**: Checks `Fun` and `App` for consistent function types (`FunTy`).
- **Let-Expressions**: Validates bindings against annotations, ensuring recursive bindings are functions.
- **Assertions**: Requires `Assert e` to have `BoolTy`.
- Returns `Ok ty` for well-typed expressions or `Error err` (e.g., `UnknownVar`, `IfTyErr`).

### Evaluation
Implements `eval` using big-step semantics and the environment model:
- **Literals**: Maps to `VUnit`, `VBool`, `VNum`.
- **Variables**: Retrieves values from the dynamic environment (`Env.find_opt`).
- **Operators**: Evaluates operands and applies operations, with short-circuiting for `&&` and `||`.
- **Conditionals**: Evaluates `If` based on the condition’s boolean value.
- **Functions**: Creates closures (`VClos`) for `Fun`, capturing the environment.
- **Applications**: Evaluates `App` by applying closures to arguments.
- **Let-Expressions**: Binds values in the environment, handling recursive closures via self-referential environments.
- **Assertions**: Raises `AssertFail` for false assertions.
- Raises exceptions: `DivByZero` (division/modulus by zero), `AssertFail` (failed assertions).

### Interp Function
- The `interp` function combines parsing, desugaring, type checking, and evaluation, returning `Ok value` or `Error err` (e.g., `ParseErr`, `FunArgTyErr`).

## Project Structure

- `lib/interp2.ml`: Core implementation of parsing, type checking, and evaluation.
- `lib/utils/utils.ml`: Defines types (`ty`, `expr`, `value`, etc.) and utilities.
- Dune build system for compilation and testing.

## Setup and Installation

### Prerequisites
- OCaml 4.14 or later
- Dune 3.0 or later
- Git

### Installation
1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/ocaml-env-interpreter.git
   cd ocaml-env-interpreter
   ```
2. Build the project:
   ```bash
   eval $(opam env)
   dune build
   ```

### Running Tests
Execute the test suite:
```bash
dune test
```

## Usage

Use the `interp` function to parse, type-check, and evaluate programs:
```ocaml
let input = "let x : int = 3 in x + 2" in
match interp input with
| Ok value -> (* Returns VNum 5 *)
| Error ParseErr -> (* Handles parse errors *)
| Error err -> (* Handles type errors *)
```

## Notes

- The implementation adheres to the course’s skeleton code and exact function signatures in `interp2.ml`.
- The test suite in `test_interp2.ml` validates core functionality, with additional manual testing for edge cases (e.g., recursive functions).
- Helper functions in `desugar` and `eval` reduce code duplication, enhancing maintainability.

## Acknowledgments

- CAS CS 320 course staff for providing the project specification and skeleton code.