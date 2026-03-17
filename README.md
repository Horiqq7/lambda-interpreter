# Lambda Calculus Interpreter

A fully functional, interactive **Lambda Calculus interpreter** built
from scratch in **Haskell**.

This project implements a custom Read-Eval-Print Loop (REPL), a
hand-written monadic parser combinator library, and a robust evaluation
engine supporting alpha-conversion, beta-reduction, and multiple
evaluation strategies.

------------------------------------------------------------------------

## 🚀 Key Features

### Custom Monadic Parser

Parses λ-expressions without external dependencies (no
Parsec/Megaparsec).\
Supports both syntaxes: - `\x.x` - `λx.x`

### Evaluation Strategies

Supports step-by-step reduction using: - **Normal Order**
(call-by-name) - **Applicative Order** (call-by-value)

### Macro & Context System

Allows defining macros:

    ID = \x.x

Macros are expanded automatically during evaluation.

### Church Encodings

Includes a built-in standard library (`Default.hs`) with: - Booleans:
`TRUE`, `FALSE`, `AND`, `OR` - Pairs - Church Numerals: `N0`, `N1`,
`SUCC`, `ADD`, `MULT`

### Interactive REPL

Command-line interface for: - evaluating expressions - defining macros -
inspecting the environment

------------------------------------------------------------------------

## 📁 Project Structure

-   `Lambda.hs` -- Core calculus engine\
-   `Parser.hs` -- Custom parser combinator library\
-   `Binding.hs` -- Environment context and macro expansion\
-   `Default.hs` -- Built-in Church encodings\
-   `main.hs` -- REPL implementation\
-   `test.hs` & `Tests/` -- Automated test suite

------------------------------------------------------------------------

## 💻 Usage

### Running the REPL

``` bash
runhaskell main.hs
```

### REPL Commands

-   `<expression>` -- Evaluate expression\
-   `MACRO = <expression>` -- Define macro\
-   `:ctx` -- Show macros\
-   `:r` -- Reset context\
-   `:q` -- Quit

------------------------------------------------------------------------

## 🧪 Running Tests

``` bash
runhaskell test.hs
```

Optional: - `lambda` - `parser` - `binding` - `default`

------------------------------------------------------------------------

## 📖 Syntax Examples

Identity:

    \x.x
    λx.x

Application:

    (\x.x y)

Using built-in macros:

    ADD N1 N2
