# gml-lint

An extremely fast GML (GameMaker Language) linter and static analysis tool, written in Rust.

Inspired by [Ruff](https://github.com/astral-sh/ruff)'s architecture with lint rules derived from [GMEdit](https://github.com/YellowAfterlife/GMEdit)'s linter and Feather.

## Performance

⚡ **~48ms warm start** (10,000+ files/sec)
⚡ **~340ms initial scan** (cold cache)

Tested against [Scribble](https://github.com/JujuAdams/Scribble) (500+ files).

| Phase | Time (Warm) | % |
|-------|------|---|
| Discovery | ~7ms | 15% |
| Reading | ~28ms | 58% |
| Lexing | ~3ms | 6% |
| Parsing | ~4ms | 8% |
| Linting | ~6ms | 13% |

Optimizations include:
- **Memory-mapped file I/O** for fast reading
- **DashMap** for lock-free concurrent caching
- **Parallel directory walking** and file processing using `rayon`
- `FxHashSet` for fast built-in lookups
- Pre-allocated vectors to minimize reallocations
- Caching for incremental linting

## Features

- ⚡️ **Blazing fast** - Sub-50ms warm start for typical projects
- 🎮 **GML-native** - Built specifically for GameMaker Language, supporting modern 2024+ syntax
- 🔧 **Auto-fix** - Automatic error correction (e.g., missing semicolons, single equals in conditions)
- 🔍 **Type Inference** - Deep static analysis to detect type mismatches across scopes
- 📦 **Caching** - Skip unchanged files for incremental linting using `.gml-lint-cache`
- 🛠️ **Configurable** - Fine-grained control with `gml-lint.toml`

## Installation

```bash
cargo install --path crates/gml_lint
```

## Usage

```bash
# Lint current directory
gml-lint .

# Lint specific file
gml-lint objects/obj_player/Create_0.gml

# Lint with specific rules
gml-lint --select GML001,GML003

# Output as JSON
gml-lint --format json

# Auto-fix
gml-lint --fix

# Show performance statistics
gml-lint --statistics
```

## Rules

| Code | Category | Name | Description |
|------|----------|------|-------------|
| GML001 | Error | UninitializedVariable | Potentially uninitialized variable |
| GML002 | Error | ReadonlyAssignment | Assignment to readonly variable or literal |
| GML003 | Warning | UnusedVariable | Variable defined but never used |
| GML004 | Warning | UnreachableCode | Statement follows a return or break |
| GML005 | Style | MissingSemicolon | (Fixable) Missing trailing semicolon |
| GML006 | Warning | KeywordShadowing | Variable shadows a built-in keyword |
| GML007 | Style | NoSingleEquals | (Fixable) Using `=` instead of `==` in conditions |
| GML008 | Style | RequireParentheses | Missing parentheses in complex expressions |
| GML009 | Style | SemicolonBeforeBranch | Semicolon before `else`, `while`, etc. |
| GML010 | Error | CheckHasReturn | Path missing a return value |
| GML011 | Error | CheckArgumentCounts | Incorrect number of arguments for function |
| GML012 | Warning | WarnVarRedeclaration | Variable redeclared in same scope |
| GML013 | Error | UnknownFunction | Use of an undefined global function |
| GML014 | Error | BreakOutsideLoop | `break` used outside of loop or switch |
| GML015 | Error | ContinueOutsideLoop | `continue` used outside of loop |
| GML016 | Error | ReturnOutsideFunction | `return` used outside of callable context |
| GML017 | Error | ConstructorValidation | Invalid constructor usage |
| GML020 | Error | TypeMismatch | Detected type mismatch in assignment |
| GML021 | Error | InvalidAssignment | Assignment to incompatible type |
| GML022 | Error | UnknownIdentifier | Use of an undefined identifier |
| GML023 | Error | UninitializedGlobal | Accessing a global variable before initialization |
| GML024 | Style | SimplifyUndefinedCheck | (Fixable) Use implicit boolean checks for safe types |
| GML025 | Error | UnsupportedTernary | Unparenthesized nested ternary operator |
| GML026 | Warning | UncapturedClosureVar | Variable used in closure but not captured |
| GML027 | Warning | RedundantBooleanComparison | Redundant comparison with boolean literal |
| GML028 | Warning | EmptyBlock | Empty block statement |

## License

MIT
