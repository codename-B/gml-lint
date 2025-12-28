# gml-lint

An extremely fast GML (GameMaker Language) linter and static analysis tool, written in Rust.

Inspired by [Ruff](https://github.com/astral-sh/ruff)'s architecture with lint rules derived from [GMEdit](https://github.com/YellowAfterlife/GMEdit)'s linter and Feather.

## Performance

⚡ **~110ms cold start** for a 194-file project (1700+ files/sec)

| Phase | Time | % |
|-------|------|---|
| Discovery | ~80ms | 73% |
| Reading | ~7ms | 6% |
| Lexing | ~7ms | 6% |
| Parsing | ~40ms | 37% |
| Linting | ~55ms | 50% |

Optimizations include:
- Parallel file discovery using `jwalk`
- Parallel file reading and symbol extraction using `rayon`
- `FxHashSet` for fast built-in lookups
- Pre-allocated vectors to minimize reallocations
- Caching for incremental linting

## Features

- ⚡️ **Blazing fast** - Sub-120ms cold start for typical projects
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
| GML017 | Error | ConstructorValidation | Invalid constructor usage |
| GML020 | Error | TypeMismatch | Detected type mismatch in assignment |
| GML021 | Error | InvalidAssignment | Assignment to incompatible type |
| GML022 | Error | UnknownIdentifier | Use of an undefined identifier |

## License

MIT
