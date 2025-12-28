//! GML014-016: Control flow validation
//!
//! These checks are handled directly by the Checker in checker.rs:
//! - GML014: `break` outside of a loop/switch
//! - GML015: `continue` outside of a loop
//! - GML016: `return` outside of a function
//!
//! The checker tracks context (in_loop, in_function) during its single-pass
//! AST traversal and emits diagnostics for invalid control flow.
//!
//! Note: In GML, scripts and events ARE callable contexts where return/exit
//! is valid, so the checker starts with in_function = true.

// This module is intentionally kept for documentation and potential future
// enhancements, but the actual checking is done by checker.rs.
