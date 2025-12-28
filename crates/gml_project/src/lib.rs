//! GML Project Validation
//!
//! This crate handles project-level health checks for GameMaker projects,
//! such as detecting orphaned files and ensuring the YYP is in sync with disk.

pub mod project;
pub mod yyp;

pub use project::{Project, ProjectIssue};
pub use yyp::Yyp;
