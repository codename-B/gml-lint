//! GML Project Validation
//!
//! This crate handles project-level health checks for GameMaker projects,
//! such as detecting orphaned files and ensuring the YYP is in sync with disk.

pub mod proj;
pub mod yyp;

pub use proj::{Project, ProjectIssue};
pub use yyp::Yyp;
