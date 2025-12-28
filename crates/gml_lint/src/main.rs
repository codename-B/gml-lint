//! gml-lint CLI - A fast GML linter
//!
//! Usage: gml-lint [OPTIONS] [PATHS]...

use anyhow::Result;
use clap::Parser;
use colored::Colorize;
use gml_cache::Cache;
use gml_diagnostics::{Diagnostic, Severity};
use gml_lexer::{Lexer, TokenKind};
use jwalk::WalkDir;

use rayon::prelude::*;
use std::panic;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;
use std::time::Instant;
use rustc_hash::FxHashSet;

/// Symbol provider that knows about project-wide scripts/functions/variables
struct ProjectSymbolProvider {
    functions: FxHashSet<String>,
    variables: FxHashSet<String>,
}

impl ProjectSymbolProvider {
    fn new() -> Self {
        Self { 
            functions: FxHashSet::default(),
            variables: FxHashSet::default(),
        }
    }
}

impl gml_linter::SymbolProvider for ProjectSymbolProvider {
    fn has_function(&self, name: &str) -> bool {
        self.functions.contains(name)
    }

    fn has_variable(&self, name: &str) -> bool {
        self.variables.contains(name)
    }
}

/// An extremely fast GML linter
#[derive(Parser, Debug)]
#[command(name = "gml-lint")]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
    #[command(flatten)]
    lint_args: LintArgs,
}

#[derive(clap::Subcommand, Debug)]
enum Commands {
    /// Lint GML files (default)
    Lint(LintArgs),
    /// Project health checks
    Project {
        #[command(subcommand)]
        cmd: ProjectCommands,
    },
}

#[derive(clap::Subcommand, Debug)]
enum ProjectCommands {
    /// Check for orphaned files and missing resources
    Check {
        /// Project root directory (containing .yyp)
        #[arg(default_value = ".")]
        path: PathBuf,
    },
}


#[derive(Parser, Debug, Clone)]
struct LintArgs {
    /// Files or directories to lint
    #[arg(default_value = ".")]
    paths: Vec<PathBuf>,

    /// Select specific rules (comma-separated)
    #[arg(long, value_delimiter = ',')]
    select: Option<Vec<String>>,

    /// Ignore specific rules (comma-separated)
    #[arg(long, value_delimiter = ',')]
    ignore: Option<Vec<String>>,

    /// Exclude files matching glob patterns (comma-separated)
    #[arg(long, value_delimiter = ',')]
    exclude: Option<Vec<String>>,

    /// Output format (text, json)
    #[arg(long, default_value = "text")]
    format: String,

    /// Apply fixes where available
    #[arg(long)]
    fix: bool,

    /// Show statistics
    #[arg(long)]
    statistics: bool,

    /// Suppress output (exit code only)
    #[arg(short, long)]
    quiet: bool,

    /// Disable caching (re-lint all files)
    #[arg(long)]
    no_cache: bool,
}


fn main() -> Result<()> {
    let cli = Cli::parse();
    
    match cli.command {
        Some(Commands::Lint(args)) => run_lint(args),
        Some(Commands::Project { cmd }) => run_project(cmd),
        None => run_lint(cli.lint_args),
    }
}

fn run_project(cmd: ProjectCommands) -> Result<()> {
    match cmd {
        ProjectCommands::Check { path } => {
            let project = gml_project::Project::new(path)?;
            println!("{} in {:?}", "Checking project health".cyan().bold(), project.root);

            
            let issues = project.check_health();
            if issues.is_empty() {
                println!("{}", "Project is healthy!".green().bold());
                return Ok(());
            }

            let mut orphaned = 0;
            let mut missing = 0;

            for issue in issues {
                match issue {
                    gml_project::ProjectIssue::OrphanedFile(path) => {
                        println!("{} {} Orphaned file: {}", "warning".yellow().bold(), "GML021".cyan(), path.display());
                        orphaned += 1;
                    }
                    gml_project::ProjectIssue::MissingResource(path) => {
                        println!("{} {} Missing resource: {}", "error".red().bold(), "GML022".cyan(), path.display());
                        missing += 1;
                    }
                }
            }

            println!("\nFound {} orphaned file(s) and {} missing resource(s).", orphaned, missing);
            if missing > 0 {
                std::process::exit(1);
            }
        }
    }
    Ok(())
}

fn run_lint(args: LintArgs) -> Result<()> {

    let start = Instant::now();

    // Track timing for each phase
    let discover_start = Instant::now();

    // Collect all GML files
    let files = collect_gml_files(&args.paths, args.exclude.as_deref());
    let file_count = files.len();

    let discover_time = discover_start.elapsed();

    if file_count == 0 {
        if !args.quiet {
            println!("{}", "No GML files found".yellow());
        }
        return Ok(());
    }

    let read_start = Instant::now();

    // Read all file contents AND extract symbols in a single parallel pass
    let file_data: Vec<(PathBuf, String, FxHashSet<String>, FxHashSet<String>)> = files
        .par_iter()
        .filter_map(|path| {
            std::fs::read_to_string(path).ok().map(|content| {
                let mut functions = FxHashSet::default();
                let mut variables = FxHashSet::default();
                extract_symbols(&content, &mut functions, &mut variables);
                (path.clone(), content, functions, variables)
            })
        })
        .collect();

    let read_time = read_start.elapsed();

    // Load cache (skip unchanged files)
    let project_root = args.paths.first().map(|p| {
        if p.is_dir() { p.clone() } else { p.parent().map(|p| p.to_path_buf()).unwrap_or_else(|| PathBuf::from(".")) }
    }).unwrap_or_else(|| PathBuf::from("."));
    
    let cache = if args.no_cache {
        Cache::new()
    } else {
        Cache::load(&project_root)
    };
    let cache = Mutex::new(cache);
    let cached_count = AtomicUsize::new(0);

    // Pre-allocate rules once for sharing across threads
    let all_rules = gml_linter::rules::all_rules();

    let symbol_start = Instant::now();

    // Build project symbol provider by merging extracted symbols
    let symbol_provider = build_symbol_provider_from_extracted(&project_root, &file_data, &cache);

    let symbol_time = symbol_start.elapsed();

    // Lint all files in parallel
    let error_count = AtomicUsize::new(0);
    let warning_count = AtomicUsize::new(0);

    let all_diagnostics: Vec<(PathBuf, Vec<Diagnostic>)> = file_data
        .par_iter()
        .filter_map(|(path, content, _functions, _variables)| {

            // Check cache - skip if file unchanged
            if !args.no_cache {
                let hash = gml_cache::hash_content(content);
                let cache_guard = cache.lock().unwrap();
                if cache_guard.is_fresh_with_hash(path, hash) {
                    cached_count.fetch_add(1, Ordering::Relaxed);
                    // File unchanged, but still need to count its diagnostics
                    if let Some(entry) = cache_guard.get(path) {
                        if entry.has_errors {
                            error_count.fetch_add(entry.diagnostic_count, Ordering::Relaxed);
                        }
                    }
                    return None; // Skip linting - file is fresh
                }
                drop(cache_guard);
            }


            // Lint with catch_unwind to prevent panics from crashing everything
            let lint_result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
                gml_linter::lint_source_with_rules(content, path.to_str().unwrap_or(""), &all_rules, &symbol_provider)
            }));


            let diagnostics = match lint_result {
                Ok(diags) => diags,
                Err(_) => {
                    if !args.quiet {
                        eprintln!("{}: Internal error (panic during linting)", path.display().to_string().red());
                    }
                    error_count.fetch_add(1, Ordering::Relaxed);
                    return None;
                }
            };

            // Update cache
            if !args.no_cache {
                let has_errors = diagnostics.iter().any(|d| matches!(d.severity, Severity::Error));
                let mut cache_guard = cache.lock().unwrap();
                cache_guard.update(path.clone(), content, diagnostics.len(), has_errors);
            }

            Some((path.clone(), diagnostics))
        })
        .collect();

    // Filter and count diagnostics
    let mut total_diagnostics = 0;
    let mut fix_count = 0;
    let mut output_buffer = String::with_capacity(16384);
    
    for (path, diagnostics) in &all_diagnostics {
        let mut file_fixes = Vec::new();
        
        for diag in diagnostics {
            // Apply --ignore filter
            if let Some(ref ignore) = args.ignore {
                if ignore.contains(&diag.code) {
                    continue;
                }
            }

            // Apply --select filter
            if let Some(ref select) = args.select {
                if !select.contains(&diag.code) {
                    continue;
                }
            }

            total_diagnostics += 1;

            match diag.severity {
                Severity::Error => { error_count.fetch_add(1, Ordering::Relaxed); }
                Severity::Warning => { warning_count.fetch_add(1, Ordering::Relaxed); }
                _ => {}
            };

            if args.fix {
                if let Some(fix) = &diag.fix {
                    file_fixes.extend(fix.edits.clone());
                    fix_count += 1;
                }
            }

            if !args.quiet {
                format_diagnostic_buffered(&mut output_buffer, path, diag, &args.format);
            }
        }
        
        if !file_fixes.is_empty() {
            if let Err(e) = apply_fixes(path, file_fixes) {
                if !args.quiet {
                    output_buffer.push_str(&format!("{}: Failed to apply fixes: {}\n", path.display().to_string().red(), e));
                }
            }
        }
    }

    if !args.quiet {
        use std::io::Write;
        let stdout = std::io::stdout();
        let mut handle = std::io::BufWriter::new(stdout.lock());
        let _ = write!(handle, "{}", output_buffer);
    }


    let duration = start.elapsed();

    // Print summary
    if !args.quiet && args.format == "text" {
        println!();
        let errors = error_count.load(Ordering::Relaxed);
        let warnings = warning_count.load(Ordering::Relaxed);

        if total_diagnostics == 0 {
            println!("{}", "All checks passed!".green().bold());
        } else {
            print!("Found ");
            if errors > 0 {
                print!("{} {}", errors.to_string().red().bold(), "error(s)".red());
            }
            if errors > 0 && warnings > 0 {
                print!(" and ");
            }
            if warnings > 0 {
                print!("{} {}", warnings.to_string().yellow().bold(), "warning(s)".yellow());
            }
            println!();
        }

        if fix_count > 0 {
            println!("{}", format!("Applied {} fix(es)", fix_count).green().bold());
        }
    }

    if args.statistics {
        let cached = cached_count.load(Ordering::Relaxed);
        println!(
            "\nChecked {} files in {:.2}ms ({:.0} files/sec){}",
            file_count,
            duration.as_secs_f64() * 1000.0,
            file_count as f64 / duration.as_secs_f64(),
            if cached > 0 { format!(" [{} cached]", cached) } else { String::new() }
        );

        // Detailed phase timing
        let discover_ms = discover_time.as_secs_f64() * 1000.0;
        let read_ms = read_time.as_secs_f64() * 1000.0;
        let symbol_ms = symbol_time.as_secs_f64() * 1000.0;
        let total_ms = duration.as_secs_f64() * 1000.0;

        println!("  Discovery: {:>6.2}ms ({:>5.1}%)", discover_ms, (discover_ms / total_ms) * 100.0);
        println!("  Reading:   {:>6.2}ms ({:>5.1}%)", read_ms, (read_ms / total_ms) * 100.0);
        println!("  Symbols:   {:>6.2}ms ({:>5.1}%)", symbol_ms, (symbol_ms / total_ms) * 100.0);

        let (lex_ms, parse_ms, lint_ms) = gml_linter::get_statistics_ms();
        if lex_ms + parse_ms + lint_ms > 0.0 {
            println!("  Lexing:    {:>6.2}ms ({:>5.1}%)", lex_ms, (lex_ms / total_ms) * 100.0);
            println!("  Parsing:   {:>6.2}ms ({:>5.1}%)", parse_ms, (parse_ms / total_ms) * 100.0);
            println!("  Linting:   {:>6.2}ms ({:>5.1}%)", lint_ms, (lint_ms / total_ms) * 100.0);
        }
    }


    // Save cache to disk
    if !args.no_cache {
        let _ = cache.lock().unwrap().save(&project_root);
    }

    // Exit with error code if there were errors
    let errors = error_count.load(Ordering::Relaxed);
    if errors > 0 {
        std::process::exit(1);
    }

    Ok(())
}

fn collect_gml_files(paths: &[PathBuf], exclude_patterns: Option<&[String]>) -> Vec<PathBuf> {
    let mut files = Vec::new();

    for path in paths {
        if path.is_file() {
            if path.extension().is_some_and(|ext| ext == "gml")
                && !is_excluded(path, exclude_patterns) {
                    files.push(path.clone());
                }
        } else if path.is_dir() {
            let walker = WalkDir::new(path)
                .skip_hidden(true)
                .process_read_dir(|_depth, _path, _read_dir_state, children| {
                    children.retain(|entry| {
                        if let Ok(e) = entry {
                            let name = e.file_name().to_string_lossy();
                            name != "node_modules" && name != "target"
                        } else {
                            true
                        }
                    });
                });

            for entry in walker.into_iter().flatten() {
                let entry_path = entry.path();
                if entry_path.is_file() 
                    && entry_path.extension().is_some_and(|ext| ext == "gml")
                    && !is_excluded(&entry_path, exclude_patterns) 
                {
                    files.push(entry_path);
                }
            }
        }
    }

    files
}

/// Build a symbol provider by merging pre-extracted symbols from all files
fn build_symbol_provider_from_extracted(
    project_root: &Path, 
    file_data: &[(PathBuf, String, FxHashSet<String>, FxHashSet<String>)],
    cache: &Mutex<Cache>,
) -> ProjectSymbolProvider {
    let mut provider = ProjectSymbolProvider::new();
    
    // 1. Add resource names from .yyp (try cache first, then parse)
    if let Ok(project) = gml_project::Project::new(project_root.to_path_buf()) {
        let cache_guard = cache.lock().unwrap();
        let resource_names = if let Some(cached) = cache_guard.get_yyp_resources(&project.yyp_path) {
            cached.to_vec()
        } else {
            drop(cache_guard);  // Release lock before parsing
            let names = project.get_resource_names();
            cache.lock().unwrap().set_yyp_resources(&project.yyp_path, names.clone());
            names
        };
        
        for name in resource_names {
            provider.functions.insert(name.clone());
            provider.variables.insert(name);
        }
    }
    
    // 2. Merge pre-extracted symbols from all files
    for (_path, _content, functions, variables) in file_data {
        provider.functions.extend(functions.iter().cloned());
        provider.variables.extend(variables.iter().cloned());
    }
    
    provider
}

/// Extract function names and macro names from GML source using the lexer
fn extract_symbols(source: &str, functions: &mut FxHashSet<String>, variables: &mut FxHashSet<String>) {
    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token();
        if matches!(token.kind, TokenKind::Eof) {
            break;
        }
        tokens.push(token);
    }

    let mut i = 0;
    while i < tokens.len() {
        match &tokens[i].kind {
            TokenKind::Function => {
                // function name(...)
                if i + 1 < tokens.len() {
                    if let TokenKind::Identifier(name) = &tokens[i + 1].kind {
                        functions.insert(name.to_string());
                    }
                }
            }
            TokenKind::Macro(name, body) => {
                functions.insert(name.to_string());
                if let Some(content) = body {
                    extract_symbols(content, functions, variables);
                }
            }
            TokenKind::Enum => {
                // enum Name { ... }
                if i + 1 < tokens.len() {
                     if let TokenKind::Identifier(name) = &tokens[i + 1].kind {
                         variables.insert(name.to_string());
                     }
                }
            }
            TokenKind::Var | TokenKind::Static => {
                // var name1, name2 = value, ...;
                // static name = ...;
                let mut j = i + 1;
                while j < tokens.len() {
                    if let TokenKind::Identifier(name) = &tokens[j].kind {
                        variables.insert(name.to_string());
                        j += 1;
                        
                        // Skip initialization if it exists
                        if j < tokens.len() && matches!(tokens[j].kind, TokenKind::Assign) {
                            j += 1;
                            // Skip until comma or end of statement
                            while j < tokens.len() && !matches!(tokens[j].kind, TokenKind::Comma | TokenKind::Semicolon | TokenKind::Newline | TokenKind::RightBrace) {
                                j += 1;
                            }
                        }
                    } else {
                        break;
                    }

                    if j < tokens.len() && matches!(tokens[j].kind, TokenKind::Comma) {
                        j += 1;
                        continue;
                    } else {
                        break;
                    }
                }
            }
            TokenKind::Identifier(name) => {
                // name = function(...) or name = method(...)
                if i + 1 < tokens.len() && matches!(tokens[i + 1].kind, TokenKind::Assign) {
                    if i + 2 < tokens.len() && matches!(tokens[i + 2].kind, TokenKind::Function) {
                        functions.insert(name.to_string());
                    } else {
                        variables.insert(name.to_string());
                    }
                }

                // self.name = ... or other.name = ...
                if (matches!(*name, "self" | "other")) && i + 2 < tokens.len() && matches!(tokens[i + 1].kind, TokenKind::Dot) {
                    if let TokenKind::Identifier(member) = &tokens[i + 2].kind {
                        if i + 3 < tokens.len() && matches!(tokens[i + 3].kind, TokenKind::Assign) {
                            if i + 4 < tokens.len() && matches!(tokens[i + 4].kind, TokenKind::Function) {
                                functions.insert(member.to_string());
                            } else {
                                variables.insert(member.to_string());
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        i += 1;
    }
}

/// Check if a path matches any of the exclude patterns
fn is_excluded(path: &Path, exclude_patterns: Option<&[String]>) -> bool {
    let Some(patterns) = exclude_patterns else {
        return false;
    };
    
    let path_str = path.to_string_lossy();
    
    for pattern in patterns {
        // Support simple glob patterns: * (any chars), ** (recursive)
        if matches_glob_pattern(&path_str, pattern) {
            return true;
        }
    }
    
    false
}

/// Simple glob pattern matching supporting * and ** wildcards
fn matches_glob_pattern(path: &str, pattern: &str) -> bool {
    // Normalize path separators for cross-platform matching
    let path = path.replace('\\', "/");
    let pattern = pattern.replace('\\', "/");
    
    // Handle ** (match any path segment)
    if pattern.contains("**") {
        let parts: Vec<&str> = pattern.split("**").collect();
        if parts.len() == 2 {
            let prefix = parts[0].trim_end_matches('/');
            let suffix = parts[1].trim_start_matches('/');
            
            // Check if path contains the pattern components
            if !prefix.is_empty() && !path.contains(prefix) {
                return false;
            }
            if !suffix.is_empty() && !path.contains(suffix) {
                return false;
            }
            
            // Check ordering: prefix must come before suffix
            if !prefix.is_empty() && !suffix.is_empty() {
                if let Some(prefix_pos) = path.find(prefix) {
                    if let Some(suffix_pos) = path.find(suffix) {
                        return suffix_pos > prefix_pos + prefix.len();
                    }
                }
                return false;
            }
            
            return true;
        }
    }
    
    // Handle simple * (match within single segment) or exact substring match
    // For simplicity, treat pattern as a substring match (common use case)
    if pattern.contains('*') {
        // Convert glob to simple match: *pattern* means contains "pattern"
        let clean_pattern = pattern.trim_matches('*');
        return path.contains(clean_pattern);
    }
    
    // Exact substring match
    path.contains(&pattern)
}

fn format_diagnostic_buffered(buffer: &mut String, path: &Path, diag: &Diagnostic, format: &str) {
    use std::fmt::Write;
    if format == "json" {
        let _ = writeln!(
            buffer,
            r#"{{"file":"{}","line":{},"column":{},"code":"{}","message":"{}","severity":"{}"}}"#,
            path.display(),
            diag.location.line,
            diag.location.column,
            diag.code,
            diag.message.replace('"', "\\\""),
            format_severity(&diag.severity)
        );
    } else {
        let line = format!(
            "{}:{}:{}: {} [{}]: {}\n",
            path.display().to_string().white().bold(),
            diag.location.line,
            diag.location.column,
            match diag.severity {
                Severity::Error => "error".red().bold(),
                Severity::Warning => "warning".yellow().bold(),
                Severity::Info => "info".blue(),
                Severity::Hint => "hint".cyan(),
            },
            diag.code.cyan(),
            diag.message
        );
        buffer.push_str(&line);
    }
}

fn format_severity(severity: &Severity) -> &'static str {
    match severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Info => "info",
        Severity::Hint => "hint",
    }
}

fn apply_fixes(path: &Path, mut edits: Vec<gml_diagnostics::Edit>) -> Result<()> {
    let mut content = std::fs::read_to_string(path)?;
    edits.sort_by(|a, b| b.start.cmp(&a.start));
    
    for edit in edits {
        let start = edit.start as usize;
        let end = edit.end as usize;
        
        if start <= content.len() && end <= content.len() && start <= end {
            content.replace_range(start..end, &edit.replacement);
        }
    }
    
    std::fs::write(path, content)?;
    Ok(())
}
