//! gml-lint CLI - A fast GML linter
//!
//! Usage: gml-lint [OPTIONS] [PATHS]...

use anyhow::Result;
use clap::Parser;
use colored::Colorize;
use gml_linter::cache::Cache;
use gml_linter::diagnostics::{Diagnostic, Severity};
use gml_linter::lexer::{Lexer, TokenKind};
use dashmap::DashSet;
use gml_linter::walker::SmartWalker;

use rayon::prelude::*;
use std::panic;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Instant;
use std::fs::File;
use memmap2::Mmap;
use rustc_hash::FxHashSet;

/// Type alias for file data extracted during parallel read
/// (path, content, functions, variables, globals)
type FileData = (PathBuf, String, FxHashSet<String>, FxHashSet<String>, FxHashSet<String>);

/// Symbol provider that knows about project-wide scripts/functions/variables
struct ProjectSymbolProvider {
    functions: FxHashSet<String>,
    variables: FxHashSet<String>,
    globals: FxHashSet<String>,
}

impl ProjectSymbolProvider {
    fn new() -> Self {
        Self { 
            functions: FxHashSet::default(),
            variables: FxHashSet::default(),
            globals: FxHashSet::default(),
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

    fn has_global(&self, name: &str) -> bool {
        self.globals.contains(name)
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
            let project = gml_linter::project::Project::new(path)?;
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
                    gml_linter::project::ProjectIssue::OrphanedFile(path) => {
                        println!("{} {} Orphaned file: {}", "warning".yellow().bold(), "GML021".cyan(), path.display());
                        orphaned += 1;
                    }
                    gml_linter::project::ProjectIssue::MissingResource(path) => {
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
    let discover_start = Instant::now();

    // Determine project root for cache loading
    let project_root = args.paths.first().map(|p| {
        if p.is_dir() { p.clone() } else { p.parent().map(|p| p.to_path_buf()).unwrap_or_else(|| PathBuf::from(".")) }
    }).unwrap_or_else(|| PathBuf::from("."));

    // Load cache
    let cache = if args.no_cache {
        Cache::new()
    } else {
        Cache::load(&project_root)
    };
    // Share cache via Arc (DashMap provides internal synchronization)
    let cache = Arc::new(cache);

    // Channels for file pipeline
    let (tx, rx) = crossbeam::channel::unbounded::<PathBuf>();

    // Set of seen files (to avoid double processing)
    let seen_files = Arc::new(DashSet::new());

    // Optimistically seed the pipeline with cached files
    // This allows us to start reading and analyzing files immediately,
    // while the walker confirms their existence in the background.
    if !args.no_cache {
        let cached_paths = cache.get_files();

        for path in cached_paths {
            // Mark as seen so SmartWalker doesn't re-send them
            seen_files.insert(path.clone());
            // Send to pipeline immediately
            let _ = tx.send(path);
        }
    }

    // Set of confirmed files (actually exist on disk during this run)
    let confirmed_files = Arc::new(DashSet::new());

    // 2. Start Discovery using SmartWalker
    let tx_discovery = tx.clone();
    let paths_clone = args.paths.clone();
    let exclude_clone = args.exclude.clone();
    let seen_files_clone = seen_files.clone();
    let confirmed_files_clone = confirmed_files.clone();
    let cache_clone = cache.clone();

    let discovery_thread = std::thread::spawn(move || {
        let walker = SmartWalker::new(
            tx_discovery,
            seen_files_clone,
            confirmed_files_clone,
            exclude_clone,
            cache_clone,
        );
        walker.run(&paths_clone);
    });

    // Drop the original tx so the channel closes when discovery finishes
    drop(tx);

    let discover_time = discover_start.elapsed();

    // 3. Collect all file paths from discovery, then batch read in parallel
    let read_start = Instant::now();

    // First, wait for discovery to complete and collect all paths
    let _ = discovery_thread.join();
    
    // Collect all paths from the channel
    let all_paths: Vec<PathBuf> = rx.into_iter().collect();

    // Batch parallel read - more efficient than par_bridge streaming
    let file_data: Vec<FileData> = all_paths
        .par_iter()
        .filter(|path| confirmed_files.contains(*path))
        .filter_map(|path| {
            // Attempt to read file using memory-mapped I/O for better performance
            read_file_fast(path).map(|content| {
                let mut functions = FxHashSet::default();
                let mut variables = FxHashSet::default();
                let mut globals = FxHashSet::default();
                extract_symbols(&content, &mut functions, &mut variables, &mut globals);
                (path.clone(), content, functions, variables, globals)
            })
        })
        .collect();

    let read_time = read_start.elapsed();
    
    // File data is already filtered to confirmed files

    let file_count = file_data.len();
    if file_count == 0 {
        if !args.quiet {
            println!("{}", "No GML files found (checked extensions: .gml)".yellow());
        }
        return Ok(());
    }

    if !args.quiet && args.format == "text" {
        println!("{}", format!("Linting {} file(s)...", file_count).cyan().bold());
    }

    // The rest of the pipeline remains similar...
    let cached_count = AtomicUsize::new(0);

    // Pre-allocate rules once for sharing across threads
    let all_rules = gml_linter::rules::all_rules();

    // Validate --ignore and --select arguments against available rules
    let valid_codes: FxHashSet<&str> = all_rules.iter().map(|r| r.code().as_str()).collect();

    if let Some(ref ignore) = args.ignore {
        for code in ignore {
            if !valid_codes.contains(code.as_str()) {
                println!("{} Unknown rule code in --ignore: {}", "warning".yellow().bold(), code);
            }
        }
    }

    if let Some(ref select) = args.select {
        for code in select {
            if !valid_codes.contains(code.as_str()) {
                println!("{} Unknown rule code in --select: {}", "warning".yellow().bold(), code);
            }
        }
    }

    let symbol_start = Instant::now();

    // Build project symbol provider by merging extracted symbols
    let symbol_provider = build_symbol_provider_from_extracted(&project_root, &file_data, &cache);

    let symbol_time = symbol_start.elapsed();

    let analysis_start = Instant::now();

    // Lint all files in parallel
    let error_count = AtomicUsize::new(0);
    let warning_count = AtomicUsize::new(0);
    let info_count = AtomicUsize::new(0);
    let hint_count = AtomicUsize::new(0);

    let all_diagnostics: Vec<(PathBuf, Vec<Diagnostic>)> = file_data
        .par_iter()
        .filter_map(|(path, content, _functions, _variables, _globals)| {

            // Check cache - skip if file unchanged
            if !args.no_cache {
                let hash = gml_linter::cache::hash_content(content);
                if cache.is_fresh_with_hash(path, hash) {
                    cached_count.fetch_add(1, Ordering::Relaxed);
                    // File unchanged, but still need to count its diagnostics
                    if let Some(entry) = cache.get(path) {
                        if entry.has_errors {
                            error_count.fetch_add(entry.diagnostic_count, Ordering::Relaxed);
                        }
                    }
                    return None; // Skip linting - file is fresh
                }
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
                cache.update(path.clone(), content, diagnostics.len(), has_errors);
            }

            Some((path.clone(), diagnostics))
        })
        .collect();

    let analysis_time = analysis_start.elapsed();

    // Filter and count diagnostics
    let mut total_diagnostics = 0;
    let mut fix_count = 0;
    let mut fixable_count = 0;
    let mut output_buffer = String::with_capacity(16384);
    
    // Create a map for quick content lookup
    let content_map: std::collections::HashMap<&PathBuf, &String> = file_data
        .iter()
        .map(|(path, content, _, _, _)| (path, content))
        .collect();

    for (path, diagnostics) in &all_diagnostics {
        let mut file_fixes = Vec::new();
        let content = content_map.get(path).map(|s| s.as_str()).unwrap_or("");
        
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
                Severity::Info => { info_count.fetch_add(1, Ordering::Relaxed); }
                Severity::Hint => { hint_count.fetch_add(1, Ordering::Relaxed); }
            };

            if diag.fix.is_some() {
                fixable_count += 1;
            }

            if args.fix {
                if let Some(fix) = &diag.fix {
                    file_fixes.extend(fix.edits.clone());
                    fix_count += 1;
                }
            }

            if !args.quiet {
                format_diagnostic_buffered(&mut output_buffer, path, diag, &args.format, content);
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
        let infos = info_count.load(Ordering::Relaxed);
        let hints = hint_count.load(Ordering::Relaxed);

        if total_diagnostics == 0 {
            println!("{}", "All checks passed!".green().bold());
        } else {
            print!("Found ");
            let mut parts = Vec::new();
            if errors > 0 {
                parts.push(format!("{} {}", errors.to_string().red().bold(), "error(s)".red()));
            }
            if warnings > 0 {
                parts.push(format!("{} {}", warnings.to_string().yellow().bold(), "warning(s)".yellow()));
            }
            if infos > 0 {
                parts.push(format!("{} {}", infos.to_string().blue().bold(), "info(s)".blue()));
            }
            if hints > 0 {
                parts.push(format!("{} {}", hints.to_string().cyan().bold(), "hint(s)".cyan()));
            }

            for (i, part) in parts.iter().enumerate() {
                if i > 0 {
                    if i == parts.len() - 1 {
                        print!(" and ");
                    } else {
                        print!(", ");
                    }
                }
                print!("{}", part);
            }
            println!();
        }

        if fix_count > 0 {
            println!("{}", format!("Applied {} fix(es)", fix_count).green().bold());
        } else if fixable_count > 0 {
            println!();
            println!("{}", format!("💡 Tip: Run with --fix to automatically apply {} fix(es)", fixable_count).cyan());
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
        let analysis_ms = analysis_time.as_secs_f64() * 1000.0;
        let total_ms = duration.as_secs_f64() * 1000.0;

        let bar_width = 50;
        let draw_timeline = |start_ms: f64, dur_ms: f64| -> String {
            let start_ratio = if total_ms > 0.0 { start_ms / total_ms } else { 0.0 };
            let width_ratio = if total_ms > 0.0 { dur_ms / total_ms } else { 0.0 };

            let start_idx = (start_ratio * bar_width as f64).round() as usize;
            let mut width = (width_ratio * bar_width as f64).round() as usize;

            // Ensure visible if duration is non-zero but small
            if width == 0 && width_ratio > 0.001 { width = 1; }

            let mut s = String::with_capacity(bar_width + 2);
            s.push('[');
            for i in 0..bar_width {
                if i >= start_idx && i < start_idx + width {
                    s.push('█');
                } else {
                    s.push(' ');
                }
            }
            s.push(']');
            s
        };

        // Relative start times
        let mut current_start = 0.0;

        println!("  Setup:     {:>6.2}ms {}", discover_ms, draw_timeline(current_start, discover_ms));
        current_start += discover_ms;

        println!("  Pipeline:  {:>6.2}ms {} (Discovery + Read)", read_ms, draw_timeline(current_start, read_ms));
        current_start += read_ms;

        println!("  Symbols:   {:>6.2}ms {}", symbol_ms, draw_timeline(current_start, symbol_ms));
        current_start += symbol_ms;

        // Breakdown analysis phase based on CPU contribution
        let (lex_cpu, parse_cpu, lint_cpu) = gml_linter::get_statistics_ms();
        let total_analysis_cpu = lex_cpu + parse_cpu + lint_cpu;

        if total_analysis_cpu > 0.0 {
             // Calculate derived wall-clock times
             let lex_wall = analysis_ms * (lex_cpu / total_analysis_cpu);
             let parse_wall = analysis_ms * (parse_cpu / total_analysis_cpu);
             let lint_wall = analysis_ms * (lint_cpu / total_analysis_cpu);

             println!("  Lexing:    {:>6.2}ms {}", lex_wall, draw_timeline(current_start, lex_wall));
             current_start += lex_wall;

             println!("  Parsing:   {:>6.2}ms {}", parse_wall, draw_timeline(current_start, parse_wall));
             current_start += parse_wall;

             println!("  Linting:   {:>6.2}ms {}", lint_wall, draw_timeline(current_start, lint_wall));
             // current_start += lint_wall; // Not strictly needed for last item but good for correctness
        } else {
             // Fallback if no CPU stats (e.g. 0 files?)
             println!("  Analysis:  {:>6.2}ms {}", analysis_ms, draw_timeline(current_start, analysis_ms));
        }
    }


    // Save cache to disk
    if !args.no_cache {
        // Remove files that are no longer present on disk
        // We use confirmed_files (what the walker found) for this.
        if !confirmed_files.is_empty() {
            // Prune files
            cache.prune_with_confirmed(&confirmed_files);

            // We should also prune stale directories from cache.directories?
            // This is harder since we don't have a 'confirmed_dirs' set.
            // But if a directory is deleted, we won't walk it, so it stays in cache until next time?
            // Actually, if we don't walk it, we don't confirm files in it.
            // But the directory entry itself remains.
            // We can prune directories whose children are empty or not in confirmed set?
            // For now, let's leave stale dirs, they don't hurt much (just memory).
        }
        let _ = cache.save(&project_root);
    }

    // Exit with error code if there were errors
    let errors = error_count.load(Ordering::Relaxed);
    if errors > 0 {
        std::process::exit(1);
    }

    Ok(())
}

/// Build a symbol provider by merging pre-extracted symbols from all files
fn build_symbol_provider_from_extracted(
    project_root: &Path, 
    file_data: &[FileData],
    cache: &Cache,
) -> ProjectSymbolProvider {
    let mut provider = ProjectSymbolProvider::new();
    
    // 1. Add resource names from .yyp (try cache first, then parse)
    if let Ok(project) = gml_linter::project::Project::new(project_root.to_path_buf()) {
        let resource_names = if let Some(cached) = cache.get_yyp_resources(&project.yyp_path) {
            cached
        } else {
            let names = project.get_resource_names();
            cache.set_yyp_resources(&project.yyp_path, names.clone());
            names
        };
        
        for name in resource_names {
            provider.functions.insert(name.clone());
            provider.variables.insert(name);
        }
    }
    
    // 2. Merge pre-extracted symbols from all files
    for (_path, _content, functions, variables, globals) in file_data {
        provider.functions.extend(functions.iter().cloned());
        provider.variables.extend(variables.iter().cloned());
        provider.globals.extend(globals.iter().cloned());
    }
    
    provider
}

/// Extract function names and macro names from GML source using the lexer
fn extract_symbols(source: &str, functions: &mut FxHashSet<String>, variables: &mut FxHashSet<String>, globals: &mut FxHashSet<String>) {
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
                    if let TokenKind::Identifier = &tokens[i + 1].kind {
                        let span = tokens[i + 1].span;
                        let name = &source[span.start as usize..span.end as usize];
                        functions.insert(name.to_string());
                    }
                }
            }
            TokenKind::Macro(def) => {
                functions.insert(def.name.to_string());
                if let Some(content) = def.body {
                    extract_symbols(content, functions, variables, globals);
                }
            }
            TokenKind::Enum => {
                // enum Name { ... }
                if i + 1 < tokens.len() {
                     if let TokenKind::Identifier = &tokens[i + 1].kind {
                         let span = tokens[i + 1].span;
                         let name = &source[span.start as usize..span.end as usize];
                         variables.insert(name.to_string());
                     }
                }
            }
            TokenKind::Globalvar => {
                // globalvar name1, name2, ...;
                let mut j = i + 1;
                while j < tokens.len() {
                    if let TokenKind::Identifier = &tokens[j].kind {
                        let span = tokens[j].span;
                        let name = &source[span.start as usize..span.end as usize];
                        globals.insert(name.to_string());
                        j += 1;
                        if j < tokens.len() && matches!(tokens[j].kind, TokenKind::Comma) {
                            j += 1;
                            continue;
                        }
                    }
                    break;
                }
            }
            TokenKind::Var | TokenKind::Static => {
                // var name1, name2 = value, ...;
                // static name = ...;
                let mut j = i + 1;
                while j < tokens.len() {
                    if let TokenKind::Identifier = &tokens[j].kind {
                        let span = tokens[j].span;
                        let name = &source[span.start as usize..span.end as usize];
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
            TokenKind::Identifier => {
                let span = tokens[i].span;
                let name = &source[span.start as usize..span.end as usize];

                // name = function(...) or name = method(...)
                if i + 1 < tokens.len() && matches!(tokens[i + 1].kind, TokenKind::Assign) {
                    if i + 2 < tokens.len() && matches!(tokens[i + 2].kind, TokenKind::Function) {
                        functions.insert(name.to_string());
                    } else {
                        variables.insert(name.to_string());
                    }
                }

                // global.name = ... (track global assignments)
                if name == "global" && i + 2 < tokens.len() && matches!(tokens[i + 1].kind, TokenKind::Dot) {
                    if let TokenKind::Identifier = &tokens[i + 2].kind {
                        let span = tokens[i + 2].span;
                        let member = &source[span.start as usize..span.end as usize];

                        // Check for any assignment operator
                        if i + 3 < tokens.len() {
                            let is_assign = matches!(tokens[i + 3].kind, 
                                TokenKind::Assign | TokenKind::PlusAssign | TokenKind::MinusAssign |
                                TokenKind::StarAssign | TokenKind::SlashAssign | TokenKind::PercentAssign |
                                TokenKind::BitAndAssign | TokenKind::BitOrAssign | TokenKind::BitXorAssign |
                                TokenKind::NullCoalesceAssign
                            );
                            if is_assign {
                                globals.insert(member.to_string());
                            }
                        }
                    }
                }

                // self.name = ... or other.name = ...
                if (matches!(name, "self" | "other")) && i + 2 < tokens.len() && matches!(tokens[i + 1].kind, TokenKind::Dot) {
                    if let TokenKind::Identifier = &tokens[i + 2].kind {
                        let span = tokens[i + 2].span;
                        let member = &source[span.start as usize..span.end as usize];

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

fn format_diagnostic_buffered(buffer: &mut String, path: &Path, diag: &Diagnostic, format: &str, content: &str) {
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

        if !content.is_empty() && diag.location.line > 0 {
            format_code_snippet(buffer, diag, content);
        }
    }
}

fn format_code_snippet(buffer: &mut String, diag: &Diagnostic, content: &str) {
    use std::fmt::Write;
    let line_idx = (diag.location.line - 1) as usize;

    if let Some(line_content) = content.lines().nth(line_idx) {
        let line_num_str = diag.location.line.to_string();
        let padding = " ".repeat(line_num_str.len());
        let gutter_color = colored::Color::Blue;

        let _ = writeln!(buffer, "{} {} ", padding.color(gutter_color), "|".color(gutter_color));
        let _ = writeln!(buffer, "{} {} {}", line_num_str.color(gutter_color), "|".color(gutter_color), line_content);

        if diag.location.column > 0 {
            let col_idx = (diag.location.column - 1) as usize;
            if col_idx < line_content.len() {
                let mut indent = String::new();
                for char in line_content.chars().take(col_idx) {
                    indent.push(if char == '\t' { '\t' } else { ' ' });
                }

                let span_len = if diag.location.end > diag.location.start {
                    (diag.location.end - diag.location.start) as usize
                } else { 1 };

                let remaining_len = line_content.len() - col_idx;
                let underline_len = std::cmp::min(span_len, remaining_len).max(1);

                let color = match diag.severity {
                    Severity::Error => colored::Color::Red,
                    Severity::Warning => colored::Color::Yellow,
                    Severity::Info => colored::Color::Blue,
                    Severity::Hint => colored::Color::Cyan,
                };

                let _ = writeln!(buffer, "{} {} {}{}",
                    padding.color(gutter_color), "|".color(gutter_color),
                    indent, "^".repeat(underline_len).color(color).bold()
                );
            }
        }
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

fn apply_fixes(path: &Path, mut edits: Vec<gml_linter::diagnostics::Edit>) -> Result<()> {
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

/// Read a file using memory-mapped I/O for better performance.
/// Falls back to regular read for small files (< 4KB) where mmap overhead isn't worth it.
fn read_file_fast(path: &Path) -> Option<String> {
    let file = File::open(path).ok()?;
    let metadata = file.metadata().ok()?;
    let len = metadata.len() as usize;
    
    // For small files, regular read is faster (mmap has fixed overhead)
    if len < 4096 {
        return std::fs::read_to_string(path).ok();
    }
    
    // Use memory-mapped I/O for larger files
    // SAFETY: We only read the file, and we're converting to owned String immediately
    let mmap = unsafe { Mmap::map(&file).ok()? };
    
    // Convert bytes to string (validates UTF-8)
    std::str::from_utf8(&mmap).ok().map(|s| s.to_string())
}
