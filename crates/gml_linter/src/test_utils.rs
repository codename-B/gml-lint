#[cfg(test)]
use crate::{LintContext, DefaultSymbolProvider};
use crate::checker::Checker;
use crate::parser::Parser;
use crate::semantic::db::Database;


#[cfg(test)]
pub fn assert_lint(rule: Box<dyn crate::Rule>, source: &str, expected_code: &str) {
    let parser = Parser::new(source);
    let program = match parser.parse() {
        Ok(p) => p,
        Err(e) => panic!("Parse errors in test source: {:?}", e),
    };
    
    let provider = DefaultSymbolProvider;
    let db = Database::new();
    let ctx = LintContext::new(source, "test.gml", &program, &provider, &db);
    let rules: Vec<Box<dyn crate::Rule>> = vec![rule];
    let checker = Checker::new(&ctx, &rules);
    let (_, diagnostics) = checker.check(&program);
    
    if expected_code.is_empty() {
        if !diagnostics.is_empty() {
            let codes: Vec<_> = diagnostics.iter().map(|d| d.code.clone()).collect();
            panic!("Expected no diagnostics, but found: {:?}", codes);
        }
    } else {
        let found = diagnostics.iter().any(|d| d.code == expected_code);
        if !found {
            let codes: Vec<_> = diagnostics.iter().map(|d| d.code.clone()).collect();
            panic!("Expected diagnostic code '{}', but found: {:?}", expected_code, codes);
        }
    }
}

/// Helper to test with a custom symbol provider
#[cfg(test)]
pub fn assert_lint_with_provider<P: crate::SymbolProvider>(
    rule: Box<dyn crate::Rule>,
    source: &str,
    provider: &P,
    expected_code: &str,
) {
    let parser = Parser::new(source);
    let program = match parser.parse() {
        Ok(p) => p,
        Err(e) => panic!("Parse errors in test source: {:?}", e),
    };
    
    let db = Database::new();
    let ctx = LintContext::new(source, "test.gml", &program, provider, &db);
    let rules: Vec<Box<dyn crate::Rule>> = vec![rule];
    let checker = Checker::new(&ctx, &rules);
    let (_, diagnostics) = checker.check(&program);
    
    if expected_code.is_empty() {
        if !diagnostics.is_empty() {
            let msgs: Vec<_> = diagnostics.iter().map(|d| format!("[{}] {}", d.code, d.message)).collect();
            panic!("Expected no diagnostics, but found: {:?}", msgs);
        }
    } else {
        let found = diagnostics.iter().any(|d| d.code == expected_code);
        if !found {
            let codes: Vec<_> = diagnostics.iter().map(|d| d.code.clone()).collect();
            panic!("Expected diagnostic code '{}', but found: {:?}", expected_code, codes);
        }
    }
}

#[cfg(test)]
pub fn assert_lint_fix(rule: Box<dyn crate::Rule>, source: &str, expected_fixed: &str) {
    let parser = Parser::new(source);
    let program = match parser.parse() {
        Ok(p) => p,
        Err(e) => panic!("Parse errors in test source: {:?}", e),
    };
    
    let provider = DefaultSymbolProvider;
    let db = Database::new();
    let ctx = LintContext::new(source, "test.gml", &program, &provider, &db);
    let rules: Vec<Box<dyn crate::Rule>> = vec![rule];
    let checker = Checker::new(&ctx, &rules);
    let (_, diagnostics) = checker.check(&program);
    
    let mut fix_edits = Vec::new();
    for diag in diagnostics {
        if let Some(fix) = diag.fix {
            fix_edits.extend(fix.edits);
        }
    }
    
    if fix_edits.is_empty() {
        if source != expected_fixed {
            panic!("No fixes found, but source doesn't match expected_fixed.\nSource: {:?}\nExpected: {:?}", source, expected_fixed);
        }
        return;
    }
    
    // Apply fixes
    let mut content = source.to_string();
    fix_edits.sort_by(|a, b| b.start.cmp(&a.start));
    
    // Filter out overlapping edits (poor man's conflict resolution)
    let mut last_start = content.len();
    for edit in fix_edits {
        let start = edit.start as usize;
        let end = edit.end as usize;
        if end <= last_start {
            content.replace_range(start..end, &edit.replacement);
            last_start = start;
        }
    }
    
    assert_eq!(content, expected_fixed, "Fixed source doesn't match expected output");
}

