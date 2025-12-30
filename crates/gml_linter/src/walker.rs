use std::path::{Path, PathBuf};
use std::sync::Arc;
use crossbeam_channel::Sender;
use crate::cache::{Cache, DirCacheEntry, get_mtime};
use dashmap::DashSet;
use rayon::prelude::*;

/// SmartWalker that utilizes cache to skip walking unchanged directories.
///
/// It recursively walks the directory tree. For each directory:
/// 1. Checks mtime against cache.
/// 2. If mtime matches (Fast Path): emits cached files and recurses into cached subdirs.
/// 3. If mtime differs (Slow Path): reads directory, updates cache, emits files, and recurses.
pub struct SmartWalker {
    tx: Sender<PathBuf>,
    seen: Arc<DashSet<PathBuf>>,
    confirmed: Arc<DashSet<PathBuf>>,
    exclude_patterns: Option<Vec<String>>,
    cache: Arc<Cache>,  // DashMap-based Cache - no Mutex needed!
}

impl SmartWalker {
    pub fn new(
        tx: Sender<PathBuf>,
        seen: Arc<DashSet<PathBuf>>,
        confirmed: Arc<DashSet<PathBuf>>,
        exclude_patterns: Option<Vec<String>>,
        cache: Arc<Cache>,
    ) -> Self {
        // Normalize patterns on creation to avoid re-normalizing in loop
        let exclude_patterns = exclude_patterns.map(|patterns| {
            patterns
                .into_iter()
                .map(|p| p.replace('\\', "/"))
                .collect()
        });

        Self {
            tx,
            seen,
            confirmed,
            exclude_patterns,
            cache,
        }
    }

    pub fn run(&self, paths: &[PathBuf]) {
        // Parallel processing of top-level paths
        paths.par_iter().for_each(|path| {
            if path.is_file() {
                self.handle_file(path);
            } else if path.is_dir() {
                self.walk_dir(path, true);
            }
        });
    }

    fn handle_file(&self, path: &Path) {
        if path.extension().is_some_and(|ext| ext == "gml")
            && !self.is_excluded(path)
        {
            self.confirmed.insert(path.to_path_buf());
            if self.seen.insert(path.to_path_buf()) {
                let _ = self.tx.send(path.to_path_buf());
            }
        }
    }

    fn walk_dir(&self, dir: &Path, is_root: bool) {
        if !is_root {
            // Hard ignore check (node_modules, target, etc)
            if let Some(name) = dir.file_name().and_then(|n| n.to_str()) {
                if name == "node_modules" || name == "target" || name.starts_with('.') {
                    return;
                }
            }
        }

        // Exclude check (skip recursion if dir matches exclude pattern)
        if self.is_excluded(dir) {
            return;
        }

        let current_mtime = get_mtime(dir);
        let cached_entry: Option<DirCacheEntry>;

        // Fast Path Check
        if let Some(mtime) = current_mtime {
            if let Some(entry) = self.cache.directories.get(dir) {
                if entry.mtime == mtime {
                    cached_entry = Some(entry.clone());
                } else {
                    cached_entry = None;
                }
            } else {
                cached_entry = None;
            }
        } else {
            cached_entry = None;
        }

        if let Some(entry) = cached_entry {
            // FAST PATH: Use cached structure
            for file_path in &entry.files {
                self.handle_file(file_path);
            }

            // Parallel recursion into subdirs
            entry.subdirs.par_iter().for_each(|subdir| {
                self.walk_dir(subdir, false);
            });
        } else {
            // Read directory (cache miss)
            if let Ok(read_dir) = std::fs::read_dir(dir) {
                let mut files = Vec::new();
                let mut subdirs = Vec::new();

                for entry in read_dir.flatten() {
                    let path = entry.path();
                    let file_name = entry.file_name();
                    let name_str = file_name.to_string_lossy();

                    if name_str == "node_modules" || name_str == "target" || name_str.starts_with('.') {
                        continue;
                    }

                    if path.is_dir() {
                        subdirs.push(path.clone());
                    } else if path.is_file() && path.extension().is_some_and(|ext| ext == "gml") {
                        files.push(path.clone());
                        self.handle_file(&path);
                    }
                }

                // Update Cache
                if let Some(mtime) = current_mtime {
                    self.cache.directories.insert(dir.to_path_buf(), DirCacheEntry {
                        mtime,
                        files: files.clone(),
                        subdirs: subdirs.clone(),
                    });
                }

                // Parallel recursion into subdirs
                subdirs.par_iter().for_each(|subdir| {
                    self.walk_dir(subdir, false);
                });
            }
        }
    }

    fn is_excluded(&self, path: &Path) -> bool {
        let Some(patterns) = &self.exclude_patterns else {
            return false;
        };

        let path_str_cow = path.to_string_lossy();
        if path_str_cow.is_empty() {
             return false;
        }

        // Optimization: Normalize path only if it contains backslashes.
        // If no backslashes, we can use the Cow directly (whether owned or borrowed).
        // If backslashes, we must allocate a new string.
        let path_ref = if path_str_cow.contains('\\') {
             std::borrow::Cow::Owned(path_str_cow.replace('\\', "/"))
        } else {
             path_str_cow
        };

        for pattern in patterns {
            if matches_glob_pattern(&path_ref, pattern) {
                return true;
            }
        }

        false
    }
}

fn matches_glob_pattern(path: &str, pattern: &str) -> bool {
    // pattern and path are assumed to be normalized (forward slashes)

    // Using a labelled block to handle the double-star logic
    // If the pattern is "too complex" (e.g. multiple **), we break out and fall through
    // to the simple substring match (which was the original behavior).
    'double_star_check: {
        if let Some(double_star_pos) = pattern.find("**") {
             let prefix = &pattern[..double_star_pos];
             let suffix = &pattern[double_star_pos + 2..];

             if suffix.contains("**") {
                 // Too complex, fall through to simpler check
                 break 'double_star_check;
             }

             // Optimized single ** check
             let prefix = prefix.trim_end_matches('/');
             let suffix = suffix.trim_start_matches('/');

             if !prefix.is_empty() && !path.contains(prefix) { return false; }
             if !suffix.is_empty() && !path.contains(suffix) { return false; }

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

    if pattern.contains('*') {
        let clean_pattern = pattern.trim_matches('*');
        return path.contains(clean_pattern);
    }

    path.contains(pattern)
}

#[cfg(test)]
#[path = "walker_tests.rs"]
mod tests;
