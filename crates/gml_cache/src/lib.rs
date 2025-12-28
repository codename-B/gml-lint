//! GML Cache - File caching for incremental linting
//!
//! Stores file hashes and lint results to skip unchanged files.

use ahash::AHashMap;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

const CACHE_VERSION: u32 = 2;  // Bump version for new cache format
const CACHE_DIR: &str = ".gml-lint-cache";
const CACHE_FILE: &str = "cache.json";

/// Cached .yyp project data
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct YypCache {
    pub mtime: u64,
    pub resource_names: Vec<String>,
}

/// A cached file entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheEntry {
    /// Hash of the file contents
    pub content_hash: u64,
    /// Modification time (unix timestamp)
    pub mtime: u64,
    /// Number of diagnostics found
    pub diagnostic_count: usize,
    /// Whether the file had errors
    pub has_errors: bool,
}

/// The file cache
#[derive(Debug, Serialize, Deserialize)]
pub struct Cache {
    /// Cache format version
    version: u32,
    /// Cached entries by file path
    entries: AHashMap<PathBuf, CacheEntry>,
    /// Cached .yyp data
    #[serde(default)]
    yyp: Option<YypCache>,
}

impl Cache {
    pub fn new() -> Self {
        Self {
            version: CACHE_VERSION,
            entries: AHashMap::new(),
            yyp: None,
        }
    }

    /// Load cache from disk
    pub fn load(project_root: &Path) -> Self {
        let cache_path = project_root.join(CACHE_DIR).join(CACHE_FILE);
        
        if let Ok(content) = fs::read_to_string(&cache_path) {
            if let Ok(cache) = serde_json::from_str::<Cache>(&content) {
                if cache.version == CACHE_VERSION {
                    return cache;
                }
            }
        }
        
        Self::new()
    }

    /// Save cache to disk
    pub fn save(&self, project_root: &Path) -> std::io::Result<()> {
        let cache_dir = project_root.join(CACHE_DIR);
        fs::create_dir_all(&cache_dir)?;
        
        let cache_path = cache_dir.join(CACHE_FILE);
        let content = serde_json::to_string_pretty(self)?;
        fs::write(cache_path, content)
    }

    /// Get cached yyp resource names if the yyp file hasn't changed
    pub fn get_yyp_resources(&self, yyp_path: &Path) -> Option<&[String]> {
        let mtime = get_mtime(yyp_path)?;
        self.yyp.as_ref().filter(|c| c.mtime == mtime).map(|c| c.resource_names.as_slice())
    }

    /// Cache yyp resource names
    pub fn set_yyp_resources(&mut self, yyp_path: &Path, names: Vec<String>) {
        let mtime = get_mtime(yyp_path).unwrap_or(0);
        self.yyp = Some(YypCache { mtime, resource_names: names });
    }

    /// Check if a file is cached and unchanged using a pre-computed hash
    pub fn is_fresh_with_hash(&self, path: &Path, hash: u64) -> bool {
        if let Some(entry) = self.entries.get(path) {
            entry.content_hash == hash
        } else {
            false
        }
    }

    pub fn is_fresh(&self, path: &Path, content: &str) -> bool {
        self.entries.get(path).is_some_and(|entry| entry.content_hash == hash_content(content))
    }

    pub fn get(&self, path: &Path) -> Option<&CacheEntry> {
        self.entries.get(path)
    }

    pub fn update(&mut self, path: PathBuf, content: &str, diagnostic_count: usize, has_errors: bool) {
        self.entries.insert(path.clone(), CacheEntry {
            content_hash: hash_content(content),
            mtime: get_mtime(&path).unwrap_or(0),
            diagnostic_count,
            has_errors,
        });
    }

    /// Remove stale entries for files that no longer exist
    pub fn prune(&mut self) {
        self.entries.retain(|path, _| path.exists());
    }

    /// Clear all cache entries
    pub fn clear(&mut self) {
        self.entries.clear();
    }
}

impl Default for Cache {
    fn default() -> Self {
        Self::new()
    }
}

/// Compute a fast hash of file content
pub fn hash_content(content: &str) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = ahash::AHasher::default();
    content.hash(&mut hasher);
    hasher.finish()
}

/// Get file modification time as unix timestamp
fn get_mtime(path: &Path) -> Option<u64> {
    fs::metadata(path)
        .and_then(|m| m.modified())
        .ok()
        .and_then(|t| t.duration_since(SystemTime::UNIX_EPOCH).ok())
        .map(|d| d.as_secs())
}
