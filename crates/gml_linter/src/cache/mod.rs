//! GML Cache - File caching for incremental linting
//!
//! Stores file hashes and lint results to skip unchanged files.
//! Uses DashMap for lock-free concurrent access.

use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::RwLock;
use std::time::SystemTime;

const CACHE_VERSION: u32 = 4;  // Bump version for DashMap cache format
const CACHE_DIR: &str = ".gml-lint-cache";
const CACHE_FILE: &str = "cache.json";

/// Cached .yyp project data
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct YypCache {
    pub mtime: u64,
    pub resource_names: Vec<String>,
}

/// A cached directory entry for smart walking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirCacheEntry {
    /// Modification time of the directory
    pub mtime: u64,
    /// Direct child GML files
    pub files: Vec<PathBuf>,
    /// Direct child subdirectories
    pub subdirs: Vec<PathBuf>,
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

/// Serializable version of Cache for disk storage
#[derive(Debug, Serialize, Deserialize)]
struct SerializableCache {
    version: u32,
    entries: HashMap<PathBuf, CacheEntry>,
    directories: HashMap<PathBuf, DirCacheEntry>,
    yyp: Option<YypCache>,
}

/// The file cache - uses DashMap for lock-free concurrent access
pub struct Cache {
    /// Cache format version
    version: u32,
    /// Cached entries by file path (lock-free concurrent access)
    entries: DashMap<PathBuf, CacheEntry>,
    /// Cached directory structures (lock-free concurrent access)
    pub directories: DashMap<PathBuf, DirCacheEntry>,
    /// Cached .yyp data (needs RwLock since it's a single value)
    yyp: RwLock<Option<YypCache>>,
}

impl Cache {
    pub fn new() -> Self {
        Self {
            version: CACHE_VERSION,
            entries: DashMap::new(),
            directories: DashMap::new(),
            yyp: RwLock::new(None),
        }
    }

    /// Load cache from disk
    pub fn load(project_root: &Path) -> Self {
        let cache_path = project_root.join(CACHE_DIR).join(CACHE_FILE);
        
        if let Ok(content) = fs::read_to_string(&cache_path) {
            if let Ok(serializable) = serde_json::from_str::<SerializableCache>(&content) {
                if serializable.version == CACHE_VERSION {
                    // Convert from serializable to DashMap-based Cache
                    let cache = Cache::new();
                    for (k, v) in serializable.entries {
                        cache.entries.insert(k, v);
                    }
                    for (k, v) in serializable.directories {
                        cache.directories.insert(k, v);
                    }
                    *cache.yyp.write().unwrap() = serializable.yyp;
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
        
        // Convert to serializable format
        let serializable = SerializableCache {
            version: self.version,
            entries: self.entries.iter().map(|r| (r.key().clone(), r.value().clone())).collect(),
            directories: self.directories.iter().map(|r| (r.key().clone(), r.value().clone())).collect(),
            yyp: self.yyp.read().unwrap().clone(),
        };
        
        let cache_path = cache_dir.join(CACHE_FILE);
        let content = serde_json::to_string_pretty(&serializable)?;
        fs::write(cache_path, content)
    }

    /// Get cached yyp resource names if the yyp file hasn't changed
    pub fn get_yyp_resources(&self, yyp_path: &Path) -> Option<Vec<String>> {
        let mtime = get_mtime(yyp_path)?;
        let guard = self.yyp.read().unwrap();
        guard.as_ref()
            .filter(|c| c.mtime == mtime)
            .map(|c| c.resource_names.clone())
    }

    /// Cache yyp resource names
    pub fn set_yyp_resources(&self, yyp_path: &Path, names: Vec<String>) {
        let mtime = get_mtime(yyp_path).unwrap_or(0);
        *self.yyp.write().unwrap() = Some(YypCache { mtime, resource_names: names });
    }

    /// Check if a file is cached and unchanged using a pre-computed hash
    pub fn is_fresh_with_hash(&self, path: &Path, hash: u64) -> bool {
        self.entries.get(path).is_some_and(|entry| entry.content_hash == hash)
    }

    pub fn is_fresh(&self, path: &Path, content: &str) -> bool {
        self.entries.get(path).is_some_and(|entry| entry.content_hash == hash_content(content))
    }

    /// Get a cached entry (returns a clone since DashMap doesn't allow returning references)
    pub fn get(&self, path: &Path) -> Option<CacheEntry> {
        self.entries.get(path).map(|r| r.value().clone())
    }

    /// Update a file's cache entry
    pub fn update(&self, path: PathBuf, content: &str, diagnostic_count: usize, has_errors: bool) {
        self.entries.insert(path.clone(), CacheEntry {
            content_hash: hash_content(content),
            mtime: get_mtime(&path).unwrap_or(0),
            diagnostic_count,
            has_errors,
        });
    }

    /// Remove stale entries for files that no longer exist
    pub fn prune(&self) {
        self.entries.retain(|path, _| path.exists());
    }

    /// Remove stale entries based on a set of confirmed files
    pub fn prune_with_confirmed(&self, confirmed: &dashmap::DashSet<PathBuf>) {
        self.entries.retain(|path, _| confirmed.contains(path));
    }

    /// Get all cached file paths
    pub fn get_files(&self) -> Vec<PathBuf> {
        self.entries.iter().map(|r| r.key().clone()).collect()
    }

    /// Clear all cache entries
    pub fn clear(&self) {
        self.entries.clear();
        self.directories.clear();
        *self.yyp.write().unwrap() = None;
    }
}

impl Default for Cache {
    fn default() -> Self {
        Self::new()
    }
}

// Implement Debug manually since DashMap's Debug is fine
impl std::fmt::Debug for Cache {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Cache")
            .field("version", &self.version)
            .field("entries_count", &self.entries.len())
            .field("directories_count", &self.directories.len())
            .finish()
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
pub fn get_mtime(path: &Path) -> Option<u64> {
    fs::metadata(path)
        .and_then(|m| m.modified())
        .ok()
        .and_then(|t| t.duration_since(SystemTime::UNIX_EPOCH).ok())
        .map(|d| d.as_secs())
}
