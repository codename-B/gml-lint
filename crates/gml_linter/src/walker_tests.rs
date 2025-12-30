#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::sync::{Arc, Mutex};
    use tempfile::TempDir;
    use crossbeam_channel::unbounded;
    use dashmap::DashSet;
    use crate::cache::Cache;
    use crate::walker::SmartWalker;

    fn create_test_files(dir: &Path, files: &[&str]) {
        for file in files {
            let path = dir.join(file);
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent).unwrap();
            }
            fs::write(path, "").unwrap();
        }
    }

    #[test]
    fn test_walker_discovers_gml_files() {
        let temp_dir = TempDir::new().unwrap();
        create_test_files(temp_dir.path(), &["a.gml", "b.gml", "sub/c.gml", "ignore.txt"]);

        let (tx, rx) = unbounded();
        let seen = Arc::new(DashSet::new());
        let confirmed = Arc::new(DashSet::new());
        let cache = Arc::new(Mutex::new(Cache::new()));

        let walker = SmartWalker::new(tx, seen, confirmed, None, cache);
        walker.run(&[temp_dir.path().to_path_buf()]);

        let mut files: Vec<PathBuf> = rx.try_iter().collect();
        files.sort();

        assert_eq!(files.len(), 3);
        assert!(files.iter().any(|p| p.ends_with("a.gml")));
        assert!(files.iter().any(|p| p.ends_with("b.gml")));
        assert!(files.iter().any(|p| p.ends_with("c.gml")));
    }

    #[test]
    fn test_walker_excludes_files() {
        let temp_dir = TempDir::new().unwrap();
        create_test_files(temp_dir.path(), &["a.gml", "exclude/b.gml", "c.gml"]);

        let (tx, rx) = unbounded();
        let seen = Arc::new(DashSet::new());
        let confirmed = Arc::new(DashSet::new());
        let cache = Arc::new(Mutex::new(Cache::new()));

        let exclude = Some(vec!["exclude".to_string()]);
        let walker = SmartWalker::new(tx, seen, confirmed, exclude, cache);
        walker.run(&[temp_dir.path().to_path_buf()]);

        let mut files: Vec<PathBuf> = rx.try_iter().collect();
        files.sort();

        assert_eq!(files.len(), 2);
        assert!(!files.iter().any(|p| p.ends_with("b.gml")));
    }

    #[test]
    fn test_walker_excludes_double_star() {
        let temp_dir = TempDir::new().unwrap();
        create_test_files(temp_dir.path(), &["src/a.gml", "node_modules/pkg/b.gml", "src/c.gml"]);

        let (tx, rx) = unbounded();
        let seen = Arc::new(DashSet::new());
        let confirmed = Arc::new(DashSet::new());
        let cache = Arc::new(Mutex::new(Cache::new()));

        // Test with **/node_modules/** pattern
        let exclude = Some(vec!["**/node_modules/**".to_string()]);
        let walker = SmartWalker::new(tx, seen, confirmed, exclude, cache);
        walker.run(&[temp_dir.path().to_path_buf()]);

        let mut files: Vec<PathBuf> = rx.try_iter().collect();
        files.sort();

        assert_eq!(files.len(), 2);
        assert!(files.iter().any(|p| p.ends_with("a.gml")));
        assert!(files.iter().any(|p| p.ends_with("c.gml")));
        assert!(!files.iter().any(|p| p.ends_with("b.gml")));
    }

    #[test]
    fn test_walker_caches_directories() {
        let temp_dir = TempDir::new().unwrap();
        create_test_files(temp_dir.path(), &["a.gml"]);

        let seen = Arc::new(DashSet::new());
        let confirmed = Arc::new(DashSet::new());
        let cache = Arc::new(Mutex::new(Cache::new()));

        // First run
        let (tx1, _) = unbounded();
        let walker1 = SmartWalker::new(tx1, seen.clone(), confirmed.clone(), None, cache.clone());
        walker1.run(&[temp_dir.path().to_path_buf()]);

        // Check cache populated
        {
            let guard = cache.lock().unwrap();
            assert!(!guard.directories.is_empty());
        }

        // Second run (should use cache)
        let (tx2, rx2) = unbounded();
        // Reset seen so we get emissions again (walker checks seen)
        seen.clear();

        let walker2 = SmartWalker::new(tx2, seen, confirmed, None, cache.clone());
        walker2.run(&[temp_dir.path().to_path_buf()]);

        let files: Vec<PathBuf> = rx2.try_iter().collect();
        assert_eq!(files.len(), 1);
        assert!(files[0].ends_with("a.gml"));
    }

    #[test]
    #[ignore]
    fn benchmark_is_excluded() {
        let (tx, _) = unbounded();
        let seen = Arc::new(DashSet::new());
        let confirmed = Arc::new(DashSet::new());
        let cache = Arc::new(Mutex::new(Cache::new()));

        // Typical exclude patterns
        let patterns = vec![
            "**/node_modules/**".to_string(),
            "target/**".to_string(),
            "**/*.bak".to_string(),
            "scripts/generated/**".to_string(),
            "**/.git/**".to_string(),
            "dist/**".to_string(),
        ];

        let walker = SmartWalker::new(tx, seen, confirmed, Some(patterns), cache);

        let path = PathBuf::from("scripts/generated/my_script.gml");
        let path2 = PathBuf::from("objects/obj_player.gml");

        let start = std::time::Instant::now();
        for _ in 0..100_000 {
            assert!(walker.is_excluded(&path));
            assert!(!walker.is_excluded(&path2));
        }
        println!("is_excluded 200k times: {:?}", start.elapsed());
    }
}
