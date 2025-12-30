use std::path::{Path, PathBuf};
use jwalk::WalkDir;
use super::Yyp;

pub struct Project {
    pub root: PathBuf,
    pub yyp_path: PathBuf,
}

#[derive(Debug)]
pub enum ProjectIssue {
    OrphanedFile(PathBuf),
    MissingResource(PathBuf),
}

impl Project {
    pub fn new(root: PathBuf) -> anyhow::Result<Self> {
        // Find .yyp in root
        let yyp_path = std::fs::read_dir(&root)?
            .filter_map(|e| e.ok())
            .find(|e| e.path().extension().is_some_and(|ext| ext == "yyp"))
            .map(|e| e.path())
            .ok_or_else(|| anyhow::anyhow!("No .yyp file found in {:?}", root))?;

        Ok(Self { root, yyp_path })
    }

    /// Get all resource names (scripts, objects, rooms, etc.)
    pub fn get_resource_names(&self) -> Vec<String> {
        match Yyp::from_file(&self.yyp_path) {
            Ok(yyp) => yyp.get_resource_names(),
            Err(_) => Vec::new(),
        }
    }

    /// Get all script/resource names that can be called as functions
    pub fn get_script_names(&self) -> Vec<String> {
        self.get_resource_names()
    }

    pub fn check_health(&self) -> Vec<ProjectIssue> {
        let mut issues = Vec::new();

        let yyp = match Yyp::from_file(&self.yyp_path) {
            Ok(y) => y,
            Err(_) => return issues,
        };

        let resources = yyp.get_resource_paths();
        
        // 1. Check for missing resources
        let mut registered_files = std::collections::HashSet::new();
        for rel_path in resources {
            let full_path = self.root.join(&rel_path);
            if !full_path.exists() {
                issues.push(ProjectIssue::MissingResource(rel_path.clone()));
            }
            registered_files.insert(rel_path);
        }

        // 2. Check for orphaned files
        let dirs_to_check = ["scripts", "objects", "sprites", "rooms", "sounds", "fonts", "tilesets", "notes", "paths", "shaders", "timelines"];
        
        for dir_name in dirs_to_check {
            let dir_path = self.root.join(dir_name);
            if !dir_path.exists() { continue; }

            for entry in WalkDir::new(&dir_path).into_iter().flatten() {
                let path = entry.path();
                if path.is_file() {
                    if let Some(ext) = path.extension() {
                        if ext == "yy" {
                            let rel_path = path.strip_prefix(&self.root)
                                .map(|p| p.to_path_buf())
                                .unwrap_or_else(|_| path.clone());
                            if !registered_files.contains(Path::new(&rel_path)) {
                                issues.push(ProjectIssue::OrphanedFile(rel_path));
                            }
                        }
                    }
                }
            }
        }

        issues
    }
    pub fn scan_macros(&self) -> Vec<String> {
        let mut macros = Vec::new();
        let re = regex::Regex::new(r"(?m)^\s*#macro\s+([a-zA-Z0-9_]+)\s+").unwrap();

        for entry in WalkDir::new(&self.root).into_iter().flatten() {
            let path = entry.path();
            if path.is_file() {
                if let Some(ext) = path.extension() {
                    if ext == "gml" {
                        if let Ok(content) = std::fs::read_to_string(&path) {
                            for cap in re.captures_iter(&content) {
                                macros.push(cap[1].to_string());
                            }
                        }
                    }
                }
            }
        }
        macros
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use std::io::Write;
    use tempfile::TempDir;

    #[test]
    fn test_scan_macros() {
        // Create temp project structure
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path().to_path_buf();
        
        // Create a dummy .yyp file
        let yyp_path = root.join("project.yyp");
        let mut yyp_file = File::create(&yyp_path).unwrap();
        writeln!(yyp_file, "{{ \"resources\": [] }}").unwrap();

        // Create a script with macros
        let script_dir = root.join("scripts");
        std::fs::create_dir(&script_dir).unwrap();
        let script_path = script_dir.join("macros.gml");
        let mut script_file = File::create(&script_path).unwrap();
        writeln!(script_file, "#macro MY_MACRO 123").unwrap();
        writeln!(script_file, "    #macro INDENTED_MACRO \"abc\"").unwrap();
        writeln!(script_file, "text #macro INVALID_MACRO 456").unwrap(); // Should be ignored

        let project = Project::new(root).unwrap();
        let macros = project.scan_macros();

        assert!(macros.contains(&"MY_MACRO".to_string()));
        assert!(macros.contains(&"INDENTED_MACRO".to_string()));
        assert!(!macros.contains(&"INVALID_MACRO".to_string()));
    }
}
