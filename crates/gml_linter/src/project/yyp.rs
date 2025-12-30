use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

#[derive(Debug, Deserialize, Serialize)]
pub struct Yyp {
    pub resources: Vec<YypResource>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct YypResource {
    pub id: YypResourceId,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct YypResourceId {
    pub name: String,
    pub path: String,
}

impl Yyp {
    pub fn from_file(path: &Path) -> anyhow::Result<Self> {
        let content = std::fs::read_to_string(path)?;
        // GMS2 .yyp files often contain trailing commas which standard JSON doesn't support.
        // We use regex to strip them: , followed by whitespace and } or ]
        let re = regex::Regex::new(r",(\s*[}\]])").unwrap();
        let content = re.replace_all(&content, "$1");
        
        let yyp: Yyp = serde_json::from_str(&content)?;
        Ok(yyp)
    }

    pub fn get_resource_paths(&self) -> Vec<PathBuf> {
        self.resources
            .iter()
            .map(|r| PathBuf::from(&r.id.path))
            .collect()
    }

    /// Get all resource names (script names are callable as functions)
    pub fn get_resource_names(&self) -> Vec<String> {
        self.resources
            .iter()
            .map(|r| r.id.name.clone())
            .collect()
    }
}
