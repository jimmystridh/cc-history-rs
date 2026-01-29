use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::path::{Path, PathBuf};

use crate::error::Result;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionIndexEntry {
    pub session_id: String,
    pub full_path: String,
    #[serde(default)]
    pub file_mtime: Option<i64>,
    #[serde(default)]
    pub first_prompt: Option<String>,
    #[serde(default)]
    pub summary: Option<String>,
    #[serde(default)]
    pub custom_title: Option<String>,
    #[serde(default)]
    pub message_count: Option<u32>,
    #[serde(default)]
    pub created: Option<String>,
    #[serde(default)]
    pub modified: Option<String>,
    #[serde(default)]
    pub git_branch: Option<String>,
    #[serde(default)]
    pub project_path: Option<String>,
    #[serde(default)]
    pub is_sidechain: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionIndex {
    pub version: u32,
    pub entries: Vec<SessionIndexEntry>,
    #[serde(default)]
    pub original_path: Option<String>,
}

impl SessionIndex {
    pub fn load(path: &Path) -> Result<Self> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);
        let index: SessionIndex = serde_json::from_reader(reader)?;
        Ok(index)
    }

    pub fn save(&self, path: &Path) -> Result<()> {
        let file = File::create(path)?;
        let writer = BufWriter::new(file);
        serde_json::to_writer_pretty(writer, self)?;
        Ok(())
    }

    pub fn find_entry(&self, session_id: &str) -> Option<&SessionIndexEntry> {
        self.entries.iter().find(|e| e.session_id == session_id)
    }

    pub fn find_entry_mut(&mut self, session_id: &str) -> Option<&mut SessionIndexEntry> {
        self.entries.iter_mut().find(|e| e.session_id == session_id)
    }
}

pub struct SessionIndexCache {
    indexes: HashMap<PathBuf, SessionIndex>,
}

impl SessionIndexCache {
    pub fn new() -> Self {
        Self {
            indexes: HashMap::new(),
        }
    }

    pub fn load_for_project(&mut self, project_dir: &Path) -> Option<&SessionIndex> {
        let index_path = project_dir.join("sessions-index.json");
        if !index_path.exists() {
            return None;
        }

        if !self.indexes.contains_key(project_dir) {
            if let Ok(index) = SessionIndex::load(&index_path) {
                self.indexes.insert(project_dir.to_path_buf(), index);
            }
        }

        self.indexes.get(project_dir)
    }

    pub fn find_entry_for_session(&mut self, session_file: &Path) -> Option<SessionIndexEntry> {
        let project_dir = session_file.parent()?;
        let session_id = session_file.file_stem()?.to_str()?;

        let index = self.load_for_project(project_dir)?;
        index.find_entry(session_id).cloned()
    }
}

impl Default for SessionIndexCache {
    fn default() -> Self {
        Self::new()
    }
}

pub fn update_custom_title(session_file: &Path, new_title: &str) -> Result<()> {
    let project_dir = session_file
        .parent()
        .ok_or_else(|| crate::error::HistoryError::InvalidPath(session_file.to_path_buf()))?;
    let session_id = session_file
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or_else(|| crate::error::HistoryError::InvalidPath(session_file.to_path_buf()))?;

    let index_path = project_dir.join("sessions-index.json");

    if !index_path.exists() {
        return Ok(());
    }

    let mut index = SessionIndex::load(&index_path)?;

    if let Some(entry) = index.find_entry_mut(session_id) {
        entry.custom_title = Some(new_title.to_string());
    } else {
        index.entries.push(SessionIndexEntry {
            session_id: session_id.to_string(),
            full_path: session_file.to_string_lossy().to_string(),
            custom_title: Some(new_title.to_string()),
            file_mtime: None,
            first_prompt: None,
            summary: None,
            message_count: None,
            created: None,
            modified: None,
            git_branch: None,
            project_path: None,
            is_sidechain: None,
        });
    }

    index.save(&index_path)?;
    Ok(())
}

pub fn index_path_for_session(session_file: &Path) -> Option<PathBuf> {
    session_file
        .parent()
        .map(|p| p.join("sessions-index.json"))
}
