use std::fs::{self, File};
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};

use crate::error::{HistoryError, Result};
use crate::paths::{archive_root_dir, compute_relative_path, encode_path_to_dir_name};

/// Result of a session action, containing the paths involved.
#[derive(Debug, Clone)]
pub struct SessionAction {
    pub original_path: PathBuf,
    pub new_path: PathBuf,
}

/// Generate a new UUID v4 string.
pub fn generate_uuid() -> String {
    uuid::Uuid::new_v4().to_string()
}

/// Archive a session file by moving it to the archive directory.
/// The archive maintains the same relative directory structure as the projects directory.
pub fn archive_session(file: &Path, projects_dir: &Path) -> Result<SessionAction> {
    if !file.exists() {
        return Err(HistoryError::SessionNotFound(file.to_path_buf()));
    }

    let relative_path = compute_relative_path(file, projects_dir)?;
    let archive_root = archive_root_dir(projects_dir);
    let archived_path = archive_root.join(&relative_path);

    if archived_path.exists() {
        return Err(HistoryError::DestinationExists(archived_path));
    }

    if let Some(parent) = archived_path.parent() {
        fs::create_dir_all(parent)?;
    }

    fs::rename(file, &archived_path)?;

    Ok(SessionAction {
        original_path: file.to_path_buf(),
        new_path: archived_path,
    })
}

/// Restore an archived session back to the projects directory.
pub fn unarchive_session(
    archived_file: &Path,
    archive_dir: &Path,
    projects_dir: &Path,
) -> Result<SessionAction> {
    if !archived_file.exists() {
        return Err(HistoryError::SessionNotFound(archived_file.to_path_buf()));
    }

    let relative_path = compute_relative_path(archived_file, archive_dir)?;
    let restored_path = projects_dir.join(&relative_path);

    if restored_path.exists() {
        return Err(HistoryError::DestinationExists(restored_path));
    }

    if let Some(parent) = restored_path.parent() {
        fs::create_dir_all(parent)?;
    }

    fs::rename(archived_file, &restored_path)?;

    Ok(SessionAction {
        original_path: archived_file.to_path_buf(),
        new_path: restored_path,
    })
}

/// Rename a session by updating its summary line.
/// This prepends a new summary line and removes any existing summary lines.
pub fn rename_session(file: &Path, new_summary: &str) -> Result<SessionAction> {
    if !file.exists() {
        return Err(HistoryError::SessionNotFound(file.to_path_buf()));
    }

    let parent = file.parent().unwrap_or_else(|| Path::new("."));
    let temp_uuid = generate_uuid();
    let temp_path = parent.join(format!(".rename-{}.tmp", temp_uuid));

    {
        let source = File::open(file)?;
        let reader = BufReader::new(source);
        let mut dest = File::create(&temp_path)?;

        let summary_json = serde_json::json!({
            "type": "summary",
            "summary": new_summary
        });
        writeln!(dest, "{}", summary_json)?;

        for line in reader.lines() {
            let line = line?;
            if let Ok(v) = serde_json::from_str::<serde_json::Value>(&line) {
                if v.get("type").and_then(|x| x.as_str()) == Some("summary") {
                    continue;
                }
            }
            writeln!(dest, "{}", line)?;
        }
    }

    fs::rename(&temp_path, file)?;

    Ok(SessionAction {
        original_path: file.to_path_buf(),
        new_path: file.to_path_buf(),
    })
}

/// Move a session to a different project directory (change its cwd).
/// This updates the metadata in the file and moves it to the appropriate directory.
pub fn repath_session(file: &Path, new_cwd: &str, projects_dir: &Path) -> Result<SessionAction> {
    if !file.exists() {
        return Err(HistoryError::SessionNotFound(file.to_path_buf()));
    }

    let new_cwd = new_cwd.trim();
    if new_cwd.is_empty() {
        return Err(HistoryError::EmptyPath);
    }

    let file_name = file
        .file_name()
        .ok_or_else(|| HistoryError::InvalidPath(file.to_path_buf()))?;

    let new_dir_name = encode_path_to_dir_name(new_cwd);
    let new_dir = projects_dir.join(&new_dir_name);

    fs::create_dir_all(&new_dir)?;

    let new_file_path = new_dir.join(file_name);

    if new_file_path.exists() && new_file_path != file {
        return Err(HistoryError::DestinationExists(new_file_path));
    }

    if new_file_path == file {
        // Only update metadata, don't move file
        let parent = file.parent().unwrap_or_else(|| Path::new("."));
        let temp_uuid = generate_uuid();
        let temp_path = parent.join(format!(".repath-{}.tmp", temp_uuid));

        {
            let source = File::open(file)?;
            let reader = BufReader::new(source);
            let mut dest = File::create(&temp_path)?;

            let metadata_json = serde_json::json!({
                "type": "metadata",
                "cwd": new_cwd
            });
            writeln!(dest, "{}", metadata_json)?;

            for line in reader.lines() {
                let line = line?;
                if let Ok(v) = serde_json::from_str::<serde_json::Value>(&line) {
                    if v.get("type").and_then(|x| x.as_str()) == Some("metadata") {
                        if let Some(obj) = v.as_object() {
                            let has_only_path_fields = obj.keys().all(|k| {
                                matches!(k.as_str(), "type" | "cwd" | "workspace" | "path")
                            });
                            if has_only_path_fields {
                                continue;
                            }
                        }
                    }
                }
                writeln!(dest, "{}", line)?;
            }
        }

        fs::rename(&temp_path, file)?;

        Ok(SessionAction {
            original_path: file.to_path_buf(),
            new_path: file.to_path_buf(),
        })
    } else {
        // Move file to new directory with updated metadata
        let temp_uuid = generate_uuid();
        let temp_path = new_dir.join(format!(".repath-{}.tmp", temp_uuid));

        {
            let source = File::open(file)?;
            let reader = BufReader::new(source);
            let mut dest = File::create(&temp_path)?;

            let metadata_json = serde_json::json!({
                "type": "metadata",
                "cwd": new_cwd
            });
            writeln!(dest, "{}", metadata_json)?;

            for line in reader.lines() {
                let line = line?;
                if let Ok(v) = serde_json::from_str::<serde_json::Value>(&line) {
                    if v.get("type").and_then(|x| x.as_str()) == Some("metadata") {
                        if let Some(obj) = v.as_object() {
                            let has_only_path_fields = obj.keys().all(|k| {
                                matches!(k.as_str(), "type" | "cwd" | "workspace" | "path")
                            });
                            if has_only_path_fields {
                                continue;
                            }
                        }
                    }
                }
                writeln!(dest, "{}", line)?;
            }
        }

        fs::rename(&temp_path, &new_file_path)?;
        fs::remove_file(file)?;

        Ok(SessionAction {
            original_path: file.to_path_buf(),
            new_path: new_file_path,
        })
    }
}

/// Duplicate a session file with a new UUID and marked as a copy.
pub fn duplicate_session(file: &Path, original_summary: Option<&str>) -> Result<SessionAction> {
    if !file.exists() {
        return Err(HistoryError::SessionNotFound(file.to_path_buf()));
    }

    let new_id = generate_uuid();
    let extension = file
        .extension()
        .and_then(|s| s.to_str())
        .unwrap_or("jsonl");
    let parent = file.parent().unwrap_or_else(|| Path::new("."));
    let new_path = parent.join(format!("{}.{}", new_id, extension));

    if new_path.exists() {
        return Err(HistoryError::DestinationExists(new_path));
    }

    let original_id = file
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown");

    {
        let mut source = File::open(file)?;
        let mut dest = File::create(&new_path)?;

        let new_summary = match original_summary {
            Some(s) if !s.is_empty() => format!("(Copy) {}", s),
            _ => format!("(Copy) {}", original_id),
        };

        let summary_json = serde_json::json!({
            "type": "summary",
            "summary": new_summary
        });
        writeln!(dest, "{}", summary_json)?;

        std::io::copy(&mut source, &mut dest)?;

        let dup_msg = serde_json::json!({
            "type": "assistant",
            "message": {
                "role": "assistant",
                "content": format!("Session duplicated from {}", original_id)
            }
        });
        writeln!(dest, "{}", dup_msg)?;
    }

    Ok(SessionAction {
        original_path: file.to_path_buf(),
        new_path,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_session(dir: &Path, id: &str, content: &str) -> PathBuf {
        let path = dir.join(format!("{}.jsonl", id));
        let mut f = File::create(&path).unwrap();
        write!(f, "{}", content).unwrap();
        path
    }

    #[test]
    fn test_generate_uuid() {
        let uuid1 = generate_uuid();
        let uuid2 = generate_uuid();
        assert_ne!(uuid1, uuid2);
        assert_eq!(uuid1.len(), 36); // UUID format: 8-4-4-4-12
    }

    #[test]
    fn test_rename_session() {
        let tmp = TempDir::new().unwrap();
        let content = r#"{"type":"summary","summary":"Old summary"}
{"type":"user","message":{"role":"user","content":"Hello"}}
"#;
        let path = create_test_session(tmp.path(), "test-session", content);

        rename_session(&path, "New summary").unwrap();

        let new_content = fs::read_to_string(&path).unwrap();
        assert!(new_content.contains("New summary"));
        assert!(!new_content.contains("Old summary"));
        assert!(new_content.contains("Hello"));
    }

    #[test]
    fn test_duplicate_session() {
        let tmp = TempDir::new().unwrap();
        let content = r#"{"type":"user","message":{"role":"user","content":"Hello"}}
"#;
        let path = create_test_session(tmp.path(), "original-id", content);

        let result = duplicate_session(&path, Some("Test session")).unwrap();

        assert!(result.new_path.exists());
        assert_ne!(result.original_path, result.new_path);

        let new_content = fs::read_to_string(&result.new_path).unwrap();
        assert!(new_content.contains("(Copy) Test session"));
        assert!(new_content.contains("Hello"));
        assert!(new_content.contains("Session duplicated from original-id"));
    }

    #[test]
    fn test_archive_and_unarchive() {
        let tmp = TempDir::new().unwrap();
        let projects_dir = tmp.path().join("projects");
        let project_subdir = projects_dir.join("-test-project");
        fs::create_dir_all(&project_subdir).unwrap();

        let content = r#"{"type":"user","message":{"role":"user","content":"Hello"}}
"#;
        let path = create_test_session(&project_subdir, "test-session", content);

        // Archive
        let archive_result = archive_session(&path, &projects_dir).unwrap();
        assert!(!path.exists());
        assert!(archive_result.new_path.exists());

        // Unarchive
        let archive_dir = archive_root_dir(&projects_dir);
        let unarchive_result =
            unarchive_session(&archive_result.new_path, &archive_dir, &projects_dir).unwrap();
        assert!(unarchive_result.new_path.exists());
        assert!(!archive_result.new_path.exists());
    }
}
