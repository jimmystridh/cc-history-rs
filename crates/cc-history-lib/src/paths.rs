use std::fs;
use std::path::{Path, PathBuf};

use crate::error::{HistoryError, Result};

/// Returns the archive directory path for the given projects directory.
/// The archive directory is a sibling of the projects directory named "projects-archive".
pub fn archive_root_dir(projects_dir: &Path) -> PathBuf {
    projects_dir.with_file_name("projects-archive")
}

/// Encode a filesystem path into Claude's project directory naming scheme.
/// e.g., "/Users/js/code/project" -> "-Users-js-code-project"
/// e.g., "/Users/js/code/visma.admin" -> "-Users-js-code-visma-admin"
/// Claude replaces both '/' and '.' with '-'
pub fn encode_path_to_dir_name(path: &str) -> String {
    path.replace(['/', '.'], "-")
}

/// Compute the relative path of a file within a projects directory.
/// Returns an error if the file is not within the projects directory.
pub fn compute_relative_path(source: &Path, projects_dir: &Path) -> Result<PathBuf> {
    if let Ok(rel) = source.strip_prefix(projects_dir) {
        return Ok(rel.to_path_buf());
    }

    let canonical_source = fs::canonicalize(source)?;
    let canonical_projects = fs::canonicalize(projects_dir)?;

    if let Ok(rel) = canonical_source.strip_prefix(&canonical_projects) {
        return Ok(rel.to_path_buf());
    }

    Err(HistoryError::NotInProjectsDir(source.to_path_buf()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode_path_to_dir_name() {
        assert_eq!(
            encode_path_to_dir_name("/Users/js/code/project"),
            "-Users-js-code-project"
        );
        assert_eq!(
            encode_path_to_dir_name("/Users/js/code/visma.admin"),
            "-Users-js-code-visma-admin"
        );
        assert_eq!(encode_path_to_dir_name("/"), "-");
        assert_eq!(encode_path_to_dir_name("foo.bar/baz"), "foo-bar-baz");
    }

    #[test]
    fn test_archive_root_dir() {
        let projects = PathBuf::from("/Users/js/.claude/projects");
        let archive = archive_root_dir(&projects);
        assert_eq!(archive, PathBuf::from("/Users/js/.claude/projects-archive"));
    }
}
