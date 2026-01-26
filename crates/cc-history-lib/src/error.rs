use std::path::PathBuf;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum HistoryError {
    #[error("Session not found: {0}")]
    SessionNotFound(PathBuf),

    #[error("Destination already exists: {0}")]
    DestinationExists(PathBuf),

    #[error("File is not in projects directory: {0}")]
    NotInProjectsDir(PathBuf),

    #[error("Invalid file path: {0}")]
    InvalidPath(PathBuf),

    #[error("New path cannot be empty")]
    EmptyPath,

    #[error(transparent)]
    Io(#[from] std::io::Error),

    #[error(transparent)]
    Json(#[from] serde_json::Error),
}

pub type Result<T> = std::result::Result<T, HistoryError>;
