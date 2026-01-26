//! cc-history-lib - Library for reading and managing Claude Code conversation history
//!
//! This library provides core functionality for working with the `.claude` folder
//! where Claude Code stores conversation history.
//!
//! # Example
//!
//! ```no_run
//! use cc_history_lib::{ChatReader, archive_session};
//!
//! let reader = ChatReader::new();
//! let conversations = reader.list_conversations().unwrap();
//!
//! for conv in &conversations {
//!     println!("{}: {:?}", conv.id, conv.summary);
//! }
//! ```

pub mod error;
pub mod paths;
pub mod reader;
pub mod writer;

pub use error::{HistoryError, Result};
pub use paths::{archive_root_dir, compute_relative_path, encode_path_to_dir_name};
pub use reader::{short_path, sort_conversations_by_earliest, ChatReader, ConversationMeta, Message};
pub use writer::{
    archive_session, duplicate_session, generate_uuid, rename_session, repath_session,
    unarchive_session, SessionAction,
};
