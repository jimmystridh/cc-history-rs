use anyhow::Result;
use cc_history_lib::{archive_session, ChatReader};
use clap::Args;

use crate::models::ActionOutput;

#[derive(Debug, Args)]
pub struct ArchiveArgs {
    /// Session ID to archive
    pub id: String,
}

pub fn execute(reader: &ChatReader, args: &ArchiveArgs) -> Result<ActionOutput> {
    let (meta, _) = reader.get_messages_by_id(&args.id)?;

    let result = archive_session(&meta.file, reader.projects_dir())?;

    Ok(ActionOutput {
        action: "Archived".into(),
        id: args.id.clone(),
        original_path: result.original_path.display().to_string(),
        new_path: result.new_path.display().to_string(),
    })
}
