use anyhow::Result;
use cc_history_lib::{archive_root_dir, unarchive_session, ChatReader};
use clap::Args;

use crate::models::ActionOutput;

#[derive(Debug, Args)]
pub struct UnarchiveArgs {
    /// Session ID to unarchive
    pub id: String,
}

pub fn execute(reader: &ChatReader, args: &UnarchiveArgs) -> Result<ActionOutput> {
    let archive_dir = archive_root_dir(reader.projects_dir());

    if !archive_dir.exists() {
        return Err(anyhow::anyhow!("No archive directory found"));
    }

    let archive_reader = ChatReader::with_projects_dir(archive_dir.clone());
    let (meta, _) = archive_reader.get_messages_by_id(&args.id)?;

    let result = unarchive_session(&meta.file, &archive_dir, reader.projects_dir())?;

    Ok(ActionOutput {
        action: "Unarchived".into(),
        id: args.id.clone(),
        original_path: result.original_path.display().to_string(),
        new_path: result.new_path.display().to_string(),
    })
}
