use anyhow::Result;
use cc_history_lib::{archive_root_dir, duplicate_session, ChatReader};
use clap::Args;

use crate::models::ActionOutput;

#[derive(Debug, Args)]
pub struct DuplicateArgs {
    /// Session ID to duplicate
    pub id: String,
}

pub fn execute(reader: &ChatReader, args: &DuplicateArgs) -> Result<ActionOutput> {
    let (meta, _) = match reader.get_messages_by_id(&args.id) {
        Ok(r) => r,
        Err(_) => {
            let archive_dir = archive_root_dir(reader.projects_dir());
            if archive_dir.exists() {
                let archive_reader = ChatReader::with_projects_dir(archive_dir);
                archive_reader.get_messages_by_id(&args.id)?
            } else {
                return Err(anyhow::anyhow!("Session not found: {}", args.id));
            }
        }
    };

    let result = duplicate_session(&meta.file, meta.summary.as_deref())?;

    Ok(ActionOutput {
        action: "Duplicated".into(),
        id: args.id.clone(),
        original_path: result.original_path.display().to_string(),
        new_path: result.new_path.display().to_string(),
    })
}
