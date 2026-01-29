use anyhow::Result;
use cc_history_lib::{archive_root_dir, rename_session, update_custom_title, ChatReader};
use clap::Args;

use crate::models::ActionOutput;

#[derive(Debug, Args)]
pub struct RenameArgs {
    /// Session ID to rename
    pub id: String,

    /// New title/name for the session
    pub name: String,
}

pub fn execute(reader: &ChatReader, args: &RenameArgs) -> Result<ActionOutput> {
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

    let result = rename_session(&meta.file, &args.name)?;

    if let Err(e) = update_custom_title(&meta.file, &args.name) {
        eprintln!("Warning: Could not update sessions-index.json: {}", e);
    }

    Ok(ActionOutput {
        action: "Renamed".into(),
        id: args.id.clone(),
        original_path: result.original_path.display().to_string(),
        new_path: result.new_path.display().to_string(),
    })
}
