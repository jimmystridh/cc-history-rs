use anyhow::Result;
use cc_history_lib::{archive_root_dir, repath_session, ChatReader};
use clap::Args;

use crate::models::ActionOutput;

#[derive(Debug, Args)]
pub struct RepathArgs {
    /// Session ID to repath
    pub id: String,

    /// New project path
    pub path: String,
}

pub fn execute(reader: &ChatReader, args: &RepathArgs) -> Result<ActionOutput> {
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

    let result = repath_session(&meta.file, &args.path, reader.projects_dir())?;

    Ok(ActionOutput {
        action: "Repathed".into(),
        id: args.id.clone(),
        original_path: result.original_path.display().to_string(),
        new_path: result.new_path.display().to_string(),
    })
}
