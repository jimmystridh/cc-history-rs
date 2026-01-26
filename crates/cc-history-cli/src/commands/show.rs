use anyhow::Result;
use cc_history_lib::{archive_root_dir, ChatReader};
use clap::Args;

use crate::models::{ConversationOutput, MessageItem};

#[derive(Debug, Args)]
pub struct ShowArgs {
    /// Session ID to show
    pub id: String,
}

pub fn execute(reader: &ChatReader, args: &ShowArgs) -> Result<ConversationOutput> {
    let result = reader.get_messages_by_id(&args.id);

    let (meta, messages) = match result {
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

    Ok(ConversationOutput {
        id: meta.id,
        path: meta.path,
        summary: meta.summary,
        messages: messages
            .into_iter()
            .map(|m| MessageItem {
                role: m.role,
                content: m.content,
            })
            .collect(),
    })
}
