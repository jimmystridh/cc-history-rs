use anyhow::Result;
use cc_history_lib::{archive_root_dir, ChatReader};
use clap::Args;

use crate::models::{ConversationItem, ConversationListOutput};

#[derive(Debug, Args)]
pub struct ListArgs {
    /// Include archived conversations
    #[arg(short, long)]
    pub archived: bool,
}

pub fn execute(reader: &ChatReader, args: &ListArgs) -> Result<ConversationListOutput> {
    let mut conversations: Vec<ConversationItem> = reader
        .list_conversations()?
        .into_iter()
        .map(|c| ConversationItem {
            id: c.id,
            path: c.path,
            summary: c.summary,
            created_at: c.created_at,
            archived: false,
        })
        .collect();

    if args.archived {
        let archive_dir = archive_root_dir(reader.projects_dir());
        if archive_dir.exists() {
            let archive_reader = ChatReader::with_projects_dir(archive_dir);
            let archived_convos: Vec<ConversationItem> = archive_reader
                .list_conversations()?
                .into_iter()
                .map(|c| ConversationItem {
                    id: c.id,
                    path: c.path,
                    summary: c.summary,
                    created_at: c.created_at,
                    archived: true,
                })
                .collect();
            conversations.extend(archived_convos);
        }
    }

    conversations.sort_by(|a, b| b.created_at.cmp(&a.created_at));

    let total = conversations.len();
    Ok(ConversationListOutput {
        total,
        conversations,
    })
}
