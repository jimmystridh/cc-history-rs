use anyhow::Result;
use cc_history_lib::{archive_root_dir, ChatReader, ConversationMeta, SessionIndexCache};
use clap::Args;

use crate::models::{ConversationItem, ConversationListOutput};

#[derive(Debug, Args)]
pub struct ListArgs {
    /// Include archived conversations
    #[arg(short, long)]
    pub archived: bool,
}

fn enrich_with_index(conv: &ConversationMeta, cache: &mut SessionIndexCache, archived: bool) -> ConversationItem {
    let index_entry = cache.find_entry_for_session(&conv.file);

    ConversationItem {
        id: conv.id.clone(),
        path: conv.path.clone(),
        summary: conv.summary.clone(),
        custom_title: index_entry.as_ref().and_then(|e| e.custom_title.clone()),
        first_prompt: index_entry.as_ref().and_then(|e| e.first_prompt.clone()),
        git_branch: index_entry.as_ref().and_then(|e| e.git_branch.clone()),
        created_at: conv.created_at,
        archived,
    }
}

pub fn execute(reader: &ChatReader, args: &ListArgs) -> Result<ConversationListOutput> {
    let mut cache = SessionIndexCache::new();

    let mut conversations: Vec<ConversationItem> = reader
        .list_conversations()?
        .iter()
        .map(|c| enrich_with_index(c, &mut cache, false))
        .collect();

    if args.archived {
        let archive_dir = archive_root_dir(reader.projects_dir());
        if archive_dir.exists() {
            let archive_reader = ChatReader::with_projects_dir(archive_dir);
            let archived_convos: Vec<ConversationItem> = archive_reader
                .list_conversations()?
                .iter()
                .map(|c| enrich_with_index(c, &mut cache, true))
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
