use anyhow::Result;
use cc_history_lib::{archive_root_dir, ChatReader, ConversationMeta, SessionIndexCache};
use clap::Args;

use crate::models::{SearchOutput, SearchResultItem};

#[derive(Debug, Args)]
pub struct SearchArgs {
    /// Search query
    pub query: String,

    /// Include archived conversations
    #[arg(short, long)]
    pub archived: bool,
}

fn search_conversation(
    conv: &ConversationMeta,
    reader: &ChatReader,
    cache: &mut SessionIndexCache,
    query_lower: &str,
    archived: bool,
) -> Option<SearchResultItem> {
    let index_entry = cache.find_entry_for_session(&conv.file);
    let custom_title = conv.custom_title.clone().or_else(|| {
        index_entry.as_ref().and_then(|e| e.custom_title.clone())
    });
    let first_prompt = index_entry.as_ref().and_then(|e| e.first_prompt.clone());
    let git_branch = index_entry.as_ref().and_then(|e| e.git_branch.clone());

    let mut match_context = None;

    if let Some(ref title) = custom_title {
        if title.to_lowercase().contains(query_lower) {
            match_context = Some(truncate_around_match(title, query_lower, 80));
        }
    }

    if match_context.is_none() {
        if let Some(ref summary) = conv.summary {
            if summary.to_lowercase().contains(query_lower) {
                match_context = Some(truncate_around_match(summary, query_lower, 80));
            }
        }
    }

    if match_context.is_none() {
        if let Some(ref prompt) = first_prompt {
            if prompt.to_lowercase().contains(query_lower) {
                match_context = Some(truncate_around_match(prompt, query_lower, 80));
            }
        }
    }

    if match_context.is_none() {
        if let Ok((_, messages)) = reader.get_messages_by_id(&conv.id) {
            for msg in &messages {
                if msg.content.to_lowercase().contains(query_lower) {
                    match_context = Some(truncate_around_match(&msg.content, query_lower, 80));
                    break;
                }
            }
        }
    }

    match_context.map(|ctx| SearchResultItem {
        id: conv.id.clone(),
        path: conv.path.clone(),
        summary: conv.summary.clone(),
        custom_title,
        first_prompt,
        git_branch,
        match_context: Some(ctx),
        archived,
    })
}

pub fn execute(reader: &ChatReader, args: &SearchArgs) -> Result<SearchOutput> {
    let query_lower = args.query.to_lowercase();
    let mut results = Vec::new();
    let mut cache = SessionIndexCache::new();

    for conv in reader.list_conversations()? {
        if let Some(result) = search_conversation(&conv, reader, &mut cache, &query_lower, false) {
            results.push(result);
        }
    }

    if args.archived {
        let archive_dir = archive_root_dir(reader.projects_dir());
        if archive_dir.exists() {
            let archive_reader = ChatReader::with_projects_dir(archive_dir);
            for conv in archive_reader.list_conversations()? {
                if let Some(result) =
                    search_conversation(&conv, &archive_reader, &mut cache, &query_lower, true)
                {
                    results.push(result);
                }
            }
        }
    }

    let total = results.len();
    Ok(SearchOutput {
        query: args.query.clone(),
        total,
        results,
    })
}

fn truncate_around_match(text: &str, query: &str, max_len: usize) -> String {
    let text_lower = text.to_lowercase();
    let pos = match text_lower.find(query) {
        Some(p) => p,
        None => return text.chars().take(max_len).collect(),
    };

    let half = max_len / 2;
    let start = pos.saturating_sub(half);
    let chars: Vec<char> = text.chars().collect();

    let end = (start + max_len).min(chars.len());
    let actual_start = if start > 0 { start } else { 0 };

    let snippet: String = chars[actual_start..end].iter().collect();

    let mut result = String::new();
    if actual_start > 0 {
        result.push_str("...");
    }
    result.push_str(snippet.trim());
    if end < chars.len() {
        result.push_str("...");
    }
    result
}
