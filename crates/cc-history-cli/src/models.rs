use anyhow::Result;
use cc_history_lib::short_path;
use serde::Serialize;

use crate::output::Formattable;

#[derive(Debug, Clone, Serialize)]
pub struct ConversationItem {
    pub id: String,
    pub path: Option<String>,
    pub summary: Option<String>,
    pub created_at: Option<i64>,
    pub archived: bool,
}

#[derive(Debug, Clone, Serialize)]
pub struct ConversationListOutput {
    pub total: usize,
    pub conversations: Vec<ConversationItem>,
}

impl Formattable for ConversationListOutput {
    fn format_markdown(&self) -> Result<String> {
        let mut out = String::new();
        out.push_str(&format!("# Conversations ({})\n\n", self.total));

        for conv in &self.conversations {
            let path_display = conv
                .path
                .as_ref()
                .map(|p| short_path(p))
                .unwrap_or_else(|| "(no path)".into());

            let summary_display = conv
                .summary
                .as_ref()
                .map(|s| {
                    let truncated: String = s.chars().take(60).collect();
                    if s.len() > 60 {
                        format!("{}...", truncated)
                    } else {
                        truncated
                    }
                })
                .unwrap_or_else(|| "(no summary)".into());

            let archived_marker = if conv.archived { " [archived]" } else { "" };

            out.push_str(&format!(
                "- **{}**{}\n  {}\n  {}\n\n",
                conv.id, archived_marker, path_display, summary_display
            ));
        }

        Ok(out)
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct MessageItem {
    pub role: String,
    pub content: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct ConversationOutput {
    pub id: String,
    pub path: Option<String>,
    pub summary: Option<String>,
    pub messages: Vec<MessageItem>,
}

impl Formattable for ConversationOutput {
    fn format_markdown(&self) -> Result<String> {
        let mut out = String::new();

        let path_display = self
            .path
            .as_ref()
            .map(|p| short_path(p))
            .unwrap_or_else(|| "(no path)".into());

        out.push_str(&format!("# Conversation: {}\n\n", self.id));
        out.push_str(&format!("**Path:** {}\n", path_display));

        if let Some(summary) = &self.summary {
            out.push_str(&format!("**Summary:** {}\n", summary));
        }

        out.push_str("\n---\n\n");

        for msg in &self.messages {
            let role_header = match msg.role.as_str() {
                "user" => "## User",
                "assistant" => "## Assistant",
                "tool" => "## Tool",
                other => &format!("## {}", other),
            };
            out.push_str(role_header);
            out.push_str("\n\n");
            out.push_str(&msg.content);
            out.push_str("\n\n");
        }

        Ok(out)
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct SearchResultItem {
    pub id: String,
    pub path: Option<String>,
    pub summary: Option<String>,
    pub match_context: Option<String>,
    pub archived: bool,
}

#[derive(Debug, Clone, Serialize)]
pub struct SearchOutput {
    pub query: String,
    pub total: usize,
    pub results: Vec<SearchResultItem>,
}

impl Formattable for SearchOutput {
    fn format_markdown(&self) -> Result<String> {
        let mut out = String::new();
        out.push_str(&format!(
            "# Search Results for \"{}\"\n\nFound {} matches\n\n",
            self.query, self.total
        ));

        for result in &self.results {
            let path_display = result
                .path
                .as_ref()
                .map(|p| short_path(p))
                .unwrap_or_else(|| "(no path)".into());

            let archived_marker = if result.archived { " [archived]" } else { "" };

            out.push_str(&format!("## {}{}\n\n", result.id, archived_marker));
            out.push_str(&format!("**Path:** {}\n", path_display));

            if let Some(summary) = &result.summary {
                out.push_str(&format!("**Summary:** {}\n", summary));
            }

            if let Some(context) = &result.match_context {
                out.push_str(&format!("\n> {}\n", context));
            }

            out.push_str("\n");
        }

        Ok(out)
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct ActionOutput {
    pub action: String,
    pub id: String,
    pub original_path: String,
    pub new_path: String,
}

impl Formattable for ActionOutput {
    fn format_markdown(&self) -> Result<String> {
        let mut out = String::new();
        out.push_str(&format!("# {}\n\n", self.action));
        out.push_str(&format!("**Session:** {}\n", self.id));
        out.push_str(&format!("**From:** {}\n", self.original_path));
        out.push_str(&format!("**To:** {}\n", self.new_path));
        Ok(out)
    }
}
