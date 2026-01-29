use anyhow::Result;
use cc_history_lib::short_path;
use serde::Serialize;

use crate::output::Formattable;

#[derive(Debug, Clone, Serialize)]
pub struct ConversationItem {
    pub id: String,
    pub path: Option<String>,
    pub summary: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub custom_title: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub first_prompt: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub git_branch: Option<String>,
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

            let title_display = conv
                .custom_title
                .as_ref()
                .or(conv.summary.as_ref())
                .map(|s| truncate_str(s, 60))
                .unwrap_or_else(|| "(no title)".into());

            let archived_marker = if conv.archived { " [archived]" } else { "" };
            let branch_marker = conv
                .git_branch
                .as_ref()
                .map(|b| format!(" ({})", b))
                .unwrap_or_default();

            out.push_str(&format!(
                "- **{}**{}{}\n  {}\n  {}\n\n",
                conv.id, archived_marker, branch_marker, path_display, title_display
            ));
        }

        Ok(out)
    }
}

fn truncate_str(s: &str, max_len: usize) -> String {
    let truncated: String = s.chars().take(max_len).collect();
    if s.chars().count() > max_len {
        format!("{}...", truncated)
    } else {
        truncated
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
    #[serde(skip_serializing_if = "Option::is_none")]
    pub custom_title: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub first_prompt: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub git_branch: Option<String>,
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
            let branch_marker = result
                .git_branch
                .as_ref()
                .map(|b| format!(" ({})", b))
                .unwrap_or_default();

            out.push_str(&format!("## {}{}{}\n\n", result.id, archived_marker, branch_marker));
            out.push_str(&format!("**Path:** {}\n", path_display));

            if let Some(title) = &result.custom_title {
                out.push_str(&format!("**Title:** {}\n", title));
            } else if let Some(summary) = &result.summary {
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
