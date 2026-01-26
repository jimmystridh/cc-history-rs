use anyhow::{anyhow, Context, Result};
use cc_history_lib::{
    archive_root_dir, archive_session, duplicate_session,
    rename_session as lib_rename_session, repath_session as lib_repath_session,
    sort_conversations_by_earliest, unarchive_session, ChatReader, Message,
};
use crossterm::event::{
    self, Event, KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind,
};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode};
use notify::{RecursiveMode, Watcher};
use once_cell::sync::Lazy;
use pulldown_cmark::{html, Options, Parser};
use ratatui::backend::CrosstermBackend;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Cell, Clear, Paragraph, Row, Table, Wrap};
use ratatui::Terminal;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};
use std::thread;
use std::time::{Duration, Instant};
use tempfile::TempDir;
use syntect::easy::HighlightLines;
use syntect::highlighting::{FontStyle, Style as SyntectStyle, Theme, ThemeSet};
use syntect::parsing::{SyntaxReference, SyntaxSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    List,
    View,
}

static SYNTAX_SET: Lazy<SyntaxSet> = Lazy::new(SyntaxSet::load_defaults_newlines);
static SYNTAX_THEME: Lazy<Theme> = Lazy::new(|| {
    let mut themes = ThemeSet::load_defaults();
    themes
        .themes
        .remove("base16-ocean.dark")
        .or_else(|| themes.themes.remove("Solarized (dark)"))
        .or_else(|| themes.themes.into_values().next())
        .expect("syntect default themes available")
});

const CODE_INDENT: &str = "     ";
const MOUSE_SCROLL_LINES: usize = 3;

// ═══════════════════════════════════════════════════════════════════════════════
// 🎨 Color Palette - Modern Dark Theme
// Inspired by Nord/Dracula with a sophisticated, cohesive look
// ═══════════════════════════════════════════════════════════════════════════════

#[allow(dead_code)]
mod theme {
    use ratatui::style::Color;

    // ─── Surface Colors ───────────────────────────────────────────────────────
    pub const SURFACE_DIM: Color = Color::Rgb(30, 30, 46); // Dark background for modals
    pub const SURFACE_BORDER: Color = Color::Rgb(69, 71, 90); // Subtle borders

    // ─── Accent Colors ────────────────────────────────────────────────────────
    pub const PRIMARY: Color = Color::Rgb(137, 180, 250); // Soft blue - main accent
    pub const PRIMARY_DIM: Color = Color::Rgb(88, 91, 112); // Muted blue for borders
    pub const SECONDARY: Color = Color::Rgb(180, 190, 254); // Lavender - headers

    // ─── Semantic Colors ──────────────────────────────────────────────────────
    pub const SUCCESS: Color = Color::Rgb(166, 227, 161); // Soft green - user messages
    pub const SUCCESS_DIM: Color = Color::Rgb(116, 199, 136); // Darker green for accents
    pub const INFO: Color = Color::Rgb(137, 220, 235); // Teal/cyan - assistant messages
    pub const WARNING: Color = Color::Rgb(249, 226, 175); // Warm yellow - paths/CWD
    pub const WARNING_BRIGHT: Color = Color::Rgb(250, 179, 135); // Peach - groups
    pub const MUTED: Color = Color::Rgb(147, 153, 178); // Gray - tool results
    pub const MUTED_DIM: Color = Color::Rgb(108, 112, 134); // Darker gray

    // ─── Text Colors ──────────────────────────────────────────────────────────
    pub const TEXT: Color = Color::Rgb(205, 214, 244); // Primary text
    pub const TEXT_DIM: Color = Color::Rgb(166, 173, 200); // Secondary text
    pub const TEXT_MUTED: Color = Color::Rgb(127, 132, 156); // Muted text

    // ─── Special Colors ───────────────────────────────────────────────────────
    pub const SELECTION_BG: Color = Color::Rgb(88, 91, 112); // Selection background
    pub const SELECTION_FG: Color = Color::Rgb(255, 255, 255); // Selection text
    pub const HIGHLIGHT: Color = Color::Rgb(249, 226, 175); // Search highlight bg
    pub const HIGHLIGHT_FG: Color = Color::Rgb(30, 30, 46); // Search highlight text

    // ─── Modal Accent Colors ──────────────────────────────────────────────────
    pub const MODAL_INFO: Color = Color::Rgb(137, 220, 235); // Info modal border
    pub const MODAL_HELP: Color = Color::Rgb(203, 166, 247); // Help modal border (mauve)
    pub const MODAL_SETTINGS: Color = Color::Rgb(166, 227, 161); // Settings modal border

    // ─── Header/Footer Gradient ───────────────────────────────────────────────
    pub const HEADER_BG: Color = Color::Rgb(49, 50, 68); // Darker surface
    pub const HEADER_FG: Color = Color::Rgb(205, 214, 244); // Light text
}

#[derive(Clone)]
struct RowItem {
    id: String,
    date_ms: i64,
    path: String,
    first_msg: String,
    file_path: String,
    last_msg_ms: Option<i64>,
}

#[derive(Clone)]
enum DisplayRow {
    Group {
        path: String,
        count: usize,
        newest_date_ms: i64,
        newest_preview: String,
    },
    Conversation(RowItem),
}

#[derive(Clone)]
struct ArchivedAction {
    row: RowItem,
    original_path: PathBuf,
    archived_path: PathBuf,
}

struct FirstMsgRequest {
    id: String,
    file_path: String,
}

struct FirstMsgResult {
    id: String,
    first_msg: Option<String>,
    messages: Vec<Message>,
}

struct App {
    reader: ChatReader,
    projects_dir: PathBuf,
    base_projects_dir: PathBuf, // original ~/.claude/projects
    viewing_archive: bool,
    all_rows: Vec<RowItem>,
    rows: Vec<RowItem>,
    messages_cache: HashMap<String, Vec<Message>>,
    last_archived: Option<ArchivedAction>,
    mode: Mode,
    selected: usize,
    top: usize,
    filter_path: Option<String>,
    view_lines: Vec<Line<'static>>,
    view_scroll: u16,
    view_text: Vec<String>,
    view_show_tools: bool,
    view_expand_tools: bool,
    search_input: bool,
    search_query: String,
    search_matches: Vec<usize>,
    search_index: usize,
    // sorting
    sort_key: SortKey,
    sort_desc: bool,
    sorting_in_progress: bool,
    sort_rx: Option<Receiver<Vec<RowItem>>>,
    // list-wide search
    list_search_input: bool,
    list_search_query: String,
    list_search_in_progress: bool,
    list_search_rx: Option<Receiver<Vec<RowItem>>>,
    // rename session
    rename_input: bool,
    rename_query: String,
    // repath session (change base directory)
    repath_input: bool,
    repath_query: String,
    // fs watching + refreshing
    updating_in_progress: bool,
    update_rx: Option<Receiver<Vec<RowItem>>>,
    fs_ping_rx: Option<Receiver<Vec<PathBuf>>>,
    watcher_enabled: bool,
    last_renamed: Option<(PathBuf, Instant)>,
    fs_debounce_until: Option<Instant>, // debounce FS events - don't refresh until this time
    // info modal
    show_info_modal: bool,
    // help modal
    show_help_modal: bool,
    // settings modal
    show_settings_modal: bool,
    settings_claude_command: String,
    settings_quit_after_launch: bool,
    settings_selected_field: usize, // 0 = claude_command, 1 = quit_after_launch
    // resume session
    resume_session_request: Option<(String, String)>, // (session_id, session_path)
    // async first message loading
    first_msg_tx: Option<Sender<FirstMsgRequest>>,
    first_msg_rx: Option<Receiver<FirstMsgResult>>,
    first_msg_pending: HashSet<String>,
    pending_selected_id: Option<String>,
    pending_selected_index: Option<usize>,
    pending_selected_offset: Option<usize>,
    // grouped view
    grouped_mode: bool,
    expanded_paths: HashSet<String>,
    display_rows: Vec<DisplayRow>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SortKey {
    Created,
    Path,
    LastMsg,
}

impl App {
    fn new_with_projects(projects_dir: PathBuf) -> Result<Self> {
        let reader = ChatReader::with_projects_dir(projects_dir.clone());
        let mut convos = reader.list_conversations()?;
        let (first_msg_tx, first_msg_rx) = spawn_first_msg_loader(projects_dir.clone());
        let mut first_msg_rx_opt = Some(first_msg_rx);
        let watch_pref = load_watcher_pref();
        let (sort_key_pref, sort_desc_pref) = load_sort_prefs();
        let filter_pref = load_filter_pref();
        let claude_command_pref = load_claude_command_pref();
        let quit_after_launch_pref = load_quit_after_launch_pref();
        if convos.is_empty() {
            let mut app = Self {
                reader,
                projects_dir: projects_dir.clone(),
                base_projects_dir: projects_dir.clone(),
                viewing_archive: false,
                all_rows: vec![],
                rows: vec![],
                messages_cache: HashMap::new(),
                last_archived: None,
                mode: Mode::List,
                selected: 0,
                top: 0,
                filter_path: None,
                view_lines: vec![],
                view_scroll: 0,
                view_text: vec![],
                view_show_tools: false,
                view_expand_tools: false,
                search_input: false,
                search_query: String::new(),
                search_matches: vec![],
                search_index: 0,
                sort_key: sort_key_pref,
                sort_desc: sort_desc_pref,
                sorting_in_progress: false,
                sort_rx: None,
                list_search_input: false,
                list_search_query: String::new(),
                list_search_in_progress: false,
                list_search_rx: None,
                rename_input: false,
                rename_query: String::new(),
                repath_input: false,
                repath_query: String::new(),
                updating_in_progress: false,
                update_rx: None,
                fs_ping_rx: if watch_pref {
                    spawn_initial_fs_watcher(&projects_dir)
                } else {
                    None
                },
                watcher_enabled: watch_pref,
                last_renamed: None,
                fs_debounce_until: None,
                show_info_modal: false,
                show_help_modal: false,
                show_settings_modal: false,
                settings_claude_command: claude_command_pref.clone(),
                settings_quit_after_launch: quit_after_launch_pref,
                settings_selected_field: 0,
                resume_session_request: None,
                first_msg_tx: Some(first_msg_tx.clone()),
                first_msg_rx: first_msg_rx_opt.take(),
                first_msg_pending: HashSet::new(),
                pending_selected_id: None,
                pending_selected_index: None,
                pending_selected_offset: None,
                grouped_mode: load_grouped_mode_pref(),
                expanded_paths: HashSet::new(),
                display_rows: vec![],
            };
            if let Some(fp) = filter_pref {
                app.set_filter(Some(fp));
            }
            app.rebuild_display_rows();
            return Ok(app);
        }
        // Avoid expensive full-file scans on startup; sort by metadata timestamp only
        convos.sort_by_key(|c| c.created_at.unwrap_or(i64::MAX));

        let mut rows = Vec::new();
        let cache = HashMap::new(); // lazy-load messages on demand to avoid long startup blank screen
        for c in convos {
            let date_ms = c.created_at.unwrap_or(0);
            let path_str = c.path.clone().unwrap_or_default();
            let summary_preview = c
                .summary
                .as_ref()
                .map(|s| single_line_preview(s, 240))
                .unwrap_or_default();
            rows.push(RowItem {
                id: c.id,
                date_ms,
                path: path_str,
                first_msg: summary_preview,
                file_path: c.file.display().to_string(),
                last_msg_ms: None,
            });
        }
        // Default sort: newest on top (created desc)
        rows.sort_by(|a, b| b.date_ms.cmp(&a.date_ms));

        // Optionally set up FS watcher ping channel based on preference
        let ping_rx = if watch_pref {
            spawn_initial_fs_watcher(&projects_dir)
        } else {
            None
        };

        let mut app = Self {
            reader,
            projects_dir: projects_dir.clone(),
            base_projects_dir: projects_dir.clone(),
            viewing_archive: false,
            all_rows: rows.clone(),
            rows,
            messages_cache: cache,
            last_archived: None,
            mode: Mode::List,
            selected: 0,
            top: 0,
            filter_path: None,
            view_lines: vec![],
            view_scroll: 0,
            view_text: vec![],
            view_show_tools: false,
            view_expand_tools: false,
            search_input: false,
            search_query: String::new(),
            search_matches: vec![],
            search_index: 0,
            sort_key: sort_key_pref,
            sort_desc: sort_desc_pref,
            sorting_in_progress: false,
            sort_rx: None,
            list_search_input: false,
            list_search_query: String::new(),
            list_search_in_progress: false,
            list_search_rx: None,
            rename_input: false,
            rename_query: String::new(),
            repath_input: false,
            repath_query: String::new(),
            updating_in_progress: false,
            update_rx: None,
            fs_ping_rx: ping_rx,
            watcher_enabled: watch_pref,
            last_renamed: None,
            fs_debounce_until: None,
            show_info_modal: false,
            show_help_modal: false,
            show_settings_modal: false,
            settings_claude_command: claude_command_pref.clone(),
            settings_quit_after_launch: quit_after_launch_pref,
            settings_selected_field: 0,
            resume_session_request: None,
            first_msg_tx: Some(first_msg_tx.clone()),
            first_msg_rx: first_msg_rx_opt.take(),
            first_msg_pending: HashSet::new(),
            pending_selected_id: None,
            pending_selected_index: None,
            pending_selected_offset: None,
            grouped_mode: load_grouped_mode_pref(),
            expanded_paths: HashSet::new(),
            display_rows: vec![],
        };
        if let Some(fp) = filter_pref.clone() {
            app.set_filter(Some(fp));
        } else {
            app.set_filter(None);
        }
        apply_sort(&mut app);
        app.rebuild_display_rows();
        Ok(app)
    }

    fn set_filter(&mut self, path: Option<String>) {
        self.filter_path = path;
        if let Some(ref p) = self.filter_path {
            self.rows = self
                .all_rows
                .iter()
                .filter(|r| &r.path == p)
                .cloned()
                .collect();
        } else {
            self.rows = self.all_rows.clone();
        }
        self.clear_pending_list_position();
        self.selected = 0;
        self.top = 0;
        self.rebuild_display_rows();
    }

    fn is_input_active(&self) -> bool {
        self.list_search_input
            || self.search_input
            || self.show_settings_modal
            || self.rename_input
            || self.repath_input
    }

    fn clear_pending_list_position(&mut self) {
        self.pending_selected_id = None;
        self.pending_selected_index = None;
        self.pending_selected_offset = None;
    }

    fn snapshot_list_position(&mut self) {
        // Get the ID from display_rows (not rows) since selected indexes into display_rows
        let id = match self.display_rows.get(self.selected) {
            Some(DisplayRow::Conversation(row)) => Some(row.id.clone()),
            Some(DisplayRow::Group { path, .. }) => {
                // For group headers, find the first conversation in that group
                self.rows
                    .iter()
                    .find(|r| &r.path == path)
                    .map(|r| r.id.clone())
            }
            None => None,
        };
        if let Some(id) = id {
            self.pending_selected_id = Some(id);
            self.pending_selected_index = Some(self.selected);
            self.pending_selected_offset = Some(self.selected.saturating_sub(self.top));
        } else {
            self.clear_pending_list_position();
        }
    }

    fn restore_list_position(&mut self) {
        // Must rebuild display_rows first since rows changed
        self.rebuild_display_rows();

        let saved_id = self.pending_selected_id.take();
        let saved_index = self.pending_selected_index.take();
        let offset = self.pending_selected_offset.take().unwrap_or(0);

        if self.display_rows.is_empty() {
            self.selected = 0;
            self.top = 0;
            return;
        }

        let mut target_index = saved_index
            .unwrap_or_else(|| self.selected.min(self.display_rows.len().saturating_sub(1)));

        if let Some(id) = saved_id {
            // Search display_rows for a conversation with matching ID
            if let Some(pos) = self
                .display_rows
                .iter()
                .position(|dr| matches!(dr, DisplayRow::Conversation(r) if r.id == id))
            {
                target_index = pos;
            }
        }

        target_index = target_index.min(self.display_rows.len().saturating_sub(1));
        self.selected = target_index;

        let kept_offset = offset.min(self.selected);
        self.top = self.selected.saturating_sub(kept_offset);
    }

    fn rebuild_display_rows(&mut self) {
        self.display_rows.clear();

        if !self.grouped_mode {
            // Flat mode: just wrap each row as a Conversation
            for row in &self.rows {
                self.display_rows
                    .push(DisplayRow::Conversation(row.clone()));
            }
            return;
        }

        // Grouped mode: group by path
        // First, collect groups in order of appearance (preserving sort order)
        let mut seen_paths: Vec<String> = Vec::new();
        let mut groups: HashMap<String, Vec<RowItem>> = HashMap::new();

        for row in &self.rows {
            let path = row.path.clone();
            if !groups.contains_key(&path) {
                seen_paths.push(path.clone());
            }
            groups.entry(path).or_default().push(row.clone());
        }

        for path in seen_paths {
            let convos = groups.remove(&path).unwrap_or_default();
            if convos.is_empty() {
                continue;
            }

            // Find newest conversation for preview
            let newest = convos
                .iter()
                .max_by_key(|c| c.date_ms)
                .cloned()
                .unwrap_or_else(|| convos[0].clone());

            // Add group header
            self.display_rows.push(DisplayRow::Group {
                path: path.clone(),
                count: convos.len(),
                newest_date_ms: newest.date_ms,
                newest_preview: newest.first_msg.clone(),
            });

            // If this group is expanded, add its conversations
            if self.expanded_paths.contains(&path) {
                for convo in convos {
                    self.display_rows.push(DisplayRow::Conversation(convo));
                }
            }
        }
    }

    fn toggle_group_at_selected(&mut self) {
        if let Some(DisplayRow::Group { path, .. }) = self.display_rows.get(self.selected) {
            let path = path.clone();
            if self.expanded_paths.contains(&path) {
                self.expanded_paths.remove(&path);
            } else {
                self.expanded_paths.insert(path);
            }
            self.rebuild_display_rows();
        }
    }

    fn expand_group_at_selected(&mut self) {
        if let Some(DisplayRow::Group { path, .. }) = self.display_rows.get(self.selected) {
            if !self.expanded_paths.contains(path) {
                let path = path.clone();
                self.expanded_paths.insert(path);
                self.rebuild_display_rows();
            }
        }
    }

    fn collapse_group_at_selected(&mut self) {
        match self.display_rows.get(self.selected) {
            Some(DisplayRow::Group { path, .. }) => {
                if self.expanded_paths.contains(path) {
                    let path = path.clone();
                    self.expanded_paths.remove(&path);
                    self.rebuild_display_rows();
                }
            }
            Some(DisplayRow::Conversation(row)) => {
                // If on a conversation, collapse its parent group and move selection to group header
                let path = row.path.clone();
                if self.expanded_paths.contains(&path) {
                    self.expanded_paths.remove(&path);
                    self.rebuild_display_rows();
                    // Find the group header for this path
                    for (i, dr) in self.display_rows.iter().enumerate() {
                        if let DisplayRow::Group { path: gp, .. } = dr {
                            if *gp == path {
                                self.selected = i;
                                break;
                            }
                        }
                    }
                }
            }
            None => {}
        }
    }

    #[allow(dead_code)]
    fn selected_row_item(&self) -> Option<&RowItem> {
        match self.display_rows.get(self.selected) {
            Some(DisplayRow::Conversation(row)) => Some(row),
            Some(DisplayRow::Group { path, .. }) => {
                // Return the first (newest) conversation in this group
                self.rows.iter().find(|r| &r.path == path)
            }
            None => None,
        }
    }

    fn is_group_selected(&self) -> bool {
        matches!(
            self.display_rows.get(self.selected),
            Some(DisplayRow::Group { .. })
        )
    }

    fn toggle_archive_view(&mut self) {
        self.viewing_archive = !self.viewing_archive;
        let new_dir = if self.viewing_archive {
            archive_root_dir(&self.base_projects_dir)
        } else {
            self.base_projects_dir.clone()
        };
        self.projects_dir = new_dir.clone();
        self.reader = ChatReader::with_projects_dir(new_dir.clone());

        // Clear current state
        self.all_rows.clear();
        self.rows.clear();
        self.messages_cache.clear();
        self.first_msg_pending.clear();
        self.filter_path = None;
        self.list_search_query.clear();
        self.list_search_input = false;
        self.expanded_paths.clear();
        self.selected = 0;
        self.top = 0;
        self.last_archived = None;

        // Restart first message loader for new directory
        let (first_msg_tx, first_msg_rx) = spawn_first_msg_loader(new_dir.clone());
        self.first_msg_tx = Some(first_msg_tx);
        self.first_msg_rx = Some(first_msg_rx);

        // Restart FS watcher if enabled
        if self.watcher_enabled {
            self.fs_ping_rx = spawn_initial_fs_watcher(&new_dir);
        }

        // Load conversations from the new directory
        if let Ok(convos) = self.reader.list_conversations() {
            let mut rows: Vec<RowItem> = convos
                .into_iter()
                .map(|c| {
                    let summary_preview = c
                        .summary
                        .as_ref()
                        .map(|s| single_line_preview(s, 240))
                        .unwrap_or_default();
                    RowItem {
                        id: c.id,
                        date_ms: c.created_at.unwrap_or(0),
                        path: c.path.unwrap_or_default(),
                        first_msg: summary_preview,
                        file_path: c.file.display().to_string(),
                        last_msg_ms: None,
                    }
                })
                .collect();

            // Apply default sort
            sort_rows_by(&mut rows, self.sort_key, self.sort_desc);
            self.all_rows = rows.clone();
            self.rows = rows;
        }

        self.rebuild_display_rows();
    }
}

fn format_time(ms: i64) -> String {
    if ms <= 0 {
        return String::new();
    }
    let dt = chrono::DateTime::<chrono::Utc>::from_timestamp_millis(ms)
        .unwrap_or(chrono::DateTime::<chrono::Utc>::UNIX_EPOCH);
    let local_dt = dt.with_timezone(&chrono::Local);
    local_dt.format("%Y-%m-%d %H:%M").to_string()
}

fn lines_for_message(
    msg: &Message,
    show_tools: bool,
    expand_tools: bool,
) -> (Vec<Line<'static>>, Vec<String>) {
    let mut out: Vec<Line> = Vec::new();
    let mut texts: Vec<String> = Vec::new();
    let trimmed = msg.content.trim();
    if trimmed.is_empty() {
        return (out, texts);
    }

    let is_tool = msg.role.contains("tool");

    if is_tool && !show_tools {
        return (out, texts);
    }

    let (prefix, title, header_text) = if msg.role == "user" {
        (
            Span::styled("▸ ", Style::default().fg(theme::SUCCESS)),
            Span::styled("You", Style::default().fg(theme::SUCCESS).add_modifier(Modifier::BOLD)),
            "You:",
        )
    } else if is_tool {
        (
            Span::styled("◂ ", Style::default().fg(theme::MUTED)),
            Span::styled(
                "Tool",
                Style::default().fg(theme::MUTED),
            ),
            "Tool Result:",
        )
    } else {
        (
            Span::styled("◂ ", Style::default().fg(theme::INFO)),
            Span::styled("Claude", Style::default().fg(theme::INFO).add_modifier(Modifier::BOLD)),
            "Assistant:",
        )
    };
    out.push(Line::from(vec![
        prefix.clone(),
        title.clone(),
        Span::raw(":"),
    ]));
    texts.push(header_text.to_string());

    if is_tool && !expand_tools {
        let preview = tool_preview_summary(trimmed);
        let summary = format!("⋯ {}  (T to expand)", preview);
        out.push(Line::from(vec![
            Span::styled(
                CODE_INDENT.to_string(),
                Style::default().fg(theme::SURFACE_BORDER),
            ),
            Span::styled(
                summary.clone(),
                Style::default().fg(theme::MUTED_DIM),
            ),
        ]));
        texts.push(format!("{}{}", CODE_INDENT, summary));
        return (out, texts);
    }

    let mut in_code = false;
    let mut code_buffer: Vec<String> = Vec::new();
    let mut code_lang: Option<String> = None;
    let mut fence_char: Option<char> = None;

    for raw_line in trimmed.lines() {
        let trimmed_line = raw_line.trim_start();
        if let Some((marker, fence_len)) = detect_code_fence(trimmed_line) {
            if in_code {
                if fence_char == Some(marker) {
                    push_code_block(
                        &mut out,
                        &mut texts,
                        &code_buffer,
                        code_lang.as_deref(),
                        is_tool,
                    );
                    code_buffer.clear();
                    code_lang = None;
                    fence_char = None;
                    in_code = false;
                    continue;
                }
            } else {
                in_code = true;
                fence_char = Some(marker);
                let info = trimmed_line[fence_len..].trim();
                if !info.is_empty() {
                    let lang = info.split_whitespace().next().unwrap_or(info);
                    code_lang = Some(lang.to_string());
                }
                continue;
            }
        }

        if in_code {
            code_buffer.push(raw_line.to_string());
        } else {
            texts.push(format!("{}{}", CODE_INDENT, raw_line));
            let mut spans = Vec::with_capacity(2);
            spans.push(Span::styled(
                CODE_INDENT.to_string(),
                Style::default().fg(theme::SURFACE_BORDER),
            ));
            let content_span = if is_tool {
                Span::styled(
                    raw_line.to_string(),
                    Style::default().fg(theme::MUTED),
                )
            } else {
                Span::styled(
                    raw_line.to_string(),
                    Style::default().fg(theme::TEXT),
                )
            };
            spans.push(content_span);
            out.push(Line::from(spans));
        }
    }

    if in_code {
        push_code_block(
            &mut out,
            &mut texts,
            &code_buffer,
            code_lang.as_deref(),
            is_tool,
        );
    }

    (out, texts)
}

fn detect_code_fence(line: &str) -> Option<(char, usize)> {
    let mut chars = line.chars();
    let first = chars.next()?;
    if first != '`' && first != '~' {
        return None;
    }
    let count = line.chars().take_while(|c| *c == first).count();
    if count >= 3 {
        Some((first, count))
    } else {
        None
    }
}

fn push_code_block(
    out: &mut Vec<Line<'static>>,
    texts: &mut Vec<String>,
    code_buffer: &[String],
    lang_hint: Option<&str>,
    monochrome: bool,
) {
    if code_buffer.is_empty() {
        texts.push(CODE_INDENT.to_string());
        out.push(Line::from(vec![
            Span::styled(
                CODE_INDENT.to_string(),
                Style::default().fg(theme::SURFACE_BORDER),
            ),
            Span::raw(String::new()),
        ]));
        return;
    }

    let highlighted = highlight_code_block(code_buffer, lang_hint, monochrome);
    for line in highlighted {
        out.push(line);
    }
    for raw in code_buffer {
        texts.push(format!("{}{}", CODE_INDENT, raw));
    }
}

fn highlight_code_block(
    code_lines: &[String],
    lang_hint: Option<&str>,
    monochrome: bool,
) -> Vec<Line<'static>> {
    let syntax_set = &*SYNTAX_SET;
    let syntax = select_syntax(lang_hint, code_lines, syntax_set);
    let mut highlighter = HighlightLines::new(syntax, &SYNTAX_THEME);
    let mut result = Vec::with_capacity(code_lines.len());

    for line in code_lines {
        let mut spans = Vec::new();
        spans.push(Span::styled(
            CODE_INDENT.to_string(),
            Style::default().fg(theme::SURFACE_BORDER),
        ));

        if monochrome {
            if line.is_empty() {
                spans.push(Span::raw(String::new()));
            } else {
                spans.push(Span::styled(
                    line.to_string(),
                    Style::default().fg(theme::MUTED),
                ));
            }
            result.push(Line::from(spans));
            continue;
        }

        match highlighter.highlight_line(line, syntax_set) {
            Ok(ranges) => {
                if ranges.is_empty() {
                    spans.push(Span::raw(String::new()));
                } else {
                    for (style, piece) in ranges {
                        if piece.is_empty() {
                            continue;
                        }
                        spans.push(Span::styled(piece.to_string(), style_to_tui(style)));
                    }
                }
            }
            Err(_) => {
                spans.push(Span::raw(line.to_string()));
            }
        }

        result.push(Line::from(spans));
    }

    result
}

fn select_syntax<'a>(
    lang_hint: Option<&str>,
    code_lines: &[String],
    syntax_set: &'a SyntaxSet,
) -> &'a SyntaxReference {
    if let Some(token) = lang_hint {
        if let Some(syn) = find_syntax_for_token(token, syntax_set) {
            return syn;
        }
    }

    if let Some(line) = code_lines.iter().find(|l| !l.trim().is_empty()) {
        if let Some(syn) = syntax_set.find_syntax_by_first_line(line) {
            return syn;
        }
    }

    syntax_set.find_syntax_plain_text()
}

fn find_syntax_for_token<'a>(
    token: &str,
    syntax_set: &'a SyntaxSet,
) -> Option<&'a SyntaxReference> {
    let trimmed = token.trim_matches(|c: char| c == '{' || c == '}' || c == '`');
    if trimmed.is_empty() {
        return None;
    }

    let mut seen = HashSet::new();
    for candidate in language_candidates(trimmed) {
        if !seen.insert(candidate.clone()) {
            continue;
        }
        if let Some(syn) = syntax_set.find_syntax_by_token(&candidate) {
            return Some(syn);
        }
        if let Some(syn) = syntax_set.find_syntax_by_extension(&candidate) {
            return Some(syn);
        }
    }

    None
}

fn language_candidates(token: &str) -> Vec<String> {
    let mut candidates = Vec::new();
    let trimmed = token.trim();
    if trimmed.is_empty() {
        return candidates;
    }

    candidates.push(trimmed.to_string());
    candidates.push(trimmed.to_lowercase());

    let sanitized = trimmed.trim_matches(|c: char| {
        !c.is_alphanumeric() && c != '+' && c != '#' && c != '.' && c != '_'
    });
    if !sanitized.is_empty() && sanitized != trimmed {
        candidates.push(sanitized.to_string());
        candidates.push(sanitized.to_lowercase());
    }

    if let Some(alias) = map_language_alias(trimmed) {
        candidates.push(alias.to_string());
    }
    if sanitized != trimmed {
        if let Some(alias) = map_language_alias(sanitized) {
            candidates.push(alias.to_string());
        }
    }

    if trimmed.contains('+') {
        candidates.push(trimmed.replace('+', "p"));
    }
    if trimmed.contains('#') {
        candidates.push(trimmed.replace('#', "sharp"));
    }
    if trimmed.contains('.') {
        candidates.push(
            trimmed
                .split('.')
                .next_back()
                .unwrap_or(trimmed)
                .to_string(),
        );
    }

    candidates
}

fn map_language_alias(token: &str) -> Option<&'static str> {
    let lower = token.to_lowercase();
    match lower.as_str() {
        "c++" | "cpp" => Some("cpp"),
        "c#" | "cs" | "csharp" => Some("csharp"),
        "shell" | "bash" | "sh" | "zsh" | "shell-session" => Some("bash"),
        "console" | "terminal" => Some("bash"),
        "powershell" | "ps" | "ps1" => Some("powershell"),
        "py" | "python" => Some("python"),
        "rb" | "ruby" => Some("ruby"),
        "js" | "javascript" | "mjs" | "cjs" => Some("javascript"),
        "ts" | "typescript" => Some("typescript"),
        "tsx" | "typescriptreact" => Some("tsx"),
        "jsx" => Some("jsx"),
        "rs" | "rust" => Some("rust"),
        "go" | "golang" => Some("go"),
        "kt" | "kotlin" => Some("kotlin"),
        "java" => Some("java"),
        "swift" => Some("swift"),
        "scala" => Some("scala"),
        "php" => Some("php"),
        "hs" | "haskell" => Some("haskell"),
        "erl" | "erlang" => Some("erlang"),
        "ex" | "elixir" => Some("elixir"),
        "html" | "htm" => Some("html"),
        "xml" => Some("xml"),
        "jsonl" | "json" => Some("json"),
        "yaml" | "yml" => Some("yaml"),
        "toml" => Some("toml"),
        "ini" | "cfg" => Some("ini"),
        "properties" | "props" => Some("properties"),
        "docker" | "dockerfile" => Some("dockerfile"),
        "make" | "makefile" => Some("makefile"),
        "cmake" => Some("cmake"),
        "gradle" => Some("groovy"),
        "sql" => Some("sql"),
        "diff" | "patch" => Some("diff"),
        "terraform" | "tf" => Some("terraform"),
        "proto" | "protobuf" => Some("proto"),
        "markdown" | "md" => Some("markdown"),
        "latex" | "tex" => Some("latex"),
        _ => None,
    }
}

fn style_to_tui(style: SyntectStyle) -> Style {
    let fg = style.foreground;
    let fg_color = if fg.a == 0 {
        theme::TEXT
    } else {
        Color::Rgb(fg.r, fg.g, fg.b)
    };

    let mut render_style = Style::default().fg(fg_color);
    let mut modifiers = Modifier::empty();
    if style.font_style.contains(FontStyle::BOLD) {
        modifiers |= Modifier::BOLD;
    }
    if style.font_style.contains(FontStyle::ITALIC) {
        modifiers |= Modifier::ITALIC;
    }
    if style.font_style.contains(FontStyle::UNDERLINE) {
        modifiers |= Modifier::UNDERLINED;
    }
    if !modifiers.is_empty() {
        render_style = render_style.add_modifier(modifiers);
    }

    render_style
}

fn list_layout(area: Rect) -> [Rect; 4] {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(2),
            Constraint::Length(1),
            Constraint::Min(1),
            Constraint::Length(1),
        ])
        .split(area);
    [chunks[0], chunks[1], chunks[2], chunks[3]]
}

fn view_layout(area: Rect) -> [Rect; 3] {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(1),
            Constraint::Min(1),
            Constraint::Length(1),
        ])
        .split(area);
    [chunks[0], chunks[1], chunks[2]]
}

fn block_inner(area: Rect) -> Option<Rect> {
    if area.width <= 2 || area.height <= 2 {
        None
    } else {
        Some(Rect {
            x: area.x + 1,
            y: area.y + 1,
            width: area.width - 2,
            height: area.height - 2,
        })
    }
}

fn rect_contains(rect: Rect, column: u16, row: u16) -> bool {
    let within_x = column >= rect.x && column < rect.x.saturating_add(rect.width);
    let within_y = row >= rect.y && row < rect.y.saturating_add(rect.height);
    within_x && within_y
}

fn render_list(f: &mut ratatui::Frame, area: Rect, app: &App) {
    let chunks = list_layout(area);

    // Compute column widths first so header aligns with rows
    let width = chunks[2].width as usize;
    let date_w = 16usize;
    let path_w = ((width as f32 * 0.35) as usize).clamp(20, 50);
    let msg_w = width.saturating_sub(date_w + path_w + 7);

    let sorting_indicator = if app.sorting_in_progress {
        " ⏳"
    } else {
        ""
    };
    let search_indicator = if app.list_search_in_progress {
        " ⏳"
    } else {
        ""
    };
    let watch_indicator = if app.watcher_enabled {
        " │ 👁 live"
    } else {
        ""
    };
    let grouped_indicator = if app.grouped_mode { " " } else { "" };
    let view_name = if app.viewing_archive {
        "📦 Archive"
    } else {
        "🗂 Sessions"
    };
    let mut title_text = if let Some(fp) = &app.filter_path {
        format!(
            " {} │ {} of {} │ {}{}{}{}{}│ 📁 {} ",
            view_name,
            app.rows.len(),
            app.all_rows.len(),
            sort_label(app),
            order_label(app),
            sorting_indicator,
            watch_indicator,
            grouped_indicator,
            cc_history_lib::short_path(fp)
        )
    } else {
        format!(
            " {} │ {} of {} │ {}{}{}{}",
            view_name,
            app.rows.len(),
            app.all_rows.len(),
            sort_label(app),
            order_label(app),
            sorting_indicator,
            watch_indicator
        )
    };
    if app.grouped_mode && app.filter_path.is_none() {
        title_text.push_str(" │ grouped ");
    }
    if !app.list_search_query.is_empty() || app.list_search_in_progress {
        if app.list_search_in_progress {
            title_text.push_str(&format!(
                " │ 🔍 '{}'{}",
                app.list_search_query, search_indicator
            ));
        } else {
            title_text.push_str(&format!(
                " │ 🔍 '{}' ({} found) │ x clear ",
                app.list_search_query,
                app.rows.len()
            ));
        }
    }
    let title = Paragraph::new(Line::from(Span::styled(
        title_text,
        Style::default().fg(theme::HEADER_FG).bg(theme::HEADER_BG),
    )));
    f.render_widget(title, chunks[0]);

    // Aligned header matching the computed column widths
    let header_text = format!(
        "{} │ {} │ {}",
        pad("Time", date_w),
        pad("CWD", path_w),
        pad("First Message", msg_w)
    );
    let header_line = Line::from(Span::styled(
        header_text,
        Style::default()
            .fg(theme::TEXT_DIM)
            .bg(theme::HEADER_BG)
            .add_modifier(Modifier::BOLD),
    ));
    f.render_widget(Paragraph::new(header_line), chunks[1]);

    // Build rows from display_rows
    let rows: Vec<Row> = app
        .display_rows
        .iter()
        .enumerate()
        .skip(app.top)
        .take(chunks[2].height.max(1) as usize)
        .map(|(i, dr)| {
            match dr {
                DisplayRow::Group {
                    path,
                    count,
                    newest_date_ms,
                    newest_preview,
                } => {
                    let expanded = app.expanded_paths.contains(path);
                    let arrow = if expanded { "▼" } else { "▶" };
                    let date = format_time(*newest_date_ms);
                    let cwd_raw = cc_history_lib::short_path(path);
                    let count_str = format!(" ({})", count);
                    // Reserve space for arrow and count in path column
                    let path_content = format!("{} {}{}", arrow, cwd_raw, count_str);
                    let cwd = format_path_cell(&path_content, path_w);
                    let msg = newest_preview;
                    let mut style = if i % 2 == 0 {
                        Style::default().fg(theme::TEXT_MUTED)
                    } else {
                        Style::default().fg(theme::TEXT_DIM)
                    };
                    if i == app.selected {
                        style = Style::default()
                            .fg(theme::SELECTION_FG)
                            .bg(theme::SELECTION_BG)
                            .add_modifier(Modifier::BOLD);
                    }
                    let cells: Vec<Cell> = vec![
                        Cell::from(pad(&date, date_w)),
                        Cell::from(Span::styled("│", Style::default().fg(theme::SURFACE_BORDER))),
                        Cell::from(Span::styled(
                            pad(&cwd, path_w),
                            Style::default()
                                .fg(theme::WARNING_BRIGHT)
                                .add_modifier(Modifier::BOLD),
                        )),
                        Cell::from(Span::styled("│", Style::default().fg(theme::SURFACE_BORDER))),
                        Cell::from(pad(msg, msg_w)),
                    ];
                    Row::new(cells).style(style)
                }
                DisplayRow::Conversation(r) => {
                    let date = format_time(r.date_ms);
                    let cwd_raw = cc_history_lib::short_path(&r.path);
                    // Indent conversations under groups
                    let cwd_display = if app.grouped_mode {
                        format!("  {}", cwd_raw)
                    } else {
                        cwd_raw
                    };
                    let cwd = format_path_cell(&cwd_display, path_w);
                    let msg = &r.first_msg;
                    let mut style = if i % 2 == 0 {
                        Style::default().fg(theme::TEXT_MUTED)
                    } else {
                        Style::default().fg(theme::TEXT_DIM)
                    };
                    if i == app.selected {
                        style = Style::default()
                            .fg(theme::SELECTION_FG)
                            .bg(theme::SELECTION_BG)
                            .add_modifier(Modifier::BOLD);
                    }
                    let cells: Vec<Cell> = vec![
                        Cell::from(pad(&date, date_w)),
                        Cell::from(Span::styled("│", Style::default().fg(theme::SURFACE_BORDER))),
                        Cell::from(Span::styled(
                            pad(&cwd, path_w),
                            Style::default().fg(theme::WARNING),
                        )),
                        Cell::from(Span::styled("│", Style::default().fg(theme::SURFACE_BORDER))),
                        Cell::from(pad(msg, msg_w)),
                    ];
                    Row::new(cells).style(style)
                }
            }
        })
        .collect();

    let widths = [
        Constraint::Length(date_w as u16),
        Constraint::Length(1),
        Constraint::Length(path_w as u16),
        Constraint::Length(1),
        Constraint::Min(10),
    ];
    let table = Table::new(rows, widths).block(
        Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(theme::PRIMARY_DIM)),
    );
    f.render_widget(table, chunks[2]);

    let archive_action = if app.viewing_archive {
        "a restore"
    } else {
        "a archive"
    };
    let footer_text = if app.list_search_input {
        format!(" /{}  — Enter run  Esc cancel", app.list_search_query)
    } else if app.rename_input {
        format!(" Rename: {}_  — Enter save  Esc cancel", app.rename_query)
    } else if app.repath_input {
        format!(" Path: {}_  — Enter save  Esc cancel", app.repath_query)
    } else if app.grouped_mode {
        format!(" ↑↓ move  Enter expand/open  ←→ collapse/expand  Tab flat  / search  s sort  e export  {}  A archive  ? help  q quit ", archive_action)
    } else {
        format!(" ↑↓ move  Enter open  Tab grouped  f filter  / search  s sort  e export  n rename  {}  A archive  ? help  q quit ", archive_action)
    };
    let footer = Paragraph::new(Line::from(Span::styled(
        footer_text,
        Style::default().fg(theme::TEXT_DIM).bg(theme::HEADER_BG),
    )));
    f.render_widget(footer, chunks[3]);
}

fn render_view(f: &mut ratatui::Frame, area: Rect, app: &App) {
    let chunks = view_layout(area);

    let header = Paragraph::new(Line::from(Span::styled(
        " 💬 Conversation ",
        Style::default().fg(theme::HEADER_FG).bg(theme::HEADER_BG),
    )));
    f.render_widget(header, chunks[0]);

    // Highlight current match line if any
    let highlighted_lines = if !app.search_query.is_empty() && !app.search_matches.is_empty() {
        let curr = app.search_matches[app.search_index];
        highlight_current_line(&app.view_lines, curr)
    } else {
        app.view_lines.clone()
    };

    let para = Paragraph::new(highlighted_lines)
        .block(
            Block::default()
                .borders(Borders::ALL)
                .border_style(Style::default().fg(theme::PRIMARY_DIM)),
        )
        .wrap(Wrap { trim: false })
        .scroll((app.view_scroll, 0));
    f.render_widget(para, chunks[1]);

    let archive_action = if app.viewing_archive {
        "a restore"
    } else {
        "a archive"
    };
    let footer_text = if app.search_input {
        format!(" /{}", app.search_query)
    } else if app.repath_input {
        format!(" Path: {}_  — Enter save  Esc cancel", app.repath_query)
    } else if !app.search_query.is_empty() {
        if app.search_matches.is_empty() {
            format!(
                " 0/0 — / search — t tools — T expand — {} ",
                archive_action
            )
        } else {
            format!(
                " {}/{} — n/N next/prev — / search — t tools — T expand — {} ",
                app.search_index + 1,
                app.search_matches.len(),
                archive_action
            )
        }
    } else {
        format!(
            " ↑↓ scroll  PgUp/Dn page  g/G top/bottom  / search  t tools  T expand  r resume  e export  {}  ? help  q back ",
            archive_action
        )
    };
    let footer = Paragraph::new(Line::from(Span::styled(
        footer_text,
        Style::default().fg(theme::TEXT_DIM).bg(theme::HEADER_BG),
    )));
    f.render_widget(footer, chunks[2]);
}

fn handle_list_mouse_event(app: &mut App, mouse: &MouseEvent, viewport: Rect) -> bool {
    if app.rows.is_empty()
        || app.show_help_modal
        || app.show_info_modal
        || app.show_settings_modal
        || app.is_input_active()
    {
        return false;
    }

    let chunks = list_layout(viewport);
    let table_area = chunks[2];
    let inner_area = block_inner(table_area).unwrap_or(table_area);
    let view_height = list_visible_height(viewport.height);
    let column = mouse.column;
    let row = mouse.row;
    let mut changed = false;

    match mouse.kind {
        MouseEventKind::Down(MouseButton::Left) => {
            if rect_contains(inner_area, column, row) {
                let offset = usize::from(row.saturating_sub(inner_area.y));
                let idx = app.top.saturating_add(offset);
                if idx < app.rows.len() {
                    if app.selected != idx {
                        app.selected = idx;
                        changed = true;
                    } else {
                        open_selected_session(app, Some(viewport.height));
                        changed = true;
                    }
                }
            }
        }
        MouseEventKind::ScrollUp => {
            if rect_contains(table_area, column, row) {
                let step = MOUSE_SCROLL_LINES.max(1);
                let prev_selected = app.selected;
                let prev_top = app.top;

                if app.selected > 0 {
                    app.selected = app.selected.saturating_sub(step);
                    if app.selected < app.top {
                        app.top = app.selected;
                    }
                } else if app.top > 0 {
                    app.top = app.top.saturating_sub(step);
                }

                if app.top > app.selected {
                    app.top = app.selected;
                }

                if prev_selected != app.selected || prev_top != app.top {
                    changed = true;
                }
            }
        }
        MouseEventKind::ScrollDown => {
            if rect_contains(table_area, column, row) {
                let step = MOUSE_SCROLL_LINES.max(1);
                let prev_selected = app.selected;
                let prev_top = app.top;

                let last_index = app.rows.len().saturating_sub(1);
                if app.selected < last_index {
                    app.selected = app.selected.saturating_add(step).min(last_index);
                    let visible_end = app.top.saturating_add(view_height.saturating_sub(1));
                    if app.selected > visible_end {
                        let new_top = app.selected.saturating_sub(view_height.saturating_sub(1));
                        let max_top = app.rows.len().saturating_sub(view_height);
                        app.top = new_top.min(max_top);
                    }
                } else {
                    let max_top = app.rows.len().saturating_sub(view_height);
                    if app.top < max_top {
                        app.top = app.top.saturating_add(step).min(max_top);
                    }
                }

                if prev_selected != app.selected || prev_top != app.top {
                    changed = true;
                }
            }
        }
        _ => {}
    }

    changed
}

fn handle_view_mouse_event(app: &mut App, mouse: &MouseEvent, viewport: Rect) -> bool {
    if app.show_help_modal || app.show_settings_modal || app.show_info_modal {
        return false;
    }

    let chunks = view_layout(viewport);
    let content_area = chunks[1];
    let view_height = view_visible_height(viewport.height);
    let current_scroll = usize::from(app.view_scroll);
    let column = mouse.column;
    let row = mouse.row;

    match mouse.kind {
        MouseEventKind::ScrollUp => {
            if rect_contains(content_area, column, row) {
                let step = MOUSE_SCROLL_LINES.max(1);
                let new_scroll = current_scroll.saturating_sub(step);
                if new_scroll != current_scroll {
                    set_view_scroll(app, new_scroll, view_height);
                    return true;
                }
            }
        }
        MouseEventKind::ScrollDown => {
            if rect_contains(content_area, column, row) {
                let step = MOUSE_SCROLL_LINES.max(1);
                let new_scroll = current_scroll.saturating_add(step);
                let before = app.view_scroll;
                set_view_scroll(app, new_scroll, view_height);
                if app.view_scroll != before {
                    return true;
                }
            }
        }
        _ => {}
    }

    false
}

fn open_selected_session(app: &mut App, viewport_height: Option<u16>) {
    // Get the conversation row from display_rows
    let row = match app.display_rows.get(app.selected) {
        Some(DisplayRow::Conversation(r)) => Some(r.clone()),
        Some(DisplayRow::Group { .. }) => None, // Don't open on group headers
        None => None,
    };

    if let Some(row) = row {
        let msgs = if let Some(cached) = app.messages_cache.get(&row.id) {
            cached.clone()
        } else {
            match app.reader.get_messages_by_id(&row.id) {
                Ok((_, fetched)) => {
                    app.messages_cache.insert(row.id.clone(), fetched.clone());
                    fetched
                }
                Err(_) => Vec::new(),
            }
        };

        app.view_show_tools = false;
        app.view_expand_tools = false;
        let (lines, text) = render_message_lines(&msgs, app.view_show_tools, app.view_expand_tools);
        app.view_lines = lines;
        app.view_text = text;
        app.view_scroll = 0;
        app.search_query.clear();
        app.search_matches.clear();
        app.search_index = 0;
        app.mode = Mode::View;

        if let Some(height) = viewport_height {
            let view_height = view_visible_height(height);
            set_view_scroll(app, usize::MAX, view_height);
        }
    }
}

fn archive_selected_session(app: &mut App) -> Result<()> {
    let selected_row = match app.display_rows.get(app.selected) {
        Some(DisplayRow::Conversation(r)) => r.clone(),
        Some(DisplayRow::Group { .. }) => return Err(anyhow!("Cannot archive a group header")),
        None => return Err(anyhow!("No session selected")),
    };

    let original_path = PathBuf::from(&selected_row.file_path);
    let action = archive_session(&original_path, &app.projects_dir)
        .map_err(|e| anyhow!("{}", e))?;

    app.messages_cache.remove(&selected_row.id);
    app.first_msg_pending.remove(&selected_row.id);
    let _ = remove_row_by_id(&mut app.all_rows, &selected_row.id);
    let _ = remove_row_by_id(&mut app.rows, &selected_row.id);

    app.rebuild_display_rows();
    adjust_selection_after_change(app);

    app.last_archived = Some(ArchivedAction {
        row: selected_row,
        original_path: action.original_path,
        archived_path: action.new_path,
    });

    Ok(())
}

fn unarchive_selected_session(app: &mut App) -> Result<()> {
    let selected_row = match app.display_rows.get(app.selected) {
        Some(DisplayRow::Conversation(r)) => r.clone(),
        Some(DisplayRow::Group { .. }) => return Err(anyhow!("Cannot unarchive a group header")),
        None => return Err(anyhow!("No session selected")),
    };

    let archived_path = PathBuf::from(&selected_row.file_path);
    let archive_dir = archive_root_dir(&app.base_projects_dir);
    let action = unarchive_session(&archived_path, &archive_dir, &app.base_projects_dir)
        .map_err(|e| anyhow!("{}", e))?;

    app.messages_cache.remove(&selected_row.id);
    app.first_msg_pending.remove(&selected_row.id);
    let _ = remove_row_by_id(&mut app.all_rows, &selected_row.id);
    let _ = remove_row_by_id(&mut app.rows, &selected_row.id);

    app.rebuild_display_rows();
    adjust_selection_after_change(app);

    app.last_archived = Some(ArchivedAction {
        row: selected_row,
        original_path: action.new_path,
        archived_path: action.original_path,
    });

    Ok(())
}

fn duplicate_selected_session(app: &mut App) -> Result<()> {
    let selected_row = match app.display_rows.get(app.selected) {
        Some(DisplayRow::Conversation(r)) => r.clone(),
        Some(DisplayRow::Group { .. }) => return Err(anyhow!("Cannot duplicate a group header")),
        None => return Err(anyhow!("No session selected")),
    };

    let original_path = PathBuf::from(&selected_row.file_path);
    let summary = if selected_row.first_msg.is_empty() {
        None
    } else {
        Some(selected_row.first_msg.as_str())
    };
    duplicate_session(&original_path, summary).map_err(|e| anyhow!("{}", e))?;

    spawn_full_refresh(app);

    Ok(())
}

fn rename_selected_session(app: &mut App) -> Result<()> {
    let selected_row = match app.display_rows.get(app.selected) {
        Some(DisplayRow::Conversation(r)) => r.clone(),
        Some(DisplayRow::Group { .. }) => return Err(anyhow!("Cannot rename a group header")),
        None => return Err(anyhow!("No session selected")),
    };

    let original_path = PathBuf::from(&selected_row.file_path);
    lib_rename_session(&original_path, &app.rename_query).map_err(|e| anyhow!("{}", e))?;

    app.last_renamed = Some((original_path, Instant::now()));

    let preview = single_line_preview(&app.rename_query, 240);

    if let Some(row) = app.all_rows.iter_mut().find(|r| r.id == selected_row.id) {
        row.first_msg = preview.clone();
    }
    if let Some(row) = app.rows.iter_mut().find(|r| r.id == selected_row.id) {
        row.first_msg = preview;
    }

    Ok(())
}

fn repath_selected_session(app: &mut App) -> Result<()> {
    let selected_row = match app.display_rows.get(app.selected) {
        Some(DisplayRow::Conversation(r)) => r.clone(),
        Some(DisplayRow::Group { .. }) => return Err(anyhow!("Cannot repath a group header")),
        None => return Err(anyhow!("No session selected")),
    };

    let original_file_path = PathBuf::from(&selected_row.file_path);
    let new_cwd = app.repath_query.trim();
    let action = lib_repath_session(&original_file_path, new_cwd, &app.projects_dir)
        .map_err(|e| anyhow!("{}", e))?;

    app.last_renamed = Some((action.new_path.clone(), Instant::now()));

    let new_path_str = new_cwd.to_string();
    let new_file_path_str = action.new_path.to_string_lossy().to_string();

    if let Some(row) = app.all_rows.iter_mut().find(|r| r.id == selected_row.id) {
        row.path = new_path_str.clone();
        row.file_path = new_file_path_str.clone();
    }
    if let Some(row) = app.rows.iter_mut().find(|r| r.id == selected_row.id) {
        row.path = new_path_str;
        row.file_path = new_file_path_str;
    }

    app.rebuild_display_rows();

    Ok(())
}

fn undo_last_archive(app: &mut App) -> Result<()> {
    let action = match app.last_archived.clone() {
        Some(action) => action,
        None => return Err(anyhow!("No archived session to undo")),
    };

    if !action.archived_path.exists() {
        return Err(anyhow!(
            "Archived file missing: {}",
            action.archived_path.display()
        ));
    }

    if action.original_path.exists() {
        return Err(anyhow!(
            "Original path already exists: {}",
            action.original_path.display()
        ));
    }

    if let Some(parent) = action.original_path.parent() {
        fs::create_dir_all(parent)?;
    }

    fs::rename(&action.archived_path, &action.original_path)?;

    app.messages_cache.remove(&action.row.id);

    app.all_rows.push(action.row.clone());
    sort_rows_by(&mut app.all_rows, app.sort_key, app.sort_desc);

    if app.list_search_query.is_empty() {
        let include_row = match &app.filter_path {
            Some(fp) => action.row.path == *fp,
            None => true,
        };
        if include_row {
            app.rows.push(action.row.clone());
            sort_rows_by(&mut app.rows, app.sort_key, app.sort_desc);
            if let Some(idx) = app.rows.iter().position(|r| r.id == action.row.id) {
                app.selected = idx;
            }
        }
    } else {
        spawn_list_search(app);
    }

    adjust_selection_after_change(app);
    app.last_archived = None;

    Ok(())
}

fn remove_row_by_id(rows: &mut Vec<RowItem>, id: &str) -> Option<RowItem> {
    rows.iter()
        .position(|r| r.id == id)
        .map(|idx| rows.remove(idx))
}

fn sort_rows_by(rows: &mut [RowItem], key: SortKey, desc: bool) {
    match key {
        SortKey::Created => {
            if desc {
                rows.sort_by(|a, b| b.date_ms.cmp(&a.date_ms));
            } else {
                rows.sort_by(|a, b| a.date_ms.cmp(&b.date_ms));
            }
        }
        SortKey::Path => {
            if desc {
                rows.sort_by(|a, b| b.path.to_lowercase().cmp(&a.path.to_lowercase()));
            } else {
                rows.sort_by(|a, b| a.path.to_lowercase().cmp(&b.path.to_lowercase()));
            }
        }
        SortKey::LastMsg => {
            for row in rows.iter_mut() {
                if row.last_msg_ms.is_none() {
                    row.last_msg_ms =
                        ChatReader::latest_message_time_ms(&row.file_path);
                }
            }
            if desc {
                rows.sort_by(|a, b| b.last_msg_ms.unwrap_or(0).cmp(&a.last_msg_ms.unwrap_or(0)));
            } else {
                rows.sort_by(|a, b| a.last_msg_ms.unwrap_or(0).cmp(&b.last_msg_ms.unwrap_or(0)));
            }
        }
    }
}

fn adjust_selection_after_change(app: &mut App) {
    let row_count = app.display_rows.len();
    if row_count == 0 {
        app.selected = 0;
        app.top = 0;
        return;
    }

    if app.selected >= row_count {
        app.selected = row_count - 1;
    }

    if app.top > app.selected {
        app.top = app.selected;
    }

    let max_top = row_count.saturating_sub(1);
    if app.top > max_top {
        app.top = max_top;
    }
}

fn highlight_current_line(lines: &[Line<'static>], idx: usize) -> Vec<Line<'static>> {
    let mut out = Vec::with_capacity(lines.len());
    for (i, ln) in lines.iter().cloned().enumerate() {
        if i == idx {
            // Apply a bg highlight to all spans in this line
            let mut new_spans: Vec<Span> = Vec::with_capacity(ln.spans.len());
            for sp in ln.spans.iter() {
                let mut st = sp.style;
                st.bg = Some(theme::HIGHLIGHT);
                // Force dark text for readability regardless of original
                st.fg = Some(theme::HIGHLIGHT_FG);
                new_spans.push(Span {
                    content: sp.content.clone(),
                    style: st,
                });
            }
            out.push(Line::from(new_spans));
        } else {
            out.push(ln);
        }
    }
    out
}

fn tool_preview_summary(content: &str) -> String {
    let filtered: Vec<&str> = content
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .collect();

    let is_noise = |line: &str| {
        line == "```"
            || line == "````"
            || line.starts_with("◀ Tool Result")
            || line.starts_with("<system-reminder>")
            || line.starts_with("</system-reminder>")
    };

    for (idx, line) in filtered.iter().enumerate() {
        if is_noise(line) {
            continue;
        }
        if line.starts_with("Stdout:") || line.starts_with("Stderr:") {
            let detail = filtered.iter().skip(idx + 1).find(|next| !is_noise(next));
            if let Some(next) = detail {
                let combined = format!("{} {}", line, next);
                return single_line_preview(&combined, 120);
            }
            return single_line_preview(line, 120);
        }
        return single_line_preview(line, 120);
    }

    String::from("tool output available")
}

fn pad(s: &str, width: usize) -> String {
    // Truncate safely on char boundaries and add ellipsis when needed
    let char_count = s.chars().count();
    if char_count > width {
        if width == 0 {
            return String::new();
        }
        if width == 1 {
            return "…".to_string();
        }
        let truncated: String = s.chars().take(width - 1).collect();
        return format!("{}…", truncated);
    }
    // Pad with spaces to the right to reach target width (by char count)
    let mut out = String::with_capacity(width);
    out.push_str(s);
    let pad_spaces = width - char_count;
    for _ in 0..pad_spaces {
        out.push(' ');
    }
    out
}

fn ellipsize_start(segment: &str, width: usize) -> String {
    if width == 0 {
        return String::new();
    }
    if width == 1 {
        return "…".to_string();
    }
    let suffix: String = segment
        .chars()
        .rev()
        .take(width - 1)
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
        .collect();
    format!("…{}", suffix)
}

fn format_path_cell(path: &str, width: usize) -> String {
    if width == 0 {
        return String::new();
    }
    if width == 1 {
        return "…".to_string();
    }
    if path.is_empty() {
        return String::new();
    }

    let mut normalized = path.replace('\\', "/");
    if normalized != "/" {
        normalized = normalized.trim_end_matches('/').to_string();
    }

    if char_width(&normalized) <= width {
        return normalized;
    }

    let mut remainder: &str = &normalized;
    let mut root: Option<String> = None;
    if let Some(rest) = normalized.strip_prefix("~/") {
        root = Some("~".to_string());
        remainder = rest;
    } else if normalized.len() >= 2 && normalized.as_bytes()[1] == b':' {
        let drive = &normalized[..2];
        root = Some(drive.to_string());
        remainder = normalized[2..].trim_start_matches('/');
    } else if let Some(rest) = normalized.strip_prefix('/') {
        root = Some("/".to_string());
        remainder = rest;
    }

    let mut components: Vec<&str> = if remainder.is_empty() {
        Vec::new()
    } else {
        remainder.split('/').filter(|s| !s.is_empty()).collect()
    };

    let last_original = if let Some(last) = components.pop() {
        last.to_string()
    } else if let Some(r) = &root {
        r.clone()
    } else {
        normalized.clone()
    };

    let mut segments: Vec<PathSegment> = Vec::new();
    if let Some(r) = root {
        if !components.is_empty() || last_original != r {
            segments.push(PathSegment::root(&r));
        }
    }
    for comp in components {
        segments.push(PathSegment::component(comp));
    }

    if segments.is_empty() {
        let last_len = char_width(&last_original);
        if last_len <= width {
            return last_original;
        }
        return ellipsize_start(&last_original, width);
    }

    let mut collapsed_prefix = false;

    loop {
        let prefix_len: usize = segments.iter().map(|s| s.current_len()).sum();
        let total_len = prefix_len + char_width(&last_original);
        if total_len <= width {
            return combine_segments(&segments, &last_original);
        }

        let mut best_idx: Option<usize> = None;
        let mut best_reduction: usize = 0;
        for (idx, seg) in segments.iter().enumerate() {
            if let Some(reduction) = seg.reduction_if_shrunk() {
                if reduction > best_reduction {
                    best_reduction = reduction;
                    best_idx = Some(idx);
                }
            }
        }

        if let Some(idx) = best_idx {
            segments[idx].shrink();
            continue;
        }

        if !collapsed_prefix && !segments.is_empty() {
            if segments.len() > 1 {
                let new_segments = vec![segments[0].clone(), PathSegment::literal("…/")];
                segments = new_segments;
            } else {
                segments = vec![PathSegment::literal("…/")];
            }
            collapsed_prefix = true;
            continue;
        }

        if prefix_len >= width {
            return ellipsize_start(&normalized, width);
        }

        let allowed = width - prefix_len;
        if allowed == 0 {
            return ellipsize_start(&last_original, width);
        }

        let new_last = if char_width(&last_original) <= allowed {
            last_original.clone()
        } else {
            ellipsize_start(&last_original, allowed)
        };

        if char_width(&new_last) + prefix_len <= width {
            if char_width(&new_last) <= 1 {
                return ellipsize_start(&normalized, width);
            }
            return combine_segments(&segments, &new_last);
        }

        return ellipsize_start(&normalized, width);
    }
}

fn char_width(s: &str) -> usize {
    s.chars().count()
}

#[derive(Clone)]
struct PathSegment {
    variants: Vec<String>,
    index: usize,
}

impl PathSegment {
    fn root(root: &str) -> Self {
        if root == "/" {
            Self {
                variants: vec!["/".to_string()],
                index: 0,
            }
        } else {
            let mut value = root.trim_end_matches('/').to_string();
            if !value.ends_with('/') {
                value.push('/');
            }
            Self {
                variants: vec![value],
                index: 0,
            }
        }
    }

    fn component(component: &str) -> Self {
        let variants = build_component_variants(component);
        Self { variants, index: 0 }
    }

    fn literal(s: &str) -> Self {
        Self {
            variants: vec![s.to_string()],
            index: 0,
        }
    }

    fn current(&self) -> &str {
        &self.variants[self.index]
    }

    fn current_len(&self) -> usize {
        char_width(self.current())
    }

    fn reduction_if_shrunk(&self) -> Option<usize> {
        if self.index + 1 >= self.variants.len() {
            return None;
        }
        let curr = char_width(&self.variants[self.index]);
        let next = char_width(&self.variants[self.index + 1]);
        if curr > next {
            Some(curr - next)
        } else {
            None
        }
    }

    fn shrink(&mut self) {
        if self.index + 1 < self.variants.len() {
            self.index += 1;
        }
    }
}

fn build_component_variants(component: &str) -> Vec<String> {
    if component.is_empty() {
        return vec!["/".to_string()];
    }

    let full = format!("{}/", component);
    let mut variants = vec![full.clone()];

    let chars: Vec<char> = component.chars().collect();
    let len = chars.len();
    let max_take = len.saturating_sub(1).min(3);
    let mut last_len = char_width(&full);

    for take in (1..=max_take).rev() {
        let prefix: String = chars.iter().take(take).collect();
        let candidate = format!("{}…/", prefix);
        let cand_len = char_width(&candidate);
        if cand_len < last_len {
            variants.push(candidate);
            last_len = cand_len;
        }
    }

    if len > 1 {
        let mut short = String::new();
        short.push(chars[0]);
        short.push('/');
        let short_len = char_width(&short);
        if short_len < last_len {
            variants.push(short.clone());
            last_len = short_len;
        }
    }

    let ellipsis = "…/".to_string();
    if char_width(&ellipsis) < last_len {
        variants.push(ellipsis);
    }

    variants
}

fn combine_segments(segments: &[PathSegment], last: &str) -> String {
    let mut out = String::new();
    for seg in segments {
        out.push_str(seg.current());
    }
    out.push_str(last);
    out
}

fn list_visible_height(total_height: u16) -> usize {
    usize::from(total_height.saturating_sub(4).max(1))
}

fn view_visible_height(total_height: u16) -> usize {
    usize::from(total_height.saturating_sub(4).max(1))
}

fn adjust_list_window(app: &mut App, view_height: usize) {
    if app.rows.is_empty() {
        app.selected = 0;
        app.top = 0;
        return;
    }
    if app.selected >= app.rows.len() {
        app.selected = app.rows.len() - 1;
    }
    let max_top = app.rows.len().saturating_sub(view_height);
    let desired_top = app.selected.saturating_sub(view_height.saturating_sub(1));
    app.top = desired_top.min(max_top);
}

fn set_view_scroll(app: &mut App, value: usize, view_height: usize) {
    let max_scroll = app
        .view_lines
        .len()
        .saturating_sub(view_height)
        .min(u16::MAX as usize);
    let capped = value.min(max_scroll);
    app.view_scroll = capped as u16;
}

fn render_message_lines(
    msgs: &[Message],
    show_tools: bool,
    expand_tools: bool,
) -> (Vec<Line<'static>>, Vec<String>) {
    let mut v = Vec::new();
    let mut t = Vec::new();
    for m in msgs {
        let (mut lines, mut txts) = lines_for_message(m, show_tools, expand_tools);
        v.append(&mut lines);
        t.append(&mut txts);
    }
    (v, t)
}

fn refresh_view_after_tool_toggle(
    app: &mut App,
    terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
) {
    // Get the id of the currently viewed conversation
    let id = match app.display_rows.get(app.selected) {
        Some(DisplayRow::Conversation(row)) => Some(row.id.clone()),
        _ => None,
    };
    if let Some(id) = id {
        if !app.messages_cache.contains_key(&id) {
            if let Ok((_, msgs)) = app.reader.get_messages_by_id(&id) {
                app.messages_cache.insert(id.clone(), msgs);
            }
        }
        if let Some(msgs) = app.messages_cache.get(&id) {
            let (lines, text) =
                render_message_lines(msgs, app.view_show_tools, app.view_expand_tools);
            app.view_lines = lines;
            app.view_text = text;
            if !app.search_query.is_empty() {
                let needle = app.search_query.to_lowercase();
                let matches: Vec<usize> = app
                    .view_text
                    .iter()
                    .enumerate()
                    .filter_map(|(i, line)| {
                        if line.to_lowercase().contains(&needle) {
                            Some(i)
                        } else {
                            None
                        }
                    })
                    .collect();
                if matches.is_empty() {
                    app.search_matches.clear();
                    app.search_index = 0;
                } else {
                    app.search_matches = matches;
                    app.search_index = app.search_index.min(app.search_matches.len() - 1);
                }
            }
            if let Ok(size) = terminal.size() {
                let view_h = view_visible_height(size.height);
                let current = usize::from(app.view_scroll);
                set_view_scroll(app, current, view_h);
            } else if app.view_lines.is_empty() {
                app.view_scroll = 0;
            }
        }
    }
}

fn export_html(title: &str, cwd: &str, msgs: &[Message]) -> Result<std::path::PathBuf> {
    fn esc(s: &str) -> String {
        s.replace('&', "&amp;")
            .replace('<', "&lt;")
            .replace('>', "&gt;")
            .replace('"', "&quot;")
            .replace('\'', "&#39;")
    }

    fn md_to_html(s: &str) -> String {
        let mut opts = Options::empty();
        // Enable commonmark extensions
        opts.insert(Options::ENABLE_TABLES);
        opts.insert(Options::ENABLE_FOOTNOTES);
        opts.insert(Options::ENABLE_STRIKETHROUGH);
        opts.insert(Options::ENABLE_TASKLISTS);
        opts.insert(Options::ENABLE_SMART_PUNCTUATION);
        let parser = Parser::new_ext(s, opts);
        let mut buf = String::new();
        html::push_html(&mut buf, parser);
        buf
    }

    let mut body = String::new();
    for m in msgs {
        let is_tool = m.role.contains("tool");
        if is_tool {
            let mut content = m.content.trim().to_string();
            if let Some(rest) = content.strip_prefix("◀ Tool Result") {
                content = rest.trim_start().to_string();
            }
            // collapse tool results
            let lines = content.lines().count();
            let escaped = esc(&content);
            body.push_str(&format!(
                "<section class=\"msg role-assistant\"><details><summary>Tool Result ({lines} lines)</summary><pre><code>{}</code></pre></details></section>",
                escaped
            ));
        } else {
            let role_title = if m.role == "user" { "You" } else { "Assistant" };
            let rendered = md_to_html(&m.content);
            body.push_str(&format!(
                "<section class=\"msg {}\"><h2>{}</h2><div class=\"content\">{}</div></section>",
                if m.role == "user" {
                    "role-user"
                } else {
                    "role-assistant"
                },
                role_title,
                rendered
            ));
        }
    }
    let doc = format!("<!doctype html><html><head><meta charset='utf-8'><meta name='viewport' content='width=device-width, initial-scale=1'><title>{}</title><style>:root{{color-scheme:light dark}}body{{font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Helvetica,Arial,sans-serif;margin:0;padding:24px;line-height:1.6}}header{{position:sticky;top:0;background:var(--bg,#fff);padding:12px 0 16px 0}}h1{{margin:0 0 4px 0;font-size:20px}}.meta{{color:#666;font-size:12px;margin-bottom:12px}}.msg{{margin:14px 0;padding:12px;border-radius:8px;border:1px solid #ddd}}.role-user{{background:rgba(46,204,113,0.08)}}.role-assistant{{background:rgba(52,152,219,0.08)}}.msg h2{{margin:0 0 8px 0;font-size:14px}}pre,code{{font-family:ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, Liberation Mono, Courier New, monospace}}pre{{background:#111;color:#eee;padding:12px;border-radius:6px;overflow:auto}}.content p{{margin:0 0 10px}}.content ul,.content ol{{padding-left:22px}}.content blockquote{{margin:8px 0;padding-left:10px;border-left:3px solid #ccc;color:#555}}</style></head><body><header><h1>{}</h1><div class='meta'>Path: {}</div></header>{}</body></html>", esc(title), esc(title), esc(cwd), body);
    let out_dir = std::env::current_dir()?.join("exports");
    fs::create_dir_all(&out_dir)?;
    let file = out_dir.join(format!("conversation-{}.html", title.replace(' ', "_")));
    fs::write(&file, doc)?;
    Ok(file)
}

fn render_info_modal(f: &mut ratatui::Frame, area: Rect, app: &App) {
    if app.rows.is_empty() || app.selected >= app.rows.len() {
        return;
    }

    let selected_row = &app.rows[app.selected];

    // Get detailed message info
    let message_count = if let Some(messages) = app.messages_cache.get(&selected_row.id) {
        messages.len()
    } else {
        0 // Unknown, not loaded yet
    };

    // Create modal popup area with adaptive horizontal margins so it can span
    // more of the width. Treat anything under ~180 columns as a "tight"
    // layout so the modal hugs the edges more aggressively there.
    let horizontal_margin = match area.width {
        0..=100 => 1,
        101..=140 => std::cmp::max(1, area.width / 30),
        141..=180 => std::cmp::max(1, area.width / 18),
        _ => std::cmp::max(1, area.width / 10),
    };

    let popup_width = std::cmp::max(
        area.width
            .saturating_sub(horizontal_margin.saturating_mul(2)),
        1,
    );

    let popup_area = Rect {
        x: horizontal_margin,
        y: area.height / 6,
        width: popup_width,
        height: (area.height * 2) / 3,
    };

    // Clear the area first
    f.render_widget(Clear, popup_area);

    // Create the modal content
    let created_date = if let Some(ts) = selected_row.date_ms.checked_div(1000) {
        chrono::DateTime::from_timestamp(ts, 0)
            .map(|dt| {
                dt.with_timezone(&chrono::Local)
                    .format("%Y-%m-%d %H:%M:%S")
                    .to_string()
            })
            .unwrap_or_else(|| "Unknown".to_string())
    } else {
        "Unknown".to_string()
    };

    let last_msg_date =
        if let Some(ts) = selected_row.last_msg_ms.and_then(|ms| ms.checked_div(1000)) {
            chrono::DateTime::from_timestamp(ts, 0)
                .map(|dt| {
                    dt.with_timezone(&chrono::Local)
                        .format("%Y-%m-%d %H:%M:%S")
                        .to_string()
                })
                .unwrap_or_else(|| "Unknown".to_string())
        } else {
            "Unknown".to_string()
        };

    let info_text = format!(
        "Session Information\n\
        \n\
        ID: {}\n\
        Path: {}\n\
        File: {}\n\
        Created: {}\n\
        Last Message: {}\n\
        Message Count: {}\n\
        \n\
        Press ESC to close",
        selected_row.id,
        if selected_row.path.is_empty() {
            "Not set"
        } else {
            &selected_row.path
        },
        selected_row.file_path,
        created_date,
        last_msg_date,
        if message_count > 0 {
            message_count.to_string()
        } else {
            "Not loaded".to_string()
        }
    );

    let modal = Paragraph::new(info_text)
        .block(
            Block::default()
                .title(" ℹ Session Info ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(theme::MODAL_INFO)),
        )
        .wrap(Wrap { trim: true })
        .style(Style::default().bg(theme::SURFACE_DIM).fg(theme::TEXT));

    f.render_widget(modal, popup_area);
}

fn render_help_modal(f: &mut ratatui::Frame, area: Rect, app: &App) {
    let horizontal_margin = match area.width {
        0..=100 => 1,
        101..=140 => std::cmp::max(1, area.width / 30),
        141..=180 => std::cmp::max(1, area.width / 18),
        _ => std::cmp::max(1, area.width / 10),
    };

    let popup_width = std::cmp::max(
        area.width
            .saturating_sub(horizontal_margin.saturating_mul(2)),
        1,
    );

    let popup_area = Rect {
        x: horizontal_margin,
        y: area.height / 6,
        width: popup_width,
        height: (area.height * 2) / 3,
    };

    f.render_widget(Clear, popup_area);

    let list_section_style = if app.mode == Mode::List {
        Style::default()
            .fg(theme::SUCCESS)
            .add_modifier(Modifier::BOLD)
    } else {
        Style::default()
            .fg(theme::TEXT_DIM)
            .add_modifier(Modifier::BOLD)
    };

    let view_section_style = if app.mode == Mode::View {
        Style::default()
            .fg(theme::SUCCESS)
            .add_modifier(Modifier::BOLD)
    } else {
        Style::default()
            .fg(theme::TEXT_DIM)
            .add_modifier(Modifier::BOLD)
    };

    let mut lines: Vec<Line> = Vec::new();
    let shortcut_line = |key: &str, desc: &str| {
        Line::from(vec![
            Span::styled(format!("{:<16}", key), Style::default().fg(theme::PRIMARY)),
            Span::styled(desc.to_string(), Style::default().fg(theme::TEXT_DIM)),
        ])
    };

    lines.push(Line::from(Span::styled(
        "⌨ General",
        Style::default()
            .fg(theme::WARNING)
            .add_modifier(Modifier::BOLD),
    )));
    lines.push(shortcut_line("?", "Show this list of shortcuts"));
    lines.push(shortcut_line("Esc", "Close modal or cancel input"));
    lines.push(shortcut_line("Ctrl+C", "Quit immediately"));
    lines.push(shortcut_line("i", "Show session info for the selection"));
    lines.push(shortcut_line(",", "Open settings modal"));

    lines.push(Line::from(""));
    lines.push(Line::from(Span::styled("📋 List Mode", list_section_style)));
    lines.push(shortcut_line("q", "Quit application"));
    lines.push(shortcut_line("Enter", "Open session / submit list search"));
    lines.push(shortcut_line("↑/↓", "Move selection"));
    lines.push(shortcut_line("PgUp/PgDn", "Page through sessions"));
    lines.push(shortcut_line("g / G", "Jump to first / last session"));
    lines.push(shortcut_line("/", "Start typing to search sessions"));
    lines.push(shortcut_line("s", "Cycle sort key"));
    lines.push(shortcut_line("o", "Toggle sort order"));
    lines.push(shortcut_line("f", "Filter by selected path"));
    lines.push(shortcut_line("x", "Clear filters and search"));
    lines.push(shortcut_line("w", "Toggle filesystem watcher"));
    lines.push(shortcut_line("r", "Resume session in shell"));
    lines.push(shortcut_line("a", "Archive/restore selected session"));
    lines.push(shortcut_line("A", "Toggle archive view"));
    lines.push(shortcut_line("d", "Duplicate selected session"));
    lines.push(shortcut_line("n", "Rename selected session"));
    lines.push(shortcut_line("p", "Change session base directory"));
    lines.push(shortcut_line("u", "Undo last archive/restore"));
    lines.push(shortcut_line("e", "Export session to HTML"));

    lines.push(Line::from(""));
    lines.push(Line::from(Span::styled("💬 View Mode", view_section_style)));
    lines.push(shortcut_line("q / Esc", "Return to session list"));
    lines.push(shortcut_line("↑/↓", "Scroll conversation"));
    lines.push(shortcut_line("PgUp/PgDn", "Page conversation"));
    lines.push(shortcut_line("g / G", "Jump to top / bottom"));
    lines.push(shortcut_line("/", "Search within conversation"));
    lines.push(shortcut_line("Enter", "Jump to first search match"));
    lines.push(shortcut_line("n / N", "Next / previous match"));
    lines.push(shortcut_line("t", "Show or hide tool calls"));
    lines.push(shortcut_line("T", "Expand or collapse tool calls"));
    lines.push(shortcut_line("r", "Resume session in shell"));
    lines.push(shortcut_line("a", "Archive/restore session"));
    lines.push(shortcut_line("A", "Toggle archive view"));
    lines.push(shortcut_line("p", "Change session base directory"));
    lines.push(shortcut_line("u", "Undo last archive/restore"));
    lines.push(shortcut_line("e", "Export session to HTML"));

    lines.push(Line::from(""));
    lines.push(Line::from(Span::styled(
        "Press ? or Esc to close",
        Style::default().fg(theme::TEXT_MUTED),
    )));

    let modal = Paragraph::new(lines)
        .block(
            Block::default()
                .title(" ⌨ Keyboard Shortcuts ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(theme::MODAL_HELP)),
        )
        .wrap(Wrap { trim: true })
        .style(Style::default().bg(theme::SURFACE_DIM).fg(theme::TEXT));

    f.render_widget(modal, popup_area);
}

fn render_settings_modal(f: &mut ratatui::Frame, area: Rect, app: &App) {
    // Create modal popup area (centered, 60% width, 50% height)
    let popup_area = Rect {
        x: area.width / 5,
        y: area.height / 4,
        width: (area.width * 3) / 5,
        height: area.height / 2,
    };

    // Clear the area first
    f.render_widget(Clear, popup_area);

    // Create lines for the settings
    let mut lines = vec![
        Line::from(Span::styled(
            "Configuration",
            Style::default().fg(theme::WARNING).add_modifier(Modifier::BOLD),
        )),
        Line::from(""),
    ];

    // Claude Command field
    let claude_indicator = if app.settings_selected_field == 0 {
        "▸ "
    } else {
        "  "
    };
    let claude_field = if app.settings_selected_field == 0 {
        format!(
            "{}Claude Command: [{}]",
            claude_indicator, app.settings_claude_command
        )
    } else {
        format!(
            "{}Claude Command: {}",
            claude_indicator, app.settings_claude_command
        )
    };
    lines.push(Line::from(Span::styled(
        claude_field,
        if app.settings_selected_field == 0 {
            Style::default().fg(theme::PRIMARY)
        } else {
            Style::default().fg(theme::TEXT_DIM)
        },
    )));

    lines.push(Line::from(""));

    // Quit after launch field
    let quit_indicator = if app.settings_selected_field == 1 {
        "▸ "
    } else {
        "  "
    };
    let quit_text = if app.settings_quit_after_launch {
        "Yes"
    } else {
        "No"
    };
    let quit_field = if app.settings_selected_field == 1 {
        format!("{}Quit after launch: [{}]", quit_indicator, quit_text)
    } else {
        format!("{}Quit after launch: {}", quit_indicator, quit_text)
    };
    lines.push(Line::from(Span::styled(
        quit_field,
        if app.settings_selected_field == 1 {
            Style::default().fg(theme::PRIMARY)
        } else {
            Style::default().fg(theme::TEXT_DIM)
        },
    )));

    lines.push(Line::from(""));
    lines.push(Line::from(Span::styled(
        "↑/↓ navigate  Enter save  Esc cancel",
        Style::default().fg(theme::TEXT_MUTED),
    )));

    let modal = Paragraph::new(lines)
        .block(
            Block::default()
                .title(" ⚙ Settings ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(theme::MODAL_SETTINGS)),
        )
        .wrap(Wrap { trim: true })
        .style(Style::default().bg(theme::SURFACE_DIM).fg(theme::TEXT));

    f.render_widget(modal, popup_area);
}

fn resume_claude_session(claude_command: &str, session_id: &str, session_path: &str) -> Result<()> {
    use std::process::Command;

    // Parse the command string to handle switches like "claude --my-switch"
    let command_parts: Vec<&str> = claude_command.split_whitespace().collect();
    if command_parts.is_empty() {
        return Err(anyhow::anyhow!("Empty command string"));
    }

    let program = command_parts[0];
    let mut cmd = Command::new(program);

    // Add any existing arguments from the command string
    if command_parts.len() > 1 {
        cmd.args(&command_parts[1..]);
    }

    // Set working directory for the command if session path exists
    if !session_path.is_empty() && std::path::Path::new(session_path).exists() {
        cmd.current_dir(session_path);
    }

    // Add the --resume arguments
    cmd.arg("--resume").arg(session_id);

    let status = cmd.status();

    match status {
        Ok(exit_status) => {
            // Print informational message regardless of exit status
            let full_command = format!("{} --resume {}", claude_command, session_id);

            let directory =
                if !session_path.is_empty() && std::path::Path::new(session_path).exists() {
                    session_path
                } else {
                    "current directory"
                };

            println!(
                "Ran `{}` for session {} in directory {}",
                full_command, session_id, directory
            );

            if exit_status.success() {
                Ok(())
            } else {
                Err(anyhow::anyhow!(
                    "Claude command exited with status: {}",
                    exit_status.code().unwrap_or(-1)
                ))
            }
        }
        Err(e) => Err(anyhow::anyhow!("Failed to execute claude command: {}", e)),
    }
}

fn print_shell_init(shell: &str) {
    let binary_name = std::env::current_exe()
        .ok()
        .and_then(|p| p.file_name().map(|s| s.to_string_lossy().to_string()))
        .unwrap_or_else(|| "cc-history".to_string());

    match shell {
        "zsh" | "bash" => {
            // Use a function that wraps the real binary
            // The binary writes to a temp file when it wants to change directory
            println!(
                r#"# cc-history shell integration
# Add this to your ~/.{shell}rc:
#   eval "$(cc-history init {shell})"

cc-history() {{
    local cd_file="/tmp/cc-history-cd.$$"
    local exit_code

    # Run the real binary with the CD file path
    __CC_HISTORY_CD_FILE__="$cd_file" command {binary} "$@"
    exit_code=$?

    # Check if the binary wrote a directory to cd to
    if [[ -f "$cd_file" ]]; then
        local cd_path="$(cat "$cd_file")"
        rm -f "$cd_file"
        if [[ -n "$cd_path" && -d "$cd_path" ]]; then
            cd "$cd_path"
        fi
    fi

    return $exit_code
}}"#,
                shell = shell,
                binary = binary_name,
            );
        }
        "fish" => {
            println!(
                r#"# cc-history shell integration for fish
# Add this to your ~/.config/fish/config.fish:
#   cc-history init fish | source

function cc-history
    set -l cd_file "/tmp/cc-history-cd.$fish_pid"
    set -lx __CC_HISTORY_CD_FILE__ "$cd_file"

    command {binary} $argv
    set -l exit_code $status

    # Check if the binary wrote a directory to cd to
    if test -f "$cd_file"
        set -l cd_path (cat "$cd_file")
        rm -f "$cd_file"
        if test -n "$cd_path" -a -d "$cd_path"
            cd $cd_path
        end
    end

    return $exit_code
end"#,
                binary = binary_name,
            );
        }
        _ => {
            eprintln!(
                "Unsupported shell: {}. Supported shells: zsh, bash, fish",
                shell
            );
            std::process::exit(1);
        }
    }
}

fn setup_demo_data() -> Result<(TempDir, PathBuf)> {
    let temp_dir = tempfile::Builder::new()
        .prefix("cc-history-demo-")
        .tempdir()
        .context("Failed to create temp directory for demo data")?;
    
    let projects_dir = temp_dir.path().join("projects");
    fs::create_dir_all(&projects_dir)?;
    
    let now = chrono::Utc::now().timestamp_millis();
    
    // Define demo conversations with real tech references and humor
    let conversations: Vec<(&str, &str, i64, &str, Option<&str>)> = vec![
        // Web/React conversations
        ("-Users-demo-react-app", "a1b2c3d4-e5f6-7890-abcd-ef1234567890", now - 3600000, "/Users/demo/react-app", Some("Created a TypeScript React Button component")),
        ("-Users-demo-react-app", "b2c3d4e5-f6a7-8901-bcde-f23456789012", now - 7200000, "/Users/demo/react-app", Some("Fixed useState hook infinite loop")),
        ("-Users-demo-nextjs-site", "c3d4e5f6-a7b8-9012-cdef-345678901234", now - 10800000, "/Users/demo/nextjs-site", Some("Set up Next.js 14 app router with server components")),
        ("-Users-demo-nextjs-site", "d4e5f6a7-b8c9-0123-def4-456789012345", now - 14400000, "/Users/demo/nextjs-site", Some("Debugged hydration mismatch in Next.js")),
        ("-Users-demo-vue-app", "e5f6a7b8-c9d0-1234-ef56-789012345678", now - 18000000, "/Users/demo/vue-app", Some("Migrated from Vue 2 to Vue 3 Composition API")),
        ("-Users-demo-svelte-kit", "f6a7b8c9-d0e1-2345-f678-901234567890", now - 21600000, "/Users/demo/svelte-kit", Some("Built SvelteKit form with validation")),
        
        // Rust conversations
        ("-Users-demo-rust-cli", "a7b8c9d0-e1f2-3456-789a-bcdef0123456", now - 86400000 * 2, "/Users/demo/rust-cli", Some("Improved error handling in Rust code using Result types")),
        ("-Users-demo-rust-cli", "b8c9d0e1-f2a3-4567-89ab-cdef01234567", now - 86400000 * 2 - 3600000, "/Users/demo/rust-cli", Some("Added clap for CLI argument parsing")),
        ("-Users-demo-tokio-async", "c9d0e1f2-a3b4-5678-9abc-def012345678", now - 86400000 * 3, "/Users/demo/tokio-async", Some("Implemented async HTTP server with tokio")),
        ("-Users-demo-tokio-async", "d0e1f2a3-b4c5-6789-abcd-ef0123456789", now - 86400000 * 3 - 7200000, "/Users/demo/tokio-async", Some("Fixed deadlock in tokio spawn_blocking")),
        ("-Users-demo-serde-json", "e1f2a3b4-c5d6-789a-bcde-f01234567890", now - 86400000 * 4, "/Users/demo/serde-json", Some("Parsed JSON with serde and custom deserializer")),
        
        // Python conversations
        ("-Users-demo-fastapi-backend", "f2a3b4c5-d6e7-89ab-cdef-012345678901", now - 86400000, "/Users/demo/fastapi-backend", Some("Created FastAPI endpoint with Pydantic validation")),
        ("-Users-demo-fastapi-backend", "a3b4c5d6-e7f8-9abc-def0-123456789012", now - 86400000 - 1800000, "/Users/demo/fastapi-backend", Some("Added async database queries with SQLAlchemy")),
        ("-Users-demo-pandas-analysis", "b4c5d6e7-f8a9-bcde-f012-345678901234", now - 86400000 * 3, "/Users/demo/pandas-analysis", Some("Processed CSV data using pandas for filtering and aggregation")),
        ("-Users-demo-pandas-analysis", "c5d6e7f8-a9b0-cdef-0123-456789012345", now - 86400000 * 3 - 3600000, "/Users/demo/pandas-analysis", Some("Created pivot table and groupby aggregations")),
        ("-Users-demo-django-cms", "d6e7f8a9-b0c1-def0-1234-567890123456", now - 86400000 * 5, "/Users/demo/django-cms", Some("Built Django admin interface for content management")),
        ("-Users-demo-django-cms", "e7f8a9b0-c1d2-ef01-2345-678901234567", now - 86400000 * 5 - 7200000, "/Users/demo/django-cms", Some("Fixed N+1 query problem with select_related")),
        
        // JavaScript/Node conversations
        ("-Users-demo-node-api", "f8a9b0c1-d2e3-f012-3456-789012345678", now - 86400000 * 2, "/Users/demo/node-api", Some("Implemented API data fetching with comprehensive error handling")),
        ("-Users-demo-node-api", "a9b0c1d2-e3f4-0123-4567-890123456789", now - 86400000 * 2 - 5400000, "/Users/demo/node-api", Some("Added Express middleware for request logging")),
        ("-Users-demo-webpack-config", "b0c1d2e3-f4a5-1234-5678-901234567890", now - 86400000 * 6, "/Users/demo/webpack-config", Some("Configured webpack 5 with code splitting")),
        ("-Users-demo-webpack-config", "c1d2e3f4-a5b6-2345-6789-012345678901", now - 86400000 * 6 - 10800000, "/Users/demo/webpack-config", Some("Fixed 'Module not found' errors in webpack build")),
        ("-Users-demo-typescript-lib", "d2e3f4a5-b6c7-3456-7890-123456789012", now - 86400000 * 4, "/Users/demo/typescript-lib", Some("Created TypeScript library with proper type exports")),
        
        // Go conversations
        ("-Users-demo-go-microservice", "e3f4a5b6-c7d8-4567-8901-234567890123", now - 86400000 * 7, "/Users/demo/go-microservice", Some("Built Go microservice with gin router")),
        ("-Users-demo-go-microservice", "f4a5b6c7-d8e9-5678-9012-345678901234", now - 86400000 * 7 - 14400000, "/Users/demo/go-microservice", Some("Implemented graceful shutdown with context cancellation")),
        ("-Users-demo-goroutines", "a5b6c7d8-e9f0-6789-0123-456789012345", now - 86400000 * 8, "/Users/demo/goroutines", Some("Fixed race condition in goroutine pool")),
        
        // Database/SQL conversations
        ("-Users-demo-postgres-optimization", "b6c7d8e9-f0a1-7890-1234-567890123456", now - 86400000 * 5, "/Users/demo/postgres-optimization", Some("Optimized SQL queries with indexing strategies")),
        ("-Users-demo-postgres-optimization", "c7d8e9f0-a1b2-8901-2345-678901234567", now - 86400000 * 5 - 1800000, "/Users/demo/postgres-optimization", Some("Added EXPLAIN ANALYZE to slow query investigation")),
        ("-Users-demo-redis-cache", "d8e9f0a1-b2c3-9012-3456-789012345678", now - 86400000 * 4, "/Users/demo/redis-cache", Some("Implemented Redis caching layer for API responses")),
        ("-Users-demo-mongodb-aggregation", "e9f0a1b2-c3d4-0123-4567-890123456789", now - 86400000 * 6, "/Users/demo/mongodb-aggregation", Some("Created MongoDB aggregation pipeline for analytics")),
        
        // DevOps/Infrastructure
        ("-Users-demo-docker-compose", "f0a1b2c3-d4e5-1234-5678-901234567890", now - 86400000 * 9, "/Users/demo/docker-compose", Some("Set up docker-compose with postgres and redis services")),
        ("-Users-demo-k8s-deployment", "a1b2c3d4-e5f6-2345-6789-012345678901", now - 86400000 * 10, "/Users/demo/k8s-deployment", Some("Created Kubernetes deployment YAML with health checks")),
        ("-Users-demo-ci-cd-github", "b2c3d4e5-f6a7-3456-7890-123456789012", now - 86400000 * 11, "/Users/demo/ci-cd-github", Some("Configured GitHub Actions workflow for CI/CD")),
        ("-Users-demo-terraform-aws", "c3d4e5f6-a7b8-4567-8901-234567890123", now - 86400000 * 12, "/Users/demo/terraform-aws", Some("Provisioned AWS infrastructure with Terraform")),
        
        // Mobile
        ("-Users-demo-react-native", "d4e5f6a7-b8c9-5678-9012-345678901234", now - 86400000 * 13, "/Users/demo/react-native", Some("Built React Native navigation with React Navigation")),
        ("-Users-demo-swiftui-app", "e5f6a7b8-c9d0-6789-0123-456789012345", now - 86400000 * 14, "/Users/demo/swiftui-app", Some("Created SwiftUI view with Combine for state management")),
        ("-Users-demo-kotlin-android", "f6a7b8c9-d0e1-7890-1234-567890123456", now - 86400000 * 15, "/Users/demo/kotlin-android", Some("Implemented RecyclerView adapter with DiffUtil")),
        
        // Testing
        ("-Users-demo-jest-tests", "a7b8c9d0-e1f2-8901-2345-678901234567", now - 86400000 * 16, "/Users/demo/jest-tests", Some("Wrote Jest unit tests with mocking")),
        ("-Users-demo-pytest-fixtures", "b8c9d0e1-f2a3-9012-3456-789012345678", now - 86400000 * 17, "/Users/demo/pytest-fixtures", Some("Created pytest fixtures for database testing")),
        ("-Users-demo-rust-tests", "c9d0e1f2-a3b4-0123-4567-890123456789", now - 86400000 * 18, "/Users/demo/rust-tests", Some("Added property-based tests with proptest")),
        
        // More diverse topics
        ("-Users-demo-graphql-api", "d0e1f2a3-b4c5-1234-5678-901234567890", now - 86400000 * 19, "/Users/demo/graphql-api", Some("Built GraphQL API with Apollo Server")),
        ("-Users-demo-websocket-chat", "e1f2a3b4-c5d6-2345-6789-012345678901", now - 86400000 * 20, "/Users/demo/websocket-chat", Some("Implemented WebSocket chat with Socket.io")),
        ("-Users-demo-elasticsearch", "f2a3b4c5-d6e7-3456-7890-123456789012", now - 86400000 * 21, "/Users/demo/elasticsearch", Some("Set up Elasticsearch indexing with proper mappings")),
        ("-Users-demo-rabbitmq-worker", "a3b4c5d6-e7f8-4567-8901-234567890123", now - 86400000 * 22, "/Users/demo/rabbitmq-worker", Some("Created RabbitMQ worker with retry logic")),
        ("-Users-demo-prometheus-metrics", "b4c5d6e7-f8a9-5678-9012-345678901234", now - 86400000 * 23, "/Users/demo/prometheus-metrics", Some("Added Prometheus metrics to Go service")),
        ("-Users-demo-grafana-dashboard", "c5d6e7f8-a9b0-6789-0123-456789012345", now - 86400000 * 24, "/Users/demo/grafana-dashboard", Some("Created Grafana dashboard for monitoring")),
        ("-Users-demo-nginx-config", "d6e7f8a9-b0c1-7890-1234-567890123456", now - 86400000 * 25, "/Users/demo/nginx-config", Some("Configured nginx reverse proxy with SSL")),
        ("-Users-demo-certbot-ssl", "e7f8a9b0-c1d2-8901-2345-678901234567", now - 86400000 * 26, "/Users/demo/certbot-ssl", Some("Set up Let's Encrypt SSL with certbot")),
        ("-Users-demo-git-hooks", "f8a9b0c1-d2e3-9012-3456-789012345678", now - 86400000 * 27, "/Users/demo/git-hooks", Some("Created pre-commit hooks with husky")),
        ("-Users-demo-eslint-config", "a9b0c1d2-e3f4-0123-4567-890123456789", now - 86400000 * 28, "/Users/demo/eslint-config", Some("Configured ESLint with TypeScript rules")),
        ("-Users-demo-prettier-format", "b0c1d2e3-f4a5-1234-5678-901234567890", now - 86400000 * 29, "/Users/demo/prettier-format", Some("Set up Prettier for code formatting")),
    ];
    
    // Generate conversations
    for (proj_dir, id, timestamp, cwd, summary) in conversations {
        let proj_path = projects_dir.join(proj_dir);
        fs::create_dir_all(&proj_path)?;
        let file_path = proj_path.join(format!("{}.jsonl", id));
        let mut file = fs::File::create(&file_path)?;
        
        // Generate humorous user messages based on project type
        let (user_msg, assistant_msg) = match proj_dir {
            s if s.contains("react") => (
                "Why is my React component re-rendering 47 times?",
                "Ah, the classic 'infinite re-render' problem! You're probably updating state in render. Here's the fix:\n\n```tsx\n// ❌ Don't do this\nfunction Component() {\n  const [count, setCount] = useState(0);\n  setCount(count + 1); // This causes infinite loop!\n  return <div>{count}</div>;\n}\n\n// ✅ Do this instead\nfunction Component() {\n  const [count, setCount] = useState(0);\n  useEffect(() => {\n    setCount(prev => prev + 1);\n  }, []); // Only run once\n  return <div>{count}</div>;\n}\n```"
            ),
            s if s.contains("nextjs") => (
                "My Next.js page is showing 'use client' errors everywhere",
                "The 'use client' directive is required when using hooks or browser APIs. Make sure you have it at the top:\n\n```tsx\n'use client';\n\nimport { useState } from 'react';\n\nexport default function Page() {\n  const [state, setState] = useState(0);\n  return <div>{state}</div>;\n}\n```\n\nServer components can't use hooks - that's the trade-off for server-side rendering!"
            ),
            s if s.contains("rust") => (
                "The borrow checker is being mean to me again",
                "The borrow checker is just trying to prevent memory safety bugs! Here's a common pattern:\n\n```rust\n// ❌ This won't compile\nlet mut vec = vec![1, 2, 3];\nlet first = &vec[0];\nvec.push(4); // Error: cannot borrow as mutable\n\n// ✅ Do this instead\nlet mut vec = vec![1, 2, 3];\nlet first = vec[0]; // Copy the value\nvec.push(4); // Now this works!\n```\n\nRemember: Rust's ownership system prevents data races at compile time!"
            ),
            s if s.contains("tokio") => (
                "My tokio tasks are spawning but never completing",
                "Check your `.await` calls! Tokio tasks run concurrently, but you need to await them:\n\n```rust\n// ❌ This spawns but doesn't wait\ntokio::spawn(async {\n    do_work().await;\n});\n\n// ✅ Store the handle and await it\nlet handle = tokio::spawn(async {\n    do_work().await;\n});\nhandle.await?;\n```"
            ),
            s if s.contains("fastapi") => (
                "FastAPI is returning 422 validation errors",
                "Pydantic is strict about types! Make sure your request body matches your model:\n\n```python\nfrom pydantic import BaseModel\n\nclass User(BaseModel):\n    name: str\n    age: int\n\n@app.post(\"/users\")\nasync def create_user(user: User):\n    return user\n```\n\nSend `{\"name\": \"Alice\", \"age\": 30}` not `{\"name\": \"Alice\", \"age\": \"30\"}` - age must be an int!"
            ),
            s if s.contains("pandas") => (
                "Pandas is giving me SettingWithCopyWarning again",
                "The classic pandas gotcha! Use `.copy()` or `.loc`:\n\n```python\n# ❌ This triggers warning\ndf[df['age'] > 18]['name'] = 'Adult'\n\n# ✅ Do this instead\ndf.loc[df['age'] > 18, 'name'] = 'Adult'\n# or\ndf_filtered = df[df['age'] > 18].copy()\ndf_filtered['name'] = 'Adult'\n```"
            ),
            s if s.contains("docker") => (
                "Docker build is taking forever",
                "Optimize your Dockerfile with multi-stage builds and layer caching:\n\n```dockerfile\n# Use specific tags, not 'latest'\nFROM node:20-alpine AS builder\nWORKDIR /app\nCOPY package*.json ./\nRUN npm ci --only=production\nCOPY . .\nRUN npm run build\n\nFROM node:20-alpine\nWORKDIR /app\nCOPY --from=builder /app/dist ./dist\nCOPY --from=builder /app/node_modules ./node_modules\nCMD [\"node\", \"dist/index.js\"]\n```\n\nLayer caching means unchanged dependencies won't rebuild!"
            ),
            s if s.contains("k8s") => (
                "My Kubernetes pod keeps crashing",
                "Check your resource limits and health checks:\n\n```yaml\nresources:\n  requests:\n    memory: \"128Mi\"\n    cpu: \"100m\"\n  limits:\n    memory: \"256Mi\"\n    cpu: \"200m\"\nlivenessProbe:\n  httpGet:\n    path: /health\n    port: 8080\n  initialDelaySeconds: 30\n```\n\nOOMKilled means you need more memory limits!"
            ),
            s if s.contains("webpack") => (
                "Webpack bundle size is 5MB, help!",
                "Use code splitting and analyze your bundle:\n\n```js\n// Dynamic imports for code splitting\nconst Component = lazy(() => import('./Component'));\n\n// Analyze bundle\nnpm install --save-dev webpack-bundle-analyzer\n```\n\nCheck what's actually in that bundle - you might be importing entire libraries!"
            ),
            s if s.contains("postgres") => (
                "My SQL query takes 30 seconds",
                "Add indexes and use EXPLAIN ANALYZE:\n\n```sql\n-- See what's slow\nEXPLAIN ANALYZE SELECT * FROM users WHERE email = 'test@example.com';\n\n-- Add index\nCREATE INDEX idx_users_email ON users(email);\n\n-- Avoid SELECT *\nSELECT id, name FROM users WHERE email = 'test@example.com';\n```"
            ),
            s if s.contains("redis") => (
                "Redis cache isn't working",
                "Check TTL and serialization:\n\n```python\nimport redis\nimport json\n\nr = redis.Redis()\n# Set with expiration\nr.setex('key', 3600, json.dumps(data))\n# Get\ncached = json.loads(r.get('key'))\n```\n\nMake sure you're setting TTLs or your cache will grow forever!"
            ),
            s if s.contains("graphql") => (
                "GraphQL N+1 query problem",
                "Use DataLoader to batch queries:\n\n```js\nconst DataLoader = require('dataloader');\nconst userLoader = new DataLoader(async (ids) => {\n  const users = await db.users.findByIds(ids);\n  return ids.map(id => users.find(u => u.id === id));\n});\n```\n\nThis batches multiple user fetches into one query!"
            ),
            s if s.contains("websocket") => (
                "WebSocket keeps disconnecting",
                "Add reconnection logic and heartbeat:\n\n```js\nconst ws = new WebSocket('ws://localhost:8080');\nws.onclose = () => {\n  setTimeout(() => {\n    // Reconnect\n    connectWebSocket();\n  }, 1000);\n};\n\n// Heartbeat\nsetInterval(() => ws.send('ping'), 30000);\n```"
            ),
            s if s.contains("eslint") => (
                "ESLint is complaining about everything",
                "Configure it properly in `.eslintrc.js`:\n\n```js\nmodule.exports = {\n  extends: ['eslint:recommended', 'plugin:@typescript-eslint/recommended'],\n  rules: {\n    '@typescript-eslint/no-explicit-any': 'warn',\n    'no-console': 'off', // Or 'warn' for production\n  },\n};\n```\n\nYou can disable specific rules per line with `// eslint-disable-next-line`"
            ),
            _ => (
                "How do I fix this bug?",
                "Let me help you debug this. Can you share the error message and stack trace?"
            ),
        };
        
        writeln!(file, r#"{{"type":"user","message":{{"role":"user","content":"{}"}},"timestamp":{},"cwd":"{}"}}"#, user_msg, timestamp, cwd)?;
        writeln!(file, r#"{{"type":"assistant","message":{{"role":"assistant","content":[{{"type":"text","text":"{}"}}]}},"timestamp":{}}}"#, assistant_msg, timestamp + 2000)?;
        if let Some(s) = summary {
            writeln!(file, r#"{{"type":"summary","summary":"{}"}}"#, s)?;
        }
    }
    
    Ok((temp_dir, projects_dir))
}

fn main() -> Result<()> {
    // Optional override for projects directory
    let args: Vec<String> = std::env::args().collect();

    // Handle init subcommand for shell integration
    if let Some(idx) = args.iter().position(|a| a == "init") {
        if let Some(shell) = args.get(idx + 1) {
            print_shell_init(shell);
            return Ok(());
        } else {
            eprintln!("Usage: cc-history init <shell>");
            eprintln!("Supported shells: zsh, bash, fish");
            std::process::exit(1);
        }
    }

    // Handle --demo flag: create demo data in temp directory
    let (_demo_temp_dir, demo_projects_override): (Option<TempDir>, Option<PathBuf>) = 
        if args.iter().any(|a| a == "--demo") {
            let (temp_dir, demo_path) = setup_demo_data()?;
            // Store temp_dir to keep it alive for the duration of the program
            (Some(temp_dir), Some(demo_path))
        } else {
            (None, None)
        };

    let mut projects_override: Option<PathBuf> = demo_projects_override;
    let mut i = 0;
    while i < args.len() {
        if args[i] == "--projects-dir" && i + 1 < args.len() {
            // --projects-dir can override --demo if explicitly provided
            projects_override = Some(PathBuf::from(args[i + 1].clone()));
            i += 1;
        }
        i += 1;
    }
    let reader = if let Some(dir) = projects_override.clone() {
        ChatReader::with_projects_dir(dir)
    } else {
        ChatReader::new()
    };

    if args.iter().any(|a| a == "--oldest-path") {
        let mut list = reader.list_conversations()?;
        if list.is_empty() {
            println!("No conversations found.");
            return Ok(());
        }
        sort_conversations_by_earliest(&mut list);
        let pick = &list[0];
        println!("{}", pick.file.display());
        return Ok(());
    }
    if args.iter().any(|a| a == "--oldest") {
        let mut list = reader.list_conversations()?;
        if list.is_empty() {
            println!("No conversations found.");
            return Ok(());
        }
        sort_conversations_by_earliest(&mut list);
        let pick = &list[0];
        let (_, messages) = reader.get_messages_by_id(&pick.id)?;
        for m in messages {
            println!("{}:\n\n{}\n", m.role, m.content);
        }
        return Ok(());
    }

    if args.iter().any(|a| a == "--export-oldest") {
        let mut list = reader.list_conversations()?;
        if list.is_empty() {
            println!("No conversations found.");
            return Ok(());
        }
        sort_conversations_by_earliest(&mut list);
        let pick = &list[0];
        let (meta, messages) = reader.get_messages_by_id(&pick.id)?;
        let path = export_html(&pick.id, &meta.path.unwrap_or_default(), &messages)?;
        println!("Exported: {}", path.display());
        return Ok(());
    }

    if let Some(id) = args
        .iter()
        .position(|a| a == "--export-id")
        .and_then(|idx| args.get(idx + 1))
        .cloned()
    {
        let (meta, messages) = reader.get_messages_by_id(&id)?;
        let path = export_html(&id, &meta.path.unwrap_or_default(), &messages)?;
        println!("Exported: {}", path.display());
        return Ok(());
    }

    // Build app before entering alt screen to avoid blank wait
    let projects_dir = projects_override.unwrap_or_else(|| {
        dirs::home_dir()
            .unwrap_or_else(|| PathBuf::from(""))
            .join(".claude")
            .join("projects")
    });
    let mut app = App::new_with_projects(projects_dir)?;

    enable_raw_mode()?;
    let mut stdout = io::stdout();
    crossterm::execute!(
        stdout,
        crossterm::terminal::EnterAlternateScreen,
        crossterm::event::EnableMouseCapture
    )?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let tick_rate = Duration::from_millis(200);
    let mut needs_redraw = true;

    loop {
        // Apply async sort results if available
        if let Some(rx) = &app.sort_rx {
            match rx.try_recv() {
                Ok(new_rows) => {
                    app.rows = new_rows;
                    app.sort_rx = None;
                    app.sorting_in_progress = false;
                    app.rebuild_display_rows();
                    needs_redraw = true;
                }
                Err(TryRecvError::Empty) => {}
                Err(TryRecvError::Disconnected) => {
                    app.sort_rx = None;
                    app.sorting_in_progress = false;
                    needs_redraw = true;
                }
            }
        }
        // Apply async list search results if available
        if let Some(rx) = &app.list_search_rx {
            match rx.try_recv() {
                Ok(new_rows) => {
                    app.rows = new_rows;
                    app.list_search_rx = None;
                    app.list_search_in_progress = false;
                    app.selected = 0;
                    app.top = 0;
                    app.rebuild_display_rows();
                    needs_redraw = true;
                }
                Err(TryRecvError::Empty) => {}
                Err(TryRecvError::Disconnected) => {
                    app.list_search_rx = None;
                    app.list_search_in_progress = false;
                    needs_redraw = true;
                }
            }
        }
        // Apply async full refresh results
        if let Some(rx) = &app.update_rx {
            match rx.try_recv() {
                Ok(new_all) => {
                    // Filter out any sessions whose files no longer exist (may have been
                    // archived/deleted between when the refresh started and now)
                    let mut new_rows: Vec<RowItem> = new_all
                        .into_iter()
                        .filter(|r| Path::new(&r.file_path).exists())
                        .collect();

                    // Preserve first_msg values that were loaded asynchronously.
                    // The refresh only has summary-based first_msg, so we keep
                    // any non-empty first_msg from the old rows.
                    for new_row in &mut new_rows {
                        if new_row.first_msg.is_empty() {
                            if let Some(old_row) = app.all_rows.iter().find(|r| r.id == new_row.id)
                            {
                                if !old_row.first_msg.is_empty() {
                                    new_row.first_msg = old_row.first_msg.clone();
                                }
                            }
                        }
                    }

                    app.all_rows = new_rows;
                    app.update_rx = None;
                    app.updating_in_progress = false;
                    // Reapply search/filter/sort state
                    if app.list_search_query.is_empty() {
                        app.rows = app.all_rows.clone();
                        if let Some(fp) = &app.filter_path {
                            app.rows = app.rows.iter().filter(|r| &r.path == fp).cloned().collect();
                        }
                        // resort current view
                        apply_sort(&mut app);
                        app.restore_list_position();
                    } else {
                        app.clear_pending_list_position();
                        // run search again on fresh data
                        spawn_list_search(&mut app);
                        app.selected = 0;
                        app.top = 0;
                    }
                    app.first_msg_pending
                        .retain(|id| app.all_rows.iter().any(|r| &r.id == id));
                    needs_redraw = true;
                }
                Err(TryRecvError::Empty) => {}
                Err(TryRecvError::Disconnected) => {
                    app.update_rx = None;
                    app.updating_in_progress = false;
                    needs_redraw = true;
                }
            }
        }
        // Apply async first message results
        // Collect display updates to apply after channel processing (to avoid borrow conflicts)
        let mut display_updates: Vec<(String, String)> = Vec::new();
        let mut channel_disconnected = false;
        if let Some(rx) = &app.first_msg_rx {
            loop {
                match rx.try_recv() {
                    Ok(result) => {
                        let FirstMsgResult {
                            id,
                            first_msg,
                            messages,
                        } = result;
                        app.first_msg_pending.remove(&id);
                        if !messages.is_empty() {
                            app.messages_cache.insert(id.clone(), messages);
                        }
                        if let Some(first) = first_msg {
                            if let Some(row) = app.all_rows.iter_mut().find(|r| r.id == id) {
                                row.first_msg = first.clone();
                            }
                            if let Some(row) = app.rows.iter_mut().find(|r| r.id == id) {
                                row.first_msg = first.clone();
                            }
                            // Queue display_rows update for after the loop
                            display_updates.push((id, first));
                        }
                        needs_redraw = true;
                    }
                    Err(TryRecvError::Empty) => break,
                    Err(TryRecvError::Disconnected) => {
                        channel_disconnected = true;
                        needs_redraw = true;
                        break;
                    }
                }
            }
        }
        if channel_disconnected {
            app.first_msg_rx = None;
        }
        // Apply queued display_rows updates
        for (id, first) in display_updates {
            update_display_row_first_msg(&mut app, &id, &first);
        }
        // React to FS ping - debounce by collecting events and waiting for quiet period
        if let Some(prx) = &app.fs_ping_rx {
            let mut got_events = false;
            let mut ignore_all = false;
            // Drain all pending events
            while let Ok(paths) = prx.try_recv() {
                got_events = true;
                // Check if this is a recently renamed file we should ignore
                if let Some((renamed_path, time)) = &app.last_renamed {
                    if time.elapsed() < Duration::from_secs(2) {
                        if paths.iter().any(|p| p == renamed_path) {
                            ignore_all = true;
                        }
                    } else {
                        app.last_renamed = None;
                    }
                }
            }
            // If we got events, reset the debounce timer (wait 500ms after last event)
            if got_events && !ignore_all {
                app.fs_debounce_until = Some(Instant::now() + Duration::from_millis(500));
            }
        }
        // Check if debounce timer has elapsed and we should refresh
        if let Some(debounce_until) = app.fs_debounce_until {
            if Instant::now() >= debounce_until && !app.updating_in_progress {
                app.fs_debounce_until = None;
                spawn_full_refresh(&mut app);
                needs_redraw = true;
            }
        }

        // Handle resume session request
        if let Some((session_id, session_path)) = app.resume_session_request.take() {
            // Restore terminal state before launching external command
            crossterm::terminal::disable_raw_mode()?;
            crossterm::execute!(
                terminal.backend_mut(),
                crossterm::cursor::Show,
                crossterm::event::DisableMouseCapture,
                crossterm::terminal::LeaveAlternateScreen
            )?;

            // Execute claude command with proper terminal state
            let result =
                resume_claude_session(&app.settings_claude_command, &session_id, &session_path);

            // Handle the result based on quit_after_launch setting
            match result {
                Ok(_) => {
                    if app.settings_quit_after_launch {
                        // Write CD path to temp file for shell integration to pick up
                        // This allows the parent shell to cd to the session's directory
                        if !session_path.is_empty() && std::path::Path::new(&session_path).exists()
                        {
                            if let Ok(cd_file) = std::env::var("__CC_HISTORY_CD_FILE__") {
                                let _ = std::fs::write(&cd_file, &session_path);
                            }
                        }
                        // Command executed successfully, exit the app
                        return Ok(());
                    } else {
                        // Restore terminal state and return to TUI
                        crossterm::terminal::enable_raw_mode()?;
                        crossterm::execute!(
                            terminal.backend_mut(),
                            crossterm::terminal::EnterAlternateScreen,
                            crossterm::event::EnableMouseCapture,
                            crossterm::cursor::Hide,
                            crossterm::terminal::Clear(crossterm::terminal::ClearType::All)
                        )?;
                        terminal.clear()?;
                        needs_redraw = true;
                        // Continue the main loop
                    }
                }
                Err(e) => {
                    // Show error message and exit regardless of setting
                    eprintln!("Failed to resume session: {}", e);
                    return Err(e);
                }
            }
        }

        if needs_redraw {
            // Precompute size for lazy loading of first message previews
            if let Ok(sz) = terminal.size() {
                if let Mode::List = app.mode {
                    ensure_first_msgs(&mut app, sz);
                }
            }
            terminal.draw(|f| {
                let size = f.size();
                match app.mode {
                    Mode::List => render_list(f, size, &app),
                    Mode::View => render_view(f, size, &app),
                }
                if app.show_info_modal {
                    render_info_modal(f, size, &app);
                }
                if app.show_settings_modal {
                    render_settings_modal(f, size, &app);
                }
                if app.show_help_modal {
                    render_help_modal(f, size, &app);
                }
            })?;
            needs_redraw = false;
        }

        if crossterm::event::poll(tick_rate)? {
            match event::read()? {
                Event::Key(KeyEvent {
                    code, modifiers, ..
                }) => {
                    needs_redraw = true;
                    match app.mode {
                        Mode::List => {
                            if app.show_help_modal {
                                match code {
                                    KeyCode::Esc | KeyCode::Char('?') => {
                                        app.show_help_modal = false;
                                    }
                                    _ => {}
                                }
                            } else {
                                match code {
                                    // Quit
                                    KeyCode::Char('c')
                                        if modifiers.contains(KeyModifiers::CONTROL) =>
                                    {
                                        break
                                    }
                                    KeyCode::Char('q') if !app.list_search_input => break,
                                    // Cancel search input or close modals (do not quit)
                                    KeyCode::Esc if app.list_search_input => {
                                        app.list_search_input = false;
                                    }
                                    KeyCode::Esc if app.show_info_modal => {
                                        app.show_info_modal = false;
                                        // Re-enable mouse capture when closing info modal
                                        crossterm::execute!(
                                            terminal.backend_mut(),
                                            crossterm::event::EnableMouseCapture
                                        )?;
                                    }
                                    KeyCode::Esc if app.show_settings_modal => {
                                        // Cancel all changes and close modal
                                        app.show_settings_modal = false;
                                        app.settings_selected_field = 0;
                                        // Reload original values
                                        app.settings_claude_command = load_claude_command_pref();
                                        app.settings_quit_after_launch =
                                            load_quit_after_launch_pref();
                                    }
                                    KeyCode::Char('?') if !app.is_input_active() => {
                                        // Re-enable mouse capture if info modal was open
                                        if app.show_info_modal {
                                            crossterm::execute!(
                                                terminal.backend_mut(),
                                                crossterm::event::EnableMouseCapture
                                            )?;
                                        }
                                        app.show_help_modal = true;
                                        app.show_info_modal = false;
                                        app.show_settings_modal = false;
                                    }
                                    KeyCode::Char('s') if !app.is_input_active() => {
                                        cycle_sort_key(&mut app);
                                        let _ = save_sort_prefs(app.sort_key, app.sort_desc);
                                        spawn_sort(&mut app);
                                    }
                                    KeyCode::Char('o') if !app.is_input_active() => {
                                        app.sort_desc = !app.sort_desc;
                                        let _ = save_sort_prefs(app.sort_key, app.sort_desc);
                                        spawn_sort(&mut app);
                                    }
                                    KeyCode::Char('w') if !app.is_input_active() => {
                                        // toggle watcher
                                        if app.watcher_enabled {
                                            app.fs_ping_rx = None;
                                            app.watcher_enabled = false;
                                        } else {
                                            spawn_fs_watcher(&mut app);
                                            app.watcher_enabled = app.fs_ping_rx.is_some();
                                        }
                                        let _ = save_watcher_pref(app.watcher_enabled);
                                    }
                                    KeyCode::Char('i') if !app.is_input_active() => {
                                        app.show_info_modal = true;
                                        app.show_help_modal = false;
                                        // Disable mouse capture so text can be selected
                                        crossterm::execute!(
                                            terminal.backend_mut(),
                                            crossterm::event::DisableMouseCapture
                                        )?;
                                    }
                                    KeyCode::Char(',') if !app.is_input_active() => {
                                        app.show_help_modal = false;
                                        app.show_settings_modal = true;
                                        app.settings_selected_field = 0;
                                        app.settings_claude_command = load_claude_command_pref();
                                        app.settings_quit_after_launch =
                                            load_quit_after_launch_pref();
                                    }
                                    KeyCode::Char('r') if !app.is_input_active() => {
                                        // Resume: skip if on a group header
                                        if !app.is_group_selected() {
                                            if let Some(DisplayRow::Conversation(row)) =
                                                app.display_rows.get(app.selected)
                                            {
                                                app.resume_session_request =
                                                    Some((row.id.clone(), row.path.clone()));
                                            }
                                        }
                                    }
                                    KeyCode::Char('a') if !app.is_input_active() => {
                                        if app.viewing_archive {
                                            if let Err(err) = unarchive_selected_session(&mut app) {
                                                eprintln!("Failed to restore session: {}", err);
                                            }
                                        } else if let Err(err) = archive_selected_session(&mut app)
                                        {
                                            eprintln!("Failed to archive session: {}", err);
                                        }
                                    }
                                    KeyCode::Char('A') if !app.is_input_active() => {
                                        app.toggle_archive_view();
                                    }
                                    KeyCode::Char('d') if !app.is_input_active() => {
                                        if let Err(err) = duplicate_selected_session(&mut app) {
                                            eprintln!("Failed to duplicate session: {}", err);
                                        }
                                    }
                                    KeyCode::Char('n') if !app.is_input_active() => {
                                        // Rename: skip if on a group header
                                        if !app.is_group_selected() {
                                            if let Some(DisplayRow::Conversation(row)) =
                                                app.display_rows.get(app.selected)
                                            {
                                                app.rename_input = true;
                                                app.rename_query = row.first_msg.clone();
                                            }
                                        }
                                    }
                                    KeyCode::Char('p') if !app.is_input_active() => {
                                        // Repath: skip if on a group header
                                        if !app.is_group_selected() {
                                            if let Some(DisplayRow::Conversation(row)) =
                                                app.display_rows.get(app.selected)
                                            {
                                                app.repath_input = true;
                                                app.repath_query = row.path.clone();
                                            }
                                        }
                                    }
                                    KeyCode::Char('u') if !app.is_input_active() => {
                                        if let Err(err) = undo_last_archive(&mut app) {
                                            eprintln!("Failed to undo archive: {}", err);
                                        }
                                    }
                                    KeyCode::Char('/') if !app.is_input_active() => {
                                        app.list_search_input = true;
                                        app.list_search_query.clear();
                                    }
                                    KeyCode::Enter if app.list_search_input => {
                                        spawn_list_search(&mut app);
                                        app.list_search_input = false;
                                    }
                                    KeyCode::Backspace if app.list_search_input => {
                                        app.list_search_query.pop();
                                    }
                                    KeyCode::Char(c) if app.list_search_input => {
                                        app.list_search_query.push(c);
                                    }
                                    // Rename input handling
                                    KeyCode::Esc if app.rename_input => {
                                        app.rename_input = false;
                                    }
                                    KeyCode::Enter if app.rename_input => {
                                        if let Err(e) = rename_selected_session(&mut app) {
                                            eprintln!("Rename failed: {}", e);
                                        }
                                        app.rename_input = false;
                                    }
                                    KeyCode::Backspace if app.rename_input => {
                                        app.rename_query.pop();
                                    }
                                    KeyCode::Char(c) if app.rename_input => {
                                        app.rename_query.push(c);
                                    }
                                    // Repath input handling
                                    KeyCode::Esc if app.repath_input => {
                                        app.repath_input = false;
                                    }
                                    KeyCode::Enter if app.repath_input => {
                                        if let Err(e) = repath_selected_session(&mut app) {
                                            eprintln!("Repath failed: {}", e);
                                        }
                                        app.repath_input = false;
                                    }
                                    KeyCode::Backspace if app.repath_input => {
                                        app.repath_query.pop();
                                    }
                                    KeyCode::Char(c) if app.repath_input => {
                                        app.repath_query.push(c);
                                    }
                                    // Settings modal input handling
                                    KeyCode::Enter if app.show_settings_modal => {
                                        // Save all settings and close modal
                                        let _ =
                                            save_claude_command_pref(&app.settings_claude_command);
                                        let _ = save_quit_after_launch_pref(
                                            app.settings_quit_after_launch,
                                        );
                                        app.show_settings_modal = false;
                                        app.settings_selected_field = 0;
                                    }
                                    KeyCode::Up if app.show_settings_modal => {
                                        if app.settings_selected_field > 0 {
                                            app.settings_selected_field -= 1;
                                        }
                                    }
                                    KeyCode::Down if app.show_settings_modal => {
                                        if app.settings_selected_field < 1 {
                                            app.settings_selected_field += 1;
                                        }
                                    }
                                    KeyCode::Backspace
                                        if app.show_settings_modal
                                            && app.settings_selected_field == 0 =>
                                    {
                                        app.settings_claude_command.pop();
                                    }
                                    KeyCode::Char(c)
                                        if app.show_settings_modal
                                            && app.settings_selected_field == 0 =>
                                    {
                                        app.settings_claude_command.push(c);
                                    }
                                    KeyCode::Char(' ')
                                    | KeyCode::Char('y')
                                    | KeyCode::Char('n')
                                        if app.show_settings_modal
                                            && app.settings_selected_field == 1 =>
                                    {
                                        app.settings_quit_after_launch =
                                            !app.settings_quit_after_launch;
                                    }
                                    KeyCode::Up if !app.show_settings_modal => {
                                        if app.selected > 0 {
                                            app.selected -= 1;
                                            if app.selected < app.top {
                                                app.top = app.selected;
                                            }
                                        }
                                    }
                                    KeyCode::Down if !app.show_settings_modal => {
                                        let row_count = app.display_rows.len();
                                        if app.selected + 1 < row_count {
                                            app.selected += 1;
                                            if let Ok(size) = terminal.size() {
                                                let view_h = list_visible_height(size.height);
                                                if app.selected >= app.top + view_h {
                                                    app.top = app
                                                        .selected
                                                        .saturating_sub(view_h.saturating_sub(1));
                                                }
                                            }
                                        }
                                    }
                                    KeyCode::PageUp => {
                                        if let Ok(size) = terminal.size() {
                                            let view_h = list_visible_height(size.height);
                                            app.selected = app.selected.saturating_sub(view_h);
                                            adjust_list_window(&mut app, view_h);
                                        }
                                    }
                                    KeyCode::PageDown => {
                                        if let Ok(size) = terminal.size() {
                                            let view_h = list_visible_height(size.height);
                                            let last_index =
                                                app.display_rows.len().saturating_sub(1);
                                            app.selected = (app.selected + view_h).min(last_index);
                                            adjust_list_window(&mut app, view_h);
                                        }
                                    }
                                    KeyCode::Home | KeyCode::Char('g')
                                        if !app.is_input_active() =>
                                    {
                                        app.selected = 0;
                                        app.top = 0;
                                    }
                                    KeyCode::End | KeyCode::Char('G') if !app.is_input_active() => {
                                        if !app.display_rows.is_empty() {
                                            app.selected = app.display_rows.len() - 1;
                                            if let Ok(size) = terminal.size() {
                                                let view_h = list_visible_height(size.height);
                                                adjust_list_window(&mut app, view_h);
                                            }
                                        }
                                    }
                                    KeyCode::Enter => {
                                        // In grouped mode, Enter on a group toggles expand
                                        if app.grouped_mode && app.is_group_selected() {
                                            app.toggle_group_at_selected();
                                        } else {
                                            let height = terminal.size().ok().map(|sz| sz.height);
                                            open_selected_session(&mut app, height);
                                        }
                                    }
                                    KeyCode::Right if !app.is_input_active() => {
                                        // Expand group or do nothing
                                        if app.grouped_mode && app.is_group_selected() {
                                            app.expand_group_at_selected();
                                        }
                                    }
                                    KeyCode::Left if !app.is_input_active() => {
                                        // Collapse group (or parent group if on conversation)
                                        if app.grouped_mode {
                                            app.collapse_group_at_selected();
                                        }
                                    }
                                    KeyCode::Char('f') if !app.is_input_active() => {
                                        // Filter by path: works on both groups and conversations
                                        let path_opt = match app.display_rows.get(app.selected) {
                                            Some(DisplayRow::Group { path, .. }) => {
                                                Some(path.clone())
                                            }
                                            Some(DisplayRow::Conversation(row)) => {
                                                if !row.path.is_empty() {
                                                    Some(row.path.clone())
                                                } else {
                                                    None
                                                }
                                            }
                                            None => None,
                                        };
                                        if let Some(chosen) = path_opt {
                                            app.set_filter(Some(chosen.clone()));
                                            let _ = save_filter_pref(Some(&chosen));
                                        }
                                    }
                                    KeyCode::Char('x') if !app.is_input_active() => {
                                        app.set_filter(None);
                                        let _ = save_filter_pref(None);
                                        if !app.list_search_query.is_empty() {
                                            app.list_search_query.clear();
                                            app.rows = app.all_rows.clone();
                                            app.selected = 0;
                                            app.top = 0;
                                            app.rebuild_display_rows();
                                        }
                                    }
                                    KeyCode::Tab if !app.is_input_active() => {
                                        // Toggle grouped mode
                                        app.grouped_mode = !app.grouped_mode;
                                        let _ = save_grouped_mode_pref(app.grouped_mode);
                                        app.expanded_paths.clear();
                                        app.selected = 0;
                                        app.top = 0;
                                        app.rebuild_display_rows();
                                    }
                                    KeyCode::Char('e') if !app.is_input_active() => {
                                        // Export: skip if on a group header
                                        if !app.is_group_selected() {
                                            if let Some(DisplayRow::Conversation(row)) =
                                                app.display_rows.get(app.selected)
                                            {
                                                if let Some(msgs) = app.messages_cache.get(&row.id)
                                                {
                                                    let title = &row.id;
                                                    if let Ok(path) =
                                                        export_html(title, &row.path, msgs)
                                                    {
                                                        let _ = open::that(path);
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                        Mode::View => {
                            if app.show_help_modal {
                                match code {
                                    KeyCode::Esc | KeyCode::Char('?') => {
                                        app.show_help_modal = false;
                                    }
                                    _ => {}
                                }
                            } else {
                                match code {
                                    KeyCode::Esc if app.show_info_modal => {
                                        app.show_info_modal = false;
                                        // Re-enable mouse capture when closing info modal
                                        crossterm::execute!(
                                            terminal.backend_mut(),
                                            crossterm::event::EnableMouseCapture
                                        )?;
                                    }
                                    KeyCode::Char('?') if !app.is_input_active() => {
                                        // Re-enable mouse capture if info modal was open
                                        if app.show_info_modal {
                                            crossterm::execute!(
                                                terminal.backend_mut(),
                                                crossterm::event::EnableMouseCapture
                                            )?;
                                        }
                                        app.show_help_modal = true;
                                        app.show_info_modal = false;
                                        app.show_settings_modal = false;
                                    }
                                    KeyCode::Char('q') | KeyCode::Esc
                                        if !app.search_input && !app.show_info_modal =>
                                    {
                                        app.mode = Mode::List;
                                    }
                                    KeyCode::Esc if app.search_input => {
                                        app.search_input = false;
                                    }
                                    KeyCode::Up => {
                                        if let Ok(size) = terminal.size() {
                                            let view_h = view_visible_height(size.height);
                                            let current = usize::from(app.view_scroll);
                                            set_view_scroll(
                                                &mut app,
                                                current.saturating_sub(1),
                                                view_h,
                                            );
                                        } else {
                                            app.view_scroll = app.view_scroll.saturating_sub(1);
                                        }
                                    }
                                    KeyCode::Down => {
                                        if let Ok(size) = terminal.size() {
                                            let view_h = view_visible_height(size.height);
                                            let current = usize::from(app.view_scroll);
                                            set_view_scroll(
                                                &mut app,
                                                current.saturating_add(1),
                                                view_h,
                                            );
                                        } else {
                                            app.view_scroll = app.view_scroll.saturating_add(1);
                                        }
                                    }
                                    KeyCode::PageUp => {
                                        if let Ok(size) = terminal.size() {
                                            let view_h = view_visible_height(size.height);
                                            let current = usize::from(app.view_scroll);
                                            set_view_scroll(
                                                &mut app,
                                                current.saturating_sub(view_h),
                                                view_h,
                                            );
                                        }
                                    }
                                    KeyCode::PageDown => {
                                        if let Ok(size) = terminal.size() {
                                            let view_h = view_visible_height(size.height);
                                            let current = usize::from(app.view_scroll);
                                            set_view_scroll(
                                                &mut app,
                                                current.saturating_add(view_h),
                                                view_h,
                                            );
                                        }
                                    }
                                    KeyCode::Home | KeyCode::Char('g')
                                        if !app.is_input_active() =>
                                    {
                                        if let Ok(size) = terminal.size() {
                                            let view_h = view_visible_height(size.height);
                                            set_view_scroll(&mut app, 0, view_h);
                                        } else {
                                            app.view_scroll = 0;
                                        }
                                    }
                                    KeyCode::End | KeyCode::Char('G') if !app.is_input_active() => {
                                        if let Ok(size) = terminal.size() {
                                            let view_h = view_visible_height(size.height);
                                            set_view_scroll(&mut app, usize::MAX, view_h);
                                        }
                                    }
                                    KeyCode::Char('/') if !app.is_input_active() => {
                                        app.search_input = true;
                                        app.search_query.clear();
                                    }
                                    KeyCode::Char(c)
                                        if (c == 'T'
                                            || (c == 't'
                                                && modifiers.contains(KeyModifiers::SHIFT)))
                                            && !app.is_input_active() =>
                                    {
                                        app.view_expand_tools = !app.view_expand_tools;
                                        if app.view_show_tools {
                                            refresh_view_after_tool_toggle(&mut app, &mut terminal);
                                        }
                                    }
                                    KeyCode::Char('t')
                                        if !modifiers.contains(KeyModifiers::SHIFT)
                                            && !app.is_input_active() =>
                                    {
                                        app.view_show_tools = !app.view_show_tools;
                                        refresh_view_after_tool_toggle(&mut app, &mut terminal);
                                    }
                                    KeyCode::Enter if app.search_input => {
                                        // compute matches
                                        app.search_matches.clear();
                                        app.search_index = 0;
                                        if !app.search_query.is_empty() {
                                            let needle = app.search_query.to_lowercase();
                                            for (i, line) in app.view_text.iter().enumerate() {
                                                if line.to_lowercase().contains(&needle) {
                                                    app.search_matches.push(i);
                                                }
                                            }
                                            if let Some(&line_idx) = app.search_matches.first() {
                                                if let Ok(size) = terminal.size() {
                                                    let view_h = view_visible_height(size.height);
                                                    set_view_scroll(&mut app, line_idx, view_h);
                                                } else {
                                                    app.view_scroll =
                                                        line_idx.min(u16::MAX as usize) as u16;
                                                }
                                            }
                                        }
                                        app.search_input = false;
                                    }
                                    KeyCode::Char('n')
                                        if !app.is_input_active()
                                            && !app.search_matches.is_empty() =>
                                    {
                                        app.search_index =
                                            (app.search_index + 1) % app.search_matches.len();
                                        let line_idx = app.search_matches[app.search_index];
                                        if let Ok(size) = terminal.size() {
                                            let view_h = view_visible_height(size.height);
                                            set_view_scroll(&mut app, line_idx, view_h);
                                        } else {
                                            app.view_scroll =
                                                line_idx.min(u16::MAX as usize) as u16;
                                        }
                                    }
                                    KeyCode::Char('N')
                                        if !app.is_input_active()
                                            && !app.search_matches.is_empty() =>
                                    {
                                        if app.search_index == 0 {
                                            app.search_index = app.search_matches.len() - 1;
                                        } else {
                                            app.search_index -= 1;
                                        }
                                        let line_idx = app.search_matches[app.search_index];
                                        if let Ok(size) = terminal.size() {
                                            let view_h = view_visible_height(size.height);
                                            set_view_scroll(&mut app, line_idx, view_h);
                                        } else {
                                            app.view_scroll =
                                                line_idx.min(u16::MAX as usize) as u16;
                                        }
                                    }
                                    KeyCode::Char(c) if app.search_input => {
                                        app.search_query.push(c);
                                    }
                                    KeyCode::Backspace if app.search_input => {
                                        app.search_query.pop();
                                    }
                                    // Repath input handling in View mode
                                    KeyCode::Esc if app.repath_input => {
                                        app.repath_input = false;
                                    }
                                    KeyCode::Enter if app.repath_input => {
                                        if let Err(e) = repath_selected_session(&mut app) {
                                            eprintln!("Repath failed: {}", e);
                                        }
                                        app.repath_input = false;
                                    }
                                    KeyCode::Backspace if app.repath_input => {
                                        app.repath_query.pop();
                                    }
                                    KeyCode::Char(c) if app.repath_input => {
                                        app.repath_query.push(c);
                                    }
                                    KeyCode::Char('e') if !app.is_input_active() => {
                                        // In View mode, get the current conversation
                                        if let Some(DisplayRow::Conversation(row)) =
                                            app.display_rows.get(app.selected)
                                        {
                                            if let Some(msgs) = app.messages_cache.get(&row.id) {
                                                let title = &row.id;
                                                if let Ok(path) =
                                                    export_html(title, &row.path, msgs)
                                                {
                                                    let _ = open::that(path);
                                                }
                                            }
                                        }
                                    }
                                    KeyCode::Char('a') if !app.is_input_active() => {
                                        let result = if app.viewing_archive {
                                            unarchive_selected_session(&mut app)
                                        } else {
                                            archive_selected_session(&mut app)
                                        };
                                        match result {
                                            Ok(()) => {
                                                app.mode = Mode::List;
                                                app.view_lines.clear();
                                                app.view_text.clear();
                                                app.view_scroll = 0;
                                                app.view_show_tools = false;
                                                app.view_expand_tools = false;
                                                app.search_input = false;
                                                app.search_query.clear();
                                                app.search_matches.clear();
                                                app.search_index = 0;
                                            }
                                            Err(err) => {
                                                let action = if app.viewing_archive {
                                                    "restore"
                                                } else {
                                                    "archive"
                                                };
                                                eprintln!("Failed to {} session: {}", action, err);
                                            }
                                        }
                                    }
                                    KeyCode::Char('A') if !app.is_input_active() => {
                                        app.mode = Mode::List;
                                        app.view_lines.clear();
                                        app.view_text.clear();
                                        app.view_scroll = 0;
                                        app.view_show_tools = false;
                                        app.view_expand_tools = false;
                                        app.search_input = false;
                                        app.search_query.clear();
                                        app.search_matches.clear();
                                        app.search_index = 0;
                                        app.toggle_archive_view();
                                    }
                                    KeyCode::Char('u') if !app.is_input_active() => {
                                        if let Err(err) = undo_last_archive(&mut app) {
                                            eprintln!("Failed to undo archive: {}", err);
                                        }
                                    }
                                    KeyCode::Char('i') if !app.is_input_active() => {
                                        app.show_info_modal = true;
                                        app.show_help_modal = false;
                                        // Disable mouse capture so text can be selected
                                        crossterm::execute!(
                                            terminal.backend_mut(),
                                            crossterm::event::DisableMouseCapture
                                        )?;
                                    }
                                    KeyCode::Char('r') if !app.is_input_active() => {
                                        if let Some(DisplayRow::Conversation(row)) =
                                            app.display_rows.get(app.selected)
                                        {
                                            app.resume_session_request =
                                                Some((row.id.clone(), row.path.clone()));
                                        }
                                    }
                                    KeyCode::Char('p') if !app.is_input_active() => {
                                        if let Some(DisplayRow::Conversation(row)) =
                                            app.display_rows.get(app.selected)
                                        {
                                            app.repath_input = true;
                                            app.repath_query = row.path.clone();
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                }
                Event::Mouse(mouse_event) => {
                    if let Ok(size) = terminal.size() {
                        let handled = match app.mode {
                            Mode::List => handle_list_mouse_event(&mut app, &mouse_event, size),
                            Mode::View => handle_view_mouse_event(&mut app, &mouse_event, size),
                        };
                        if handled {
                            needs_redraw = true;
                        }
                    }
                }
                Event::Resize(_, _) => {
                    needs_redraw = true;
                }
                _ => {}
            }
        }
    }

    disable_raw_mode()?;
    crossterm::execute!(
        terminal.backend_mut(),
        crossterm::event::DisableMouseCapture,
        crossterm::terminal::LeaveAlternateScreen
    )?;
    terminal.show_cursor()?;
    Ok(())
}

fn spawn_first_msg_loader(
    projects_dir: PathBuf,
) -> (Sender<FirstMsgRequest>, Receiver<FirstMsgResult>) {
    let (req_tx, req_rx) = mpsc::channel::<FirstMsgRequest>();
    let (res_tx, res_rx) = mpsc::channel::<FirstMsgResult>();
    thread::spawn(move || {
        let reader = ChatReader::with_projects_dir(projects_dir);
        while let Ok(req) = req_rx.recv() {
            let result = match reader.get_messages_by_path(&req.file_path) {
                Ok((_, messages)) => {
                    let first_msg = first_nonempty_text(&messages);
                    FirstMsgResult {
                        id: req.id,
                        first_msg,
                        messages,
                    }
                }
                Err(_) => FirstMsgResult {
                    id: req.id,
                    first_msg: None,
                    messages: Vec::new(),
                },
            };
            if res_tx.send(result).is_err() {
                break;
            }
        }
    });
    (req_tx, res_rx)
}

fn ensure_first_msgs(app: &mut App, area: Rect) {
    // Visible body height = total - (title 2 + header 1 + footer 1)
    if app.rows.is_empty() {
        return;
    }
    let body_h = area.height.saturating_sub(4) as usize;
    let end = (app.top + body_h).min(app.rows.len());
    for i in app.top..end {
        if app.rows[i].first_msg.is_empty() {
            let id = app.rows[i].id.clone();
            if let Some(msgs) = app.messages_cache.get(&id) {
                if let Some(first) = first_nonempty_text(msgs) {
                    let first_str = first.clone();
                    if let Some(row) = app.all_rows.iter_mut().find(|r| r.id == id) {
                        row.first_msg = first.clone();
                    }
                    app.rows[i].first_msg = first_str.clone();
                    // Also update display_rows so the rendered view reflects the change
                    update_display_row_first_msg(app, &id, &first_str);
                    app.first_msg_pending.remove(&id);
                    continue;
                }
            }
            if app.first_msg_pending.contains(&id) {
                continue;
            }
            if let Some(tx) = app.first_msg_tx.as_ref() {
                let req = FirstMsgRequest {
                    id: id.clone(),
                    file_path: app.rows[i].file_path.clone(),
                };
                if tx.send(req).is_ok() {
                    app.first_msg_pending.insert(id);
                } else {
                    app.first_msg_tx = None;
                }
            }
        }
    }
}

/// Update a conversation's first_msg in display_rows, including any group headers
/// that show this conversation as their newest preview.
fn update_display_row_first_msg(app: &mut App, id: &str, first_msg: &str) {
    // Find the path of this conversation so we can update group headers if needed
    let path = app.rows.iter().find(|r| r.id == id).map(|r| r.path.clone());

    for dr in &mut app.display_rows {
        match dr {
            DisplayRow::Conversation(row) if row.id == id => {
                row.first_msg = first_msg.to_string();
            }
            DisplayRow::Group {
                path: group_path,
                newest_preview,
                ..
            } => {
                // Update group preview if this conversation belongs to this group
                // and is the newest one (has the newest_preview currently empty or
                // matches what we're updating)
                if let Some(ref p) = path {
                    if group_path == p {
                        // Check if this conversation is the newest in the group
                        // by finding the conversation with max date_ms in this path
                        if let Some(newest) = app
                            .rows
                            .iter()
                            .filter(|r| &r.path == p)
                            .max_by_key(|r| r.date_ms)
                        {
                            if newest.id == id {
                                *newest_preview = first_msg.to_string();
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

fn first_nonempty_text(msgs: &[Message]) -> Option<String> {
    // Prefer the latest non-empty ASSISTANT message so previews mirror
    // the final response when no summary metadata exists.
    if let Some(m) = msgs
        .iter()
        .rev()
        .find(|m| m.role == "assistant" && !m.content.trim().is_empty())
    {
        return Some(single_line_preview(m.content.trim(), 240));
    }

    // Otherwise, fall back to the first non-empty USER message to surface the
    // original request when we cannot show the reply.
    if let Some(m) = msgs
        .iter()
        .find(|m| m.role == "user" && !m.content.trim().is_empty())
    {
        return Some(single_line_preview(m.content.trim(), 240));
    }

    // Finally, return the first non-empty message regardless of role.
    msgs.iter()
        .map(|m| m.content.trim())
        .find(|t| !t.is_empty())
        .map(|t| single_line_preview(t, 240))
}

fn single_line_preview(s: &str, max_chars: usize) -> String {
    let mut out = String::with_capacity(s.len());
    let mut last_space = false;
    for ch in s.chars() {
        if ch == '\n' || ch == '\r' {
            if !last_space {
                out.push(' ');
                last_space = true;
            }
            continue;
        }
        if ch.is_whitespace() {
            if !last_space {
                out.push(' ');
                last_space = true;
            }
        } else {
            out.push(ch);
            last_space = false;
        }
        if out.chars().count() >= max_chars {
            break;
        }
    }
    out.trim().to_string()
}

fn cycle_sort_key(app: &mut App) {
    app.sort_key = match app.sort_key {
        SortKey::Created => SortKey::Path,
        SortKey::Path => SortKey::LastMsg,
        SortKey::LastMsg => SortKey::Created,
    };
}

fn order_label(app: &App) -> &'static str {
    if app.sort_desc {
        " desc"
    } else {
        " asc"
    }
}
fn sort_label(app: &App) -> &'static str {
    match app.sort_key {
        SortKey::Created => "created",
        SortKey::Path => "path",
        SortKey::LastMsg => "last",
    }
}

// (resort removed; use apply_sort/spawn_sort instead)

fn spawn_sort(app: &mut App) {
    // Cancel previous receiver by replacing it; old thread will drop on send failure
    let (tx, rx) = mpsc::channel();
    app.sort_rx = Some(rx);
    app.sorting_in_progress = true;

    let mut rows = app.rows.clone();
    let key = app.sort_key;
    let desc = app.sort_desc;
    thread::spawn(move || {
        if let SortKey::LastMsg = key {
            for r in rows.iter_mut() {
                if r.last_msg_ms.is_none() {
                    r.last_msg_ms = ChatReader::latest_message_time_ms(&r.file_path);
                }
            }
        }
        match key {
            SortKey::Created => {
                if desc {
                    rows.sort_by(|a, b| b.date_ms.cmp(&a.date_ms));
                } else {
                    rows.sort_by(|a, b| a.date_ms.cmp(&b.date_ms));
                }
            }
            SortKey::Path => {
                if desc {
                    rows.sort_by(|a, b| b.path.to_lowercase().cmp(&a.path.to_lowercase()));
                } else {
                    rows.sort_by(|a, b| a.path.to_lowercase().cmp(&b.path.to_lowercase()));
                }
            }
            SortKey::LastMsg => {
                if desc {
                    rows.sort_by(|a, b| {
                        b.last_msg_ms.unwrap_or(0).cmp(&a.last_msg_ms.unwrap_or(0))
                    });
                } else {
                    rows.sort_by(|a, b| {
                        a.last_msg_ms.unwrap_or(0).cmp(&b.last_msg_ms.unwrap_or(0))
                    });
                }
            }
        }
        let _ = tx.send(rows);
    });
}

fn spawn_list_search(app: &mut App) {
    let (tx, rx) = mpsc::channel();
    app.list_search_rx = Some(rx);
    app.list_search_in_progress = true;
    let query = app.list_search_query.to_lowercase();
    let all_rows = app.all_rows.clone();
    thread::spawn(move || {
        let reader = ChatReader::new();
        let mut results: Vec<RowItem> = Vec::new();
        for r in all_rows.into_iter() {
            if let Ok((_, msgs)) = reader.get_messages_by_id(&r.id) {
                if msgs
                    .iter()
                    .any(|m| m.content.to_lowercase().contains(&query))
                {
                    results.push(r);
                }
            }
        }
        let _ = tx.send(results);
    });
}

fn spawn_full_refresh(app: &mut App) {
    app.snapshot_list_position();
    let (tx, rx) = mpsc::channel();
    app.update_rx = Some(rx);
    app.updating_in_progress = true;
    let projects_dir = app.projects_dir.clone();
    let key = app.sort_key;
    let desc = app.sort_desc;

    thread::spawn(move || {
        let reader = ChatReader::with_projects_dir(projects_dir);
        let mut rows: Vec<RowItem> = Vec::new();
        if let Ok(convos) = reader.list_conversations() {
            rows = convos
                .into_iter()
                .map(|c| {
                    let summary_preview = c
                        .summary
                        .as_ref()
                        .map(|s| single_line_preview(s, 240))
                        .unwrap_or_default();
                    RowItem {
                        id: c.id,
                        date_ms: c.created_at.unwrap_or(0),
                        path: c.path.unwrap_or_default(),
                        first_msg: summary_preview,
                        file_path: c.file.display().to_string(),
                        last_msg_ms: None,
                    }
                })
                .collect();
        }

        match key {
            SortKey::Created => {
                if desc {
                    rows.sort_by(|a, b| b.date_ms.cmp(&a.date_ms));
                } else {
                    rows.sort_by(|a, b| a.date_ms.cmp(&b.date_ms));
                }
            }
            SortKey::Path => {
                if desc {
                    rows.sort_by(|a, b| b.path.to_lowercase().cmp(&a.path.to_lowercase()));
                } else {
                    rows.sort_by(|a, b| a.path.to_lowercase().cmp(&b.path.to_lowercase()));
                }
            }
            SortKey::LastMsg => {
                for r in rows.iter_mut() {
                    if r.last_msg_ms.is_none() {
                        r.last_msg_ms =
                            ChatReader::latest_message_time_ms(&r.file_path);
                    }
                }
                if desc {
                    rows.sort_by(|a, b| {
                        b.last_msg_ms.unwrap_or(0).cmp(&a.last_msg_ms.unwrap_or(0))
                    });
                } else {
                    rows.sort_by(|a, b| {
                        a.last_msg_ms.unwrap_or(0).cmp(&b.last_msg_ms.unwrap_or(0))
                    });
                }
            }
        }
        let _ = tx.send(rows);
    });
}

fn apply_sort(app: &mut App) {
    match app.sort_key {
        SortKey::Created => {
            if app.sort_desc {
                app.rows.sort_by(|a, b| b.date_ms.cmp(&a.date_ms));
            } else {
                app.rows.sort_by(|a, b| a.date_ms.cmp(&b.date_ms));
            }
        }
        SortKey::Path => {
            if app.sort_desc {
                app.rows
                    .sort_by(|a, b| b.path.to_lowercase().cmp(&a.path.to_lowercase()));
            } else {
                app.rows
                    .sort_by(|a, b| a.path.to_lowercase().cmp(&b.path.to_lowercase()));
            }
        }
        SortKey::LastMsg => {
            for r in app.rows.iter_mut() {
                if r.last_msg_ms.is_none() {
                    r.last_msg_ms = ChatReader::latest_message_time_ms(&r.file_path);
                }
            }
            if app.sort_desc {
                app.rows
                    .sort_by(|a, b| b.last_msg_ms.unwrap_or(0).cmp(&a.last_msg_ms.unwrap_or(0)));
            } else {
                app.rows
                    .sort_by(|a, b| a.last_msg_ms.unwrap_or(0).cmp(&b.last_msg_ms.unwrap_or(0)));
            }
        }
    }
}

fn spawn_fs_watcher(app: &mut App) {
    // Create a new ping channel and watcher thread
    let projects = app.projects_dir.clone();
    if projects.exists() {
        let (ping_tx, ping_rx) = mpsc::channel();
        app.fs_ping_rx = Some(ping_rx);
        thread::spawn(move || {
            let (txn, rxn) = mpsc::channel::<Result<notify::Event, notify::Error>>();
            let mut watcher =
                notify::recommended_watcher(move |res: Result<notify::Event, notify::Error>| {
                    let _ = txn.send(res);
                })
                .ok();
            if let Some(w) = watcher.as_mut() {
                let _ = w.watch(&projects, RecursiveMode::Recursive);
                while let Ok(evt) = rxn.recv() {
                    if let Ok(event) = evt {
                        let _ = ping_tx.send(event.paths);
                    }
                }
            }
        });
    }
}

// Spawn watcher at startup and return its ping receiver
fn spawn_initial_fs_watcher(projects: &Path) -> Option<Receiver<Vec<PathBuf>>> {
    if projects.exists() {
        let projects = projects.to_path_buf();
        let (ping_tx, ping_rx) = mpsc::channel();
        thread::spawn(move || {
            let (txn, rxn) = mpsc::channel::<Result<notify::Event, notify::Error>>();
            let mut watcher =
                notify::recommended_watcher(move |res: Result<notify::Event, notify::Error>| {
                    let _ = txn.send(res);
                })
                .ok();
            if let Some(w) = watcher.as_mut() {
                let _ = w.watch(&projects, RecursiveMode::Recursive);
                while let Ok(evt) = rxn.recv() {
                    if let Ok(event) = evt {
                        let _ = ping_tx.send(event.paths);
                    }
                }
            }
        });
        return Some(ping_rx);
    }
    None
}

// Config directory: ~/.config/cc-history
fn config_dir() -> Option<PathBuf> {
    dirs::home_dir().map(|h| h.join(".config").join("cc-history"))
}

fn load_watcher_pref() -> bool {
    if let Some(dir) = config_dir() {
        let file = dir.join("config.json");
        if let Ok(text) = fs::read_to_string(file) {
            if let Ok(v) = serde_json::from_str::<serde_json::Value>(&text) {
                if let Some(b) = v.get("watcher").and_then(|x| x.as_bool()) {
                    return b;
                }
            }
        }
    }
    true // default: enabled
}

fn save_watcher_pref(enabled: bool) -> Result<()> {
    let mut v = read_config_json();
    v["watcher"] = serde_json::json!(enabled);
    write_config_json(&v)?;
    Ok(())
}

fn load_sort_prefs() -> (SortKey, bool) {
    let v = read_config_json();
    let key = v
        .get("sort_key")
        .and_then(|x| x.as_str())
        .unwrap_or("created");
    let sk = match key {
        "path" => SortKey::Path,
        "last" => SortKey::LastMsg,
        _ => SortKey::Created,
    };
    let desc = v.get("sort_desc").and_then(|x| x.as_bool()).unwrap_or(true);
    (sk, desc)
}

fn save_sort_prefs(key: SortKey, desc: bool) -> Result<()> {
    let mut v = read_config_json();
    let key_str = match key {
        SortKey::Created => "created",
        SortKey::Path => "path",
        SortKey::LastMsg => "last",
    };
    v["sort_key"] = serde_json::json!(key_str);
    v["sort_desc"] = serde_json::json!(desc);
    write_config_json(&v)?;
    Ok(())
}

fn load_filter_pref() -> Option<String> {
    let v = read_config_json();
    v.get("filter_path")
        .and_then(|x| x.as_str())
        .map(|s| s.to_string())
}

fn save_filter_pref(path: Option<&str>) -> Result<()> {
    let mut v = read_config_json();
    match path {
        Some(p) => v["filter_path"] = serde_json::json!(p),
        None => {
            let _ = v.as_object_mut().map(|m| m.remove("filter_path"));
        }
    }
    write_config_json(&v)?;
    Ok(())
}

fn load_claude_command_pref() -> String {
    let v = read_config_json();
    v.get("claude_command")
        .and_then(|x| x.as_str())
        .unwrap_or("claude")
        .to_string()
}

fn save_claude_command_pref(command: &str) -> Result<()> {
    let mut v = read_config_json();
    v["claude_command"] = serde_json::json!(command);
    write_config_json(&v)?;
    Ok(())
}

fn load_quit_after_launch_pref() -> bool {
    let v = read_config_json();
    v.get("quit_after_launch")
        .and_then(|x| x.as_bool())
        .unwrap_or(true)
}

fn save_quit_after_launch_pref(quit_after_launch: bool) -> Result<()> {
    let mut v = read_config_json();
    v["quit_after_launch"] = serde_json::json!(quit_after_launch);
    write_config_json(&v)?;
    Ok(())
}

fn load_grouped_mode_pref() -> bool {
    let v = read_config_json();
    v.get("grouped_mode")
        .and_then(|x| x.as_bool())
        .unwrap_or(true) // default: grouped mode enabled
}

fn save_grouped_mode_pref(grouped: bool) -> Result<()> {
    let mut v = read_config_json();
    v["grouped_mode"] = serde_json::json!(grouped);
    write_config_json(&v)?;
    Ok(())
}

fn read_config_json() -> serde_json::Value {
    if let Some(dir) = config_dir() {
        let file = dir.join("config.json");
        if let Ok(text) = fs::read_to_string(file) {
            if let Ok(v) = serde_json::from_str::<serde_json::Value>(&text) {
                return v;
            }
        }
    }
    serde_json::json!({})
}

fn write_config_json(v: &serde_json::Value) -> Result<()> {
    if let Some(dir) = config_dir() {
        fs::create_dir_all(&dir)?;
        let file = dir.join("config.json");
        fs::write(file, serde_json::to_string_pretty(v)?)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{first_nonempty_text, format_path_cell, tool_preview_summary, Message};

    #[test]
    fn keeps_home_prefix_for_short_paths() {
        assert_eq!(format_path_cell("~/code", 10), "~/code");
    }

    #[test]
    fn truncates_home_path_with_ellipsis() {
        assert_eq!(format_path_cell("~/projects/deep/path", 12), "~/p/de…/path");
    }

    #[test]
    fn narrows_home_path_without_root_when_needed() {
        assert_eq!(format_path_cell("~/projects/deep/path", 6), "~/…/…h");
    }

    #[test]
    fn falls_back_to_suffix_when_space_tight() {
        assert_eq!(format_path_cell("~/projects/deep/path", 3), "…th");
    }

    #[test]
    fn truncates_absolute_paths() {
        assert_eq!(format_path_cell("/Users/name/project", 12), "/U/n/project");
    }

    #[test]
    fn truncates_windows_paths() {
        assert_eq!(
            format_path_cell("C:/Users/name/project", 12),
            "C:/…/project"
        );
    }

    #[test]
    fn preview_prefers_last_assistant_when_available() {
        let msgs = vec![
            Message {
                role: "user".into(),
                content: "Warmup".into(),
            },
            Message {
                role: "assistant".into(),
                content: "Final assistant summary".into(),
            },
        ];
        let preview = first_nonempty_text(&msgs).expect("preview");
        assert_eq!(preview, "Final assistant summary");
    }

    #[test]
    fn tool_preview_skips_header_and_uses_stdout_line() {
        let content = "◀ Tool Result\n\nStdout:\n\n```\nfirst line\nsecond line\n```\n";
        assert_eq!(tool_preview_summary(content), "Stdout: first line");
    }

    #[test]
    fn tool_preview_uses_first_meaningful_line() {
        let content = "◀ Tool Result\nOperation completed successfully\nMore details";
        assert_eq!(
            tool_preview_summary(content),
            "Operation completed successfully"
        );
    }
}
