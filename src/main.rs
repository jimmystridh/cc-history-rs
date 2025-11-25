mod chat_reader;

use anyhow::{anyhow, Result};
use chat_reader::{sort_conversations_by_earliest, ChatReader, Message};
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
use std::io;
use std::path::{Path, PathBuf};
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};
use std::thread;
use std::time::{Duration, Instant};
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
    // fs watching + refreshing
    updating_in_progress: bool,
    update_rx: Option<Receiver<Vec<RowItem>>>,
    fs_ping_rx: Option<Receiver<Vec<PathBuf>>>,
    watcher_enabled: bool,
    last_renamed: Option<(PathBuf, Instant)>,
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
                updating_in_progress: false,
                update_rx: None,
                fs_ping_rx: if watch_pref {
                    spawn_initial_fs_watcher(&projects_dir)
                } else {
                    None
                },
                watcher_enabled: watch_pref,
                last_renamed: None,
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
            };
            if let Some(fp) = filter_pref {
                app.set_filter(Some(fp));
            }
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
            updating_in_progress: false,
            update_rx: None,
            fs_ping_rx: ping_rx,
            watcher_enabled: watch_pref,
            last_renamed: None,
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
        };
        if let Some(fp) = filter_pref.clone() {
            app.set_filter(Some(fp));
        } else {
            app.set_filter(None);
        }
        apply_sort(&mut app);
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
    }

    fn is_input_active(&self) -> bool {
        self.list_search_input || self.search_input || self.show_settings_modal || self.rename_input
    }

    fn clear_pending_list_position(&mut self) {
        self.pending_selected_id = None;
        self.pending_selected_index = None;
        self.pending_selected_offset = None;
    }

    fn snapshot_list_position(&mut self) {
        if let Some(row) = self.rows.get(self.selected) {
            self.pending_selected_id = Some(row.id.clone());
            self.pending_selected_index = Some(self.selected);
            self.pending_selected_offset = Some(self.selected.saturating_sub(self.top));
        } else {
            self.clear_pending_list_position();
        }
    }

    fn restore_list_position(&mut self) {
        let saved_id = self.pending_selected_id.take();
        let saved_index = self.pending_selected_index.take();
        let offset = self.pending_selected_offset.take().unwrap_or(0);

        if self.rows.is_empty() {
            self.selected = 0;
            self.top = 0;
            return;
        }

        let mut target_index =
            saved_index.unwrap_or_else(|| self.selected.min(self.rows.len().saturating_sub(1)));

        if let Some(id) = saved_id {
            if let Some(pos) = self.rows.iter().position(|r| r.id == id) {
                target_index = pos;
            }
        }

        target_index = target_index.min(self.rows.len().saturating_sub(1));
        self.selected = target_index;

        let kept_offset = offset.min(self.selected);
        self.top = self.selected.saturating_sub(kept_offset);
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
            Span::styled("You  » ", Style::default().fg(Color::Green)),
            Span::styled("You", Style::default().fg(Color::Green)),
            "You:",
        )
    } else if is_tool {
        (
            Span::styled("Tool « ", Style::default().fg(Color::Rgb(180, 180, 180))),
            Span::styled(
                "Tool Result",
                Style::default().fg(Color::Rgb(200, 200, 200)),
            ),
            "Tool Result:",
        )
    } else {
        (
            Span::styled("AI   « ", Style::default().fg(Color::Cyan)),
            Span::styled("Assistant", Style::default().fg(Color::Cyan)),
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
        let summary = format!("[tool result] {}  (T to expand)", preview);
        out.push(Line::from(vec![
            Span::styled(
                CODE_INDENT.to_string(),
                Style::default().fg(Color::DarkGray),
            ),
            Span::styled(
                summary.clone(),
                Style::default().fg(Color::Rgb(200, 200, 200)),
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
                Style::default().fg(Color::DarkGray),
            ));
            let content_span = if is_tool {
                Span::styled(
                    raw_line.to_string(),
                    Style::default().fg(Color::Rgb(200, 200, 200)),
                )
            } else {
                Span::raw(raw_line.to_string())
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
                Style::default().fg(Color::DarkGray),
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
            Style::default().fg(Color::DarkGray),
        ));

        if monochrome {
            if line.is_empty() {
                spans.push(Span::raw(String::new()));
            } else {
                spans.push(Span::styled(
                    line.to_string(),
                    Style::default().fg(Color::Rgb(200, 200, 200)),
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
    candidates.push(trimmed.split('.').next_back().unwrap_or(trimmed).to_string());
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
        Color::Rgb(220, 220, 220)
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
        " sorting…"
    } else {
        ""
    };
    let search_indicator = if app.list_search_in_progress {
        " searching…"
    } else {
        ""
    };
    let watch_indicator = if app.watcher_enabled {
        " watch:on"
    } else {
        " watch:off"
    };
    let mut title_text = if let Some(fp) = &app.filter_path {
        format!(
            " Conversations — {} of {} [{}{}{}{}] [cwd: {}] ",
            app.rows.len(),
            app.all_rows.len(),
            sort_label(app),
            order_label(app),
            sorting_indicator,
            watch_indicator,
            chat_reader::short_path(fp)
        )
    } else {
        format!(
            " Conversations — {} of {} [{}{}{}{}] ",
            app.rows.len(),
            app.all_rows.len(),
            sort_label(app),
            order_label(app),
            sorting_indicator,
            watch_indicator
        )
    };
    if !app.list_search_query.is_empty() || app.list_search_in_progress {
        if app.list_search_in_progress {
            title_text.push_str(&format!(
                " [search: '{}'{}] ",
                app.list_search_query, search_indicator
            ));
        } else {
            title_text.push_str(&format!(
                " [search: '{}' — {} matches — x to clear] ",
                app.list_search_query,
                app.rows.len()
            ));
        }
    }
    let title = Paragraph::new(Line::from(Span::styled(
        title_text,
        Style::default().fg(Color::White).bg(Color::Blue),
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
        Style::default().fg(Color::White).bg(Color::Blue),
    ));
    f.render_widget(Paragraph::new(header_line), chunks[1]);

    // widths computed above

    let rows: Vec<Row> = app
        .rows
        .iter()
        .enumerate()
        .skip(app.top)
        .take(chunks[2].height.max(1) as usize)
        .map(|(i, r)| {
            let date = format_time(r.date_ms);
            let cwd_raw = chat_reader::short_path(&r.path);
            let cwd = format_path_cell(&cwd_raw, path_w);
            let msg = &r.first_msg;
            let mut style = if i % 2 == 0 {
                Style::default().fg(Color::Gray)
            } else {
                Style::default()
            };
            if i == app.selected {
                style = Style::default().fg(Color::Black).bg(Color::Cyan);
            }
            let cells: Vec<Cell> = vec![
                Cell::from(pad(&date, date_w)),
                Cell::from("│".to_string()),
                Cell::from(Span::styled(
                    pad(&cwd, path_w),
                    Style::default().fg(Color::Green),
                )),
                Cell::from("│".to_string()),
                Cell::from(pad(msg, msg_w)),
            ];
            Row::new(cells).style(style)
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
            .border_style(Style::default().fg(Color::Blue)),
    );
    f.render_widget(table, chunks[2]);

    let footer_text = if app.list_search_input {
        format!("/{}  — Enter run  Esc cancel", app.list_search_query)
    } else if app.rename_input {
        format!("Rename: {}_  — Enter save  Esc cancel", app.rename_query)
    } else {
        " ↑/↓ move   PgUp/PgDn page   Home/g top   End/G bottom   Enter open   a archive   u undo   f filter cwd   / search   s sort key   o order   e export   n rename   i info   , settings   r resume   x clear   q quit ".to_string()
    };
    let footer = Paragraph::new(Line::from(Span::styled(
        footer_text,
        Style::default().fg(Color::White).bg(Color::Blue),
    )));
    f.render_widget(footer, chunks[3]);
}

fn render_view(f: &mut ratatui::Frame, area: Rect, app: &App) {
    let chunks = view_layout(area);

    let header = Paragraph::new(Line::from(Span::styled(
        " Chat ",
        Style::default().fg(Color::White).bg(Color::Blue),
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
                .border_style(Style::default().fg(Color::Blue)),
        )
        .wrap(Wrap { trim: false })
        .scroll((app.view_scroll, 0));
    f.render_widget(para, chunks[1]);

    let footer_text = if app.search_input {
        format!("/{}", app.search_query)
    } else if !app.search_query.is_empty() {
        if app.search_matches.is_empty() {
            String::from(" 0/0 — / to search — t toggle tools — T expand/collapse — a archive ")
        } else {
            format!(
                " {}/{} — n/N next/prev — / new search — t toggle tools — T expand/collapse — a archive ",
                app.search_index + 1,
                app.search_matches.len()
            )
        }
    } else {
        String::from(
            " ↑/↓ scroll   PgUp/PgDn page   Home/g top   End/G bottom   / search   t toggle tools   T expand/collapse   Esc/q back   i info   r resume   e export   a archive   u undo ",
        )
    };
    let footer = Paragraph::new(Line::from(Span::styled(
        footer_text,
        Style::default().fg(Color::White).bg(Color::Blue),
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
    if let Some(row) = app.rows.get(app.selected) {
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
    let selected_row = app
        .rows
        .get(app.selected)
        .cloned()
        .ok_or_else(|| anyhow!("No session selected"))?;

    let original_path = PathBuf::from(&selected_row.file_path);
    if !original_path.exists() {
        return Err(anyhow!(
            "Session file not found at {}",
            original_path.display()
        ));
    }

    let relative_path = compute_relative_path(&original_path, &app.projects_dir)?;
    let archive_root = archive_root_dir(&app.projects_dir);
    let archived_path = archive_root.join(&relative_path);

    if archived_path.exists() {
        return Err(anyhow!(
            "Archive destination already exists: {}",
            archived_path.display()
        ));
    }

    if let Some(parent) = archived_path.parent() {
        fs::create_dir_all(parent)?;
    }

    fs::rename(&original_path, &archived_path)?;

    app.messages_cache.remove(&selected_row.id);
    app.first_msg_pending.remove(&selected_row.id);
    let _ = remove_row_by_id(&mut app.all_rows, &selected_row.id);
    let _ = remove_row_by_id(&mut app.rows, &selected_row.id);

    adjust_selection_after_change(app);

    app.last_archived = Some(ArchivedAction {
        row: selected_row,
        original_path,
        archived_path,
    });

    Ok(())
}

fn generate_uuid() -> Result<String> {
    let output = std::process::Command::new("uuidgen")
        .output()?;
    let s = String::from_utf8(output.stdout)?;
    Ok(s.trim().to_lowercase())
}

fn duplicate_selected_session(app: &mut App) -> Result<()> {
    let selected_row = app
        .rows
        .get(app.selected)
        .cloned()
        .ok_or_else(|| anyhow!("No session selected"))?;

    let original_path = PathBuf::from(&selected_row.file_path);
    if !original_path.exists() {
        return Err(anyhow!(
            "Session file not found at {}",
            original_path.display()
        ));
    }

    let new_id = generate_uuid()?;
    let extension = original_path
        .extension()
        .and_then(|s| s.to_str())
        .unwrap_or("jsonl");
    let parent = original_path.parent().unwrap_or_else(|| Path::new("."));
    let new_path = parent.join(format!("{}.{}", new_id, extension));

    if new_path.exists() {
        return Err(anyhow!("Generated UUID collision, try again"));
    }

    {
        let mut source = fs::File::open(&original_path)?;
        let mut dest = fs::File::create(&new_path)?;

        // Prepend a new summary so the list view shows it's a copy
        let new_summary = if selected_row.first_msg.is_empty() {
            format!("(Copy) {}", selected_row.id)
        } else {
            format!("(Copy) {}", selected_row.first_msg)
        };

        let summary_json = serde_json::json!({
            "type": "summary",
            "summary": new_summary
        });
        use std::io::Write;
        writeln!(dest, "{}", summary_json)?;

        // Copy original content
        std::io::copy(&mut source, &mut dest)?;

        // Append duplication message for the chat view
        let dup_msg = serde_json::json!({
            "type": "assistant",
            "message": {
                "role": "assistant",
                "content": format!("Session duplicated from {}", selected_row.id)
            }
        });
        writeln!(dest, "{}", dup_msg)?;
    }

    spawn_full_refresh(app);

    Ok(())
}

fn rename_selected_session(app: &mut App) -> Result<()> {
    let selected_row = app
        .rows
        .get(app.selected)
        .cloned()
        .ok_or_else(|| anyhow!("No session selected"))?;

    let original_path = PathBuf::from(&selected_row.file_path);
    if !original_path.exists() {
        return Err(anyhow!(
            "Session file not found at {}",
            original_path.display()
        ));
    }

    let parent = original_path.parent().unwrap_or_else(|| Path::new("."));
    let temp_uuid = generate_uuid()?;
    let temp_path = parent.join(format!(".rename-{}.tmp", temp_uuid));

    {
        let source = fs::File::open(&original_path)?;
        let reader = io::BufReader::new(source);
        let mut dest = fs::File::create(&temp_path)?;

        let summary_json = serde_json::json!({
            "type": "summary",
            "summary": app.rename_query
        });
        use std::io::Write;
        writeln!(dest, "{}", summary_json)?;

        use std::io::BufRead;
        for line in reader.lines() {
            let line = line?;
            // Filter out existing summary lines to avoid duplication
            if let Ok(v) = serde_json::from_str::<serde_json::Value>(&line) {
                if v.get("type").and_then(|x| x.as_str()) == Some("summary") {
                    continue;
                }
            }
            writeln!(dest, "{}", line)?;
        }
    }

    fs::rename(&temp_path, &original_path)?;

    app.last_renamed = Some((original_path.clone(), Instant::now()));

    let preview = single_line_preview(&app.rename_query, 240);

    // Update memory
    if let Some(row) = app.all_rows.iter_mut().find(|r| r.id == selected_row.id) {
        row.first_msg = preview.clone();
    }
    if let Some(row) = app.rows.iter_mut().find(|r| r.id == selected_row.id) {
        row.first_msg = preview;
    }

    // We don't need a full refresh because we updated in-place.
    // The filesystem watcher might trigger one eventually, but we don't need to force it now.
    // spawn_full_refresh(app);

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

fn compute_relative_path(source: &Path, projects_dir: &Path) -> Result<PathBuf> {
    if let Ok(rel) = source.strip_prefix(projects_dir) {
        return Ok(rel.to_path_buf());
    }

    let canonical_source = fs::canonicalize(source)?;
    let canonical_projects = fs::canonicalize(projects_dir)?;

    if let Ok(rel) = canonical_source.strip_prefix(&canonical_projects) {
        return Ok(rel.to_path_buf());
    }

    Err(anyhow!(
        "File {} is not inside projects directory {}",
        source.display(),
        projects_dir.display()
    ))
}

fn archive_root_dir(projects_dir: &Path) -> PathBuf {
    projects_dir.with_file_name("projects-archive")
}

fn remove_row_by_id(rows: &mut Vec<RowItem>, id: &str) -> Option<RowItem> {
    rows.iter().position(|r| r.id == id).map(|idx| rows.remove(idx))
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
                        chat_reader::ChatReader::latest_message_time_ms(&row.file_path);
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
    if app.rows.is_empty() {
        app.selected = 0;
        app.top = 0;
        return;
    }

    if app.selected >= app.rows.len() {
        app.selected = app.rows.len() - 1;
    }

    if app.top > app.selected {
        app.top = app.selected;
    }

    let max_top = app.rows.len().saturating_sub(1);
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
                st.bg = Some(Color::Yellow);
                // Force black text for readability regardless of original
                st.fg = Some(Color::Black);
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
                let new_segments = vec![
                    segments[0].clone(),
                    PathSegment::literal("…/"),
                ];
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
    if let Some(id) = app.rows.get(app.selected).map(|r| r.id.clone()) {
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
                .title(" Session Info ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::Cyan)),
        )
        .wrap(Wrap { trim: true })
        .style(Style::default().bg(Color::Black).fg(Color::White));

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
            .fg(Color::Green)
            .add_modifier(Modifier::BOLD)
    } else {
        Style::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::BOLD)
    };

    let view_section_style = if app.mode == Mode::View {
        Style::default()
            .fg(Color::Green)
            .add_modifier(Modifier::BOLD)
    } else {
        Style::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::BOLD)
    };

    let mut lines: Vec<Line> = Vec::new();
    let shortcut_line = |key: &str, desc: &str| Line::from(format!("{:<20}{}", key, desc));

    lines.push(Line::from(Span::styled(
        "General",
        Style::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::BOLD),
    )));
    lines.push(shortcut_line("?", "Show this list of shortcuts"));
    lines.push(shortcut_line("Esc", "Close modal or cancel input"));
    lines.push(shortcut_line("Ctrl+C", "Quit immediately"));
    lines.push(shortcut_line("i", "Show session info for the selection"));
    lines.push(shortcut_line(",", "Open settings modal"));

    lines.push(Line::from(""));
    lines.push(Line::from(Span::styled("List Mode", list_section_style)));
    lines.push(shortcut_line("q", "Quit application"));
    lines.push(shortcut_line("Enter", "Open session / submit list search"));
    lines.push(shortcut_line("Up/Down", "Move selection"));
    lines.push(shortcut_line("PageUp/PageDown", "Page through sessions"));
    lines.push(shortcut_line("Home or g", "Jump to first session"));
    lines.push(shortcut_line("End or G", "Jump to last session"));
    lines.push(shortcut_line("/", "Start typing to search sessions"));
    lines.push(shortcut_line("s", "Cycle sort key"));
    lines.push(shortcut_line("o", "Toggle sort order"));
    lines.push(shortcut_line("f", "Filter by selected path"));
    lines.push(shortcut_line("x", "Clear filters and search"));
    lines.push(shortcut_line("w", "Toggle filesystem watcher"));
    lines.push(shortcut_line("r", "Resume session in shell"));
    lines.push(shortcut_line("a", "Archive selected session"));
    lines.push(shortcut_line("d", "Duplicate selected session"));
    lines.push(shortcut_line("n", "Rename selected session"));
    lines.push(shortcut_line("u", "Undo last archive"));
    lines.push(shortcut_line("e", "Export session to HTML"));

    lines.push(Line::from(""));
    lines.push(Line::from(Span::styled("View Mode", view_section_style)));
    lines.push(shortcut_line("q or Esc", "Return to session list"));
    lines.push(shortcut_line("Up/Down", "Scroll conversation"));
    lines.push(shortcut_line("PageUp/PageDown", "Page conversation"));
    lines.push(shortcut_line("Home or g", "Jump to top of conversation"));
    lines.push(shortcut_line("End or G", "Jump to bottom of conversation"));
    lines.push(shortcut_line("/", "Search within conversation"));
    lines.push(shortcut_line("Enter", "Jump to first search match"));
    lines.push(shortcut_line("n / N", "Next / previous match"));
    lines.push(shortcut_line("t", "Show or hide tool calls"));
    lines.push(shortcut_line("T", "Expand or collapse tool calls"));
    lines.push(shortcut_line("r", "Resume session in shell"));
    lines.push(shortcut_line("a", "Archive session"));
    lines.push(shortcut_line("u", "Undo last archive"));
    lines.push(shortcut_line("e", "Export session to HTML"));

    lines.push(Line::from(""));
    lines.push(Line::from("Press ? again or Esc to close."));

    let modal = Paragraph::new(lines)
        .block(
            Block::default()
                .title(" Shortcuts ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::Magenta)),
        )
        .wrap(Wrap { trim: true })
        .style(Style::default().bg(Color::Black).fg(Color::White));

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
    let mut lines = vec![Line::from("Settings"), Line::from("")];

    // Claude Command field
    let claude_indicator = if app.settings_selected_field == 0 {
        " >"
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
            Style::default().fg(Color::Yellow)
        } else {
            Style::default().fg(Color::White)
        },
    )));

    lines.push(Line::from(""));

    // Quit after launch field
    let quit_indicator = if app.settings_selected_field == 1 {
        " >"
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
            Style::default().fg(Color::Yellow)
        } else {
            Style::default().fg(Color::White)
        },
    )));

    lines.push(Line::from(""));
    lines.push(Line::from("↑/↓ navigate  Enter save  Esc cancel"));

    let modal = Paragraph::new(lines)
        .block(
            Block::default()
                .title(" Settings ")
                .borders(Borders::ALL)
                .border_style(Style::default().fg(Color::Green)),
        )
        .wrap(Wrap { trim: true })
        .style(Style::default().bg(Color::Black).fg(Color::White));

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

    let mut projects_override: Option<PathBuf> = None;
    let mut i = 0;
    while i < args.len() {
        if args[i] == "--projects-dir" && i + 1 < args.len() {
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
                    app.all_rows = new_all;
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
                                row.first_msg = first;
                            }
                        }
                        needs_redraw = true;
                    }
                    Err(TryRecvError::Empty) => break,
                    Err(TryRecvError::Disconnected) => {
                        app.first_msg_rx = None;
                        needs_redraw = true;
                        break;
                    }
                }
            }
        }
        // React to FS ping
        if let Some(prx) = &app.fs_ping_rx {
            if let Ok(paths) = prx.try_recv() {
                if !app.updating_in_progress {
                    let mut ignore = false;
                    if let Some((renamed_path, time)) = &app.last_renamed {
                        if time.elapsed() < Duration::from_secs(2) {
                            if paths.iter().any(|p| p == renamed_path) {
                                ignore = true;
                            }
                        } else {
                            app.last_renamed = None;
                        }
                    }

                    if !ignore {
                        spawn_full_refresh(&mut app);
                        needs_redraw = true;
                    }
                }
            }
        }

        // Handle resume session request
        if let Some((session_id, session_path)) = app.resume_session_request.take() {
            // Restore terminal state before launching external command
            crossterm::terminal::disable_raw_mode()?;
            crossterm::execute!(
                terminal.backend_mut(),
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
                                        if let Some(row) = app.rows.get(app.selected) {
                                            app.resume_session_request =
                                                Some((row.id.clone(), row.path.clone()));
                                        }
                                    }
                                    KeyCode::Char('a') if !app.is_input_active() => {
                                        if let Err(err) = archive_selected_session(&mut app) {
                                            eprintln!("Failed to archive session: {}", err);
                                        }
                                    }
                                    KeyCode::Char('d') if !app.is_input_active() => {
                                        if let Err(err) = duplicate_selected_session(&mut app) {
                                            eprintln!("Failed to duplicate session: {}", err);
                                        }
                                    }
                                    KeyCode::Char('n') if !app.is_input_active() => {
                                        if let Some(row) = app.rows.get(app.selected) {
                                            app.rename_input = true;
                                            // Start with the current summary/first msg as the default value
                                            // We use the row's first_msg which is already a preview,
                                            // but it's better than nothing. Ideally we would read the full summary
                                            // from file, but that might be slow.
                                            app.rename_query = row.first_msg.clone();
                                        }
                                    }
                                    KeyCode::Char('u') if !app.is_input_active() => {
                                        if let Err(err) = undo_last_archive(&mut app) {
                                            eprintln!("Failed to undo archive: {}", err);
                                        }
                                    }
                                    KeyCode::Char('/') => {
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
                                        if app.selected + 1 < app.rows.len() {
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
                                            let last_index = app.rows.len().saturating_sub(1);
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
                                        if !app.rows.is_empty() {
                                            app.selected = app.rows.len() - 1;
                                            if let Ok(size) = terminal.size() {
                                                let view_h = list_visible_height(size.height);
                                                adjust_list_window(&mut app, view_h);
                                            }
                                        }
                                    }
                                    KeyCode::Enter => {
                                        let height = terminal.size().ok().map(|sz| sz.height);
                                        open_selected_session(&mut app, height);
                                    }
                                    KeyCode::Char('f') if !app.is_input_active() => {
                                        if let Some(row) = app.rows.get(app.selected) {
                                            if !row.path.is_empty() {
                                                let chosen = row.path.clone();
                                                app.set_filter(Some(chosen.clone()));
                                                let _ = save_filter_pref(Some(&chosen));
                                            }
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
                                        }
                                    }
                                    KeyCode::Char('e') if !app.is_input_active() => {
                                        if let Some(row) = app.rows.get(app.selected) {
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
                                    }
                                    KeyCode::Char('?') if !app.is_input_active() => {
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
                                    KeyCode::Char('/') => {
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
                                    KeyCode::Char('e') if !app.is_input_active() => {
                                        if let Some(row) = app.rows.get(app.selected) {
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
                                        match archive_selected_session(&mut app) {
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
                                                eprintln!("Failed to archive session: {}", err);
                                            }
                                        }
                                    }
                                    KeyCode::Char('u') if !app.is_input_active() => {
                                        if let Err(err) = undo_last_archive(&mut app) {
                                            eprintln!("Failed to undo archive: {}", err);
                                        }
                                    }
                                    KeyCode::Char('i') if !app.is_input_active() => {
                                        app.show_info_modal = true;
                                        app.show_help_modal = false;
                                    }
                                    KeyCode::Char('r') if !app.is_input_active() => {
                                        if let Some(row) = app.rows.get(app.selected) {
                                            app.resume_session_request =
                                                Some((row.id.clone(), row.path.clone()));
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
                    app.rows[i].first_msg = first_str;
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
                    r.last_msg_ms = chat_reader::ChatReader::latest_message_time_ms(&r.file_path);
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
        let reader = chat_reader::ChatReader::new();
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
        let reader = chat_reader::ChatReader::with_projects_dir(projects_dir);
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
                            chat_reader::ChatReader::latest_message_time_ms(&r.file_path);
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
                    r.last_msg_ms = chat_reader::ChatReader::latest_message_time_ms(&r.file_path);
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
