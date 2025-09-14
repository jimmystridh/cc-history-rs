mod chat_reader;

use anyhow::Result;
use chat_reader::{sort_conversations_by_earliest, ChatReader, Message};
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode};
use ratatui::backend::CrosstermBackend;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Paragraph, Row, Table, Wrap, Cell};
use ratatui::Terminal;
use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::PathBuf;
use std::time::{Duration, Instant};
use pulldown_cmark::{Parser, Options, html};
use std::sync::mpsc::{self, Receiver, TryRecvError};
use std::thread;
use notify::{Watcher, RecursiveMode};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode { List, View }

#[derive(Clone)]
struct RowItem {
    id: String,
    date_ms: i64,
    path: String,
    first_msg: String,
    file_path: String,
    last_msg_ms: Option<i64>,
}

struct App {
    reader: ChatReader,
    projects_dir: PathBuf,
    all_rows: Vec<RowItem>,
    rows: Vec<RowItem>,
    messages_cache: HashMap<String, Vec<Message>>,
    mode: Mode,
    selected: usize,
    top: usize,
    filter_path: Option<String>,
    view_lines: Vec<Line<'static>>,
    view_scroll: u16,
    view_text: Vec<String>,
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
    // fs watching + refreshing
    updating_in_progress: bool,
    update_rx: Option<Receiver<Vec<RowItem>>>,
    fs_ping_rx: Option<Receiver<()>>,
    watcher_enabled: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SortKey { Created, Path, LastMsg }

impl App {
    fn new_with_projects(projects_dir: PathBuf) -> Result<Self> {
        let reader = ChatReader::with_projects_dir(projects_dir.clone());
        let mut convos = reader.list_conversations()?;
        let watch_pref = load_watcher_pref();
        let (sort_key_pref, sort_desc_pref) = load_sort_prefs();
        let filter_pref = load_filter_pref();
        if convos.is_empty() {
            let mut app = Self {
                reader,
                projects_dir: projects_dir.clone(),
                all_rows: vec![],
                rows: vec![],
                messages_cache: HashMap::new(),
                mode: Mode::List,
                selected: 0,
                top: 0,
                filter_path: None,
                view_lines: vec![],
                view_scroll: 0,
                view_text: vec![],
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
                updating_in_progress: false,
                update_rx: None,
                fs_ping_rx: if watch_pref { spawn_initial_fs_watcher(&projects_dir) } else { None },
                watcher_enabled: watch_pref,
            };
            if let Some(fp) = filter_pref { app.set_filter(Some(fp)); }
            return Ok(app);
        }
        // Avoid expensive full-file scans on startup; sort by metadata timestamp only
        convos.sort_by_key(|c| c.created_at.unwrap_or(i64::MAX));

        let mut rows = Vec::new();
        let cache = HashMap::new(); // lazy-load messages on demand to avoid long startup blank screen
        for c in convos {
            let date_ms = c.created_at.unwrap_or(0);
            let path_str = c.path.clone().unwrap_or_default();
            rows.push(RowItem { id: c.id, date_ms, path: path_str, first_msg: String::new(), file_path: c.file.display().to_string(), last_msg_ms: None });
        }
        // Default sort: newest on top (created desc)
        rows.sort_by(|a,b| b.date_ms.cmp(&a.date_ms));

        // Optionally set up FS watcher ping channel based on preference
        let ping_rx = if watch_pref { spawn_initial_fs_watcher(&projects_dir) } else { None };

        let mut app = Self {
            reader,
            projects_dir: projects_dir.clone(),
            all_rows: rows.clone(),
            rows,
            messages_cache: cache,
            mode: Mode::List,
            selected: 0,
            top: 0,
            filter_path: None,
            view_lines: vec![],
            view_scroll: 0,
            view_text: vec![],
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
            updating_in_progress: false,
            update_rx: None,
            fs_ping_rx: ping_rx,
            watcher_enabled: watch_pref,
        };
        if let Some(fp) = filter_pref.clone() { app.set_filter(Some(fp)); } else { app.set_filter(None); }
        apply_sort(&mut app);
        Ok(app)
    }

    fn set_filter(&mut self, path: Option<String>) {
        self.filter_path = path;
        if let Some(ref p) = self.filter_path {
            self.rows = self.all_rows.iter().cloned().filter(|r| &r.path == p).collect();
        } else {
            self.rows = self.all_rows.clone();
        }
        self.selected = 0;
        self.top = 0;
    }
}

fn format_time(ms: i64) -> String {
    if ms <= 0 { return String::new(); }
    let dt = chrono::DateTime::<chrono::Utc>::from_timestamp_millis(ms)
        .unwrap_or(chrono::DateTime::<chrono::Utc>::UNIX_EPOCH);
    dt.format("%Y-%m-%d %H:%M").to_string()
}

fn lines_for_message(msg: &Message) -> (Vec<Line<'static>>, Vec<String>) {
    let mut out: Vec<Line> = Vec::new();
    let mut texts: Vec<String> = Vec::new();
    let trimmed = msg.content.trim();
    if trimmed.is_empty() { return (out, texts); }
    let (prefix, title, header_text) = if msg.role == "user" {
        (Span::styled("You  » ", Style::default().fg(Color::Green)), Span::styled("You", Style::default().fg(Color::Green)), "You:")
    } else if msg.role.contains("tool") {
        (Span::styled("Tool « ", Style::default().fg(Color::Yellow)), Span::styled("Tool Result", Style::default().fg(Color::Yellow)), "Tool Result:")
    } else {
        (Span::styled("AI   « ", Style::default().fg(Color::Cyan)), Span::styled("Assistant", Style::default().fg(Color::Cyan)), "Assistant:")
    };
    out.push(Line::from(vec![prefix.clone(), title.clone(), Span::raw(":" )]));
    texts.push(header_text.to_string());

    let mut in_code = false;
    for raw_line in trimmed.lines() {
        if raw_line.trim_start().starts_with("```") { in_code = !in_code; continue; }
        let indent = "     ";
        if in_code {
            let s = Span::styled(raw_line.to_string(), Style::default().fg(Color::Gray));
            out.push(Line::from(vec![Span::raw(indent), s]));
            texts.push(format!("{}{}", indent, raw_line));
        } else {
            // render plain text for search
            texts.push(format!("{}{}", indent, raw_line));
            // minimal styled rendering (reuse previous logic without accumulating plain again)
            let mut line_spans: Vec<Span> = Vec::new();
            line_spans.push(Span::raw(indent));
            line_spans.push(Span::raw(raw_line.to_string()));
            out.push(Line::from(line_spans));
        }
    }
    (out, texts)
}

fn render_list(f: &mut ratatui::Frame, area: Rect, app: &App) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(2),
            Constraint::Length(1),
            Constraint::Min(1),
            Constraint::Length(1),
        ])
        .split(area);

    // Compute column widths first so header aligns with rows
    let width = chunks[2].width as usize;
    let date_w = 16usize;
    let path_w = ((width as f32 * 0.35) as usize).clamp(20, 50);
    let msg_w = width.saturating_sub(date_w + path_w + 7);

    let sorting_indicator = if app.sorting_in_progress { " sorting…" } else { "" };
    let search_indicator = if app.list_search_in_progress { " searching…" } else { "" };
    let watch_indicator = if app.watcher_enabled { " watch:on" } else { " watch:off" };
    let mut title_text = if let Some(fp) = &app.filter_path {
        format!(" Conversations — {} of {} [{}{}{}{}] [cwd: {}] ", app.rows.len(), app.all_rows.len(), sort_label(app), order_label(app), sorting_indicator, watch_indicator, chat_reader::short_path(fp))
    } else {
        format!(" Conversations — {} of {} [{}{}{}{}] ", app.rows.len(), app.all_rows.len(), sort_label(app), order_label(app), sorting_indicator, watch_indicator)
    };
    if !app.list_search_query.is_empty() || app.list_search_in_progress {
        if app.list_search_in_progress {
            title_text.push_str(&format!(" [search: '{}'{}] ", app.list_search_query, search_indicator));
        } else {
            title_text.push_str(&format!(" [search: '{}' — {} matches — x to clear] ", app.list_search_query, app.rows.len()));
        }
    }
    let title = Paragraph::new(Line::from(Span::styled(title_text, Style::default().fg(Color::White).bg(Color::Blue))));
    f.render_widget(title, chunks[0]);

    // Aligned header matching the computed column widths
    let header_text = format!(
        "{} │ {} │ {}",
        pad("Time", date_w),
        pad("CWD", path_w),
        pad("First Message", msg_w)
    );
    let header_line = Line::from(Span::styled(header_text, Style::default().fg(Color::White).bg(Color::Blue)));
    f.render_widget(Paragraph::new(header_line), chunks[1]);

    // widths computed above

    let rows: Vec<Row> = app.rows.iter().enumerate().skip(app.top).take((chunks[2].height.max(1) - 0) as usize).map(|(i, r)| {
        let date = format_time(r.date_ms);
        let cwd = chat_reader::short_path(&r.path);
        let msg = &r.first_msg;
        let mut style = if i % 2 == 0 { Style::default().fg(Color::Gray) } else { Style::default() };
        if i == app.selected { style = Style::default().fg(Color::Black).bg(Color::Cyan); }
        let cells: Vec<Cell> = vec![
            Cell::from(pad(&date, date_w)),
            Cell::from("│".to_string()),
            Cell::from(Span::styled(pad(&cwd, path_w), Style::default().fg(Color::Green))),
            Cell::from("│".to_string()),
            Cell::from(pad(msg, msg_w)),
        ];
        Row::new(cells).style(style)
    }).collect();

    let widths = [Constraint::Length(date_w as u16), Constraint::Length(1), Constraint::Length(path_w as u16), Constraint::Length(1), Constraint::Min(10)];
    let table = Table::new(rows, widths)
        .block(Block::default().borders(Borders::ALL).border_style(Style::default().fg(Color::Blue)));
    f.render_widget(table, chunks[2]);

    let footer_text = if app.list_search_input {
        format!("/{}  — Enter run  Esc cancel", app.list_search_query)
    } else {
        " ↑/↓ move   PgUp/PgDn page   Home/g top   End/G bottom   Enter open   f filter cwd   / search   s sort key   o order   e export   x clear   q quit ".to_string()
    };
    let footer = Paragraph::new(Line::from(Span::styled(footer_text, Style::default().fg(Color::White).bg(Color::Blue))));
    f.render_widget(footer, chunks[3]);
}

fn render_view(f: &mut ratatui::Frame, area: Rect, app: &App) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(1),
            Constraint::Min(1),
            Constraint::Length(1),
        ])
        .split(area);

    let header = Paragraph::new(Line::from(Span::styled(" Chat ", Style::default().fg(Color::White).bg(Color::Blue))));
    f.render_widget(header, chunks[0]);

    // Highlight current match line if any
    let highlighted_lines = if !app.search_query.is_empty() && !app.search_matches.is_empty() {
        let curr = app.search_matches[app.search_index];
        highlight_current_line(&app.view_lines, curr)
    } else {
        app.view_lines.clone()
    };

    let para = Paragraph::new(highlighted_lines)
        .block(Block::default().borders(Borders::ALL).border_style(Style::default().fg(Color::Blue)))
        .wrap(Wrap { trim: false })
        .scroll((app.view_scroll, 0));
    f.render_widget(para, chunks[1]);

    let footer_text = if app.search_input {
        format!("/{}", app.search_query)
    } else if !app.search_query.is_empty() {
        if app.search_matches.is_empty() { String::from(" 0/0 — / to search ") } else { format!(" {}/{} — n/N next/prev — / new search ", app.search_index + 1, app.search_matches.len()) }
    } else {
        String::from(" ↑/↓ scroll   PgUp/PgDn page   Home/g top   End/G bottom   / search   Esc/q back   e export ")
    };
    let footer = Paragraph::new(Line::from(Span::styled(footer_text, Style::default().fg(Color::White).bg(Color::Blue))));
    f.render_widget(footer, chunks[2]);
}

fn highlight_current_line(lines: &Vec<Line<'static>>, idx: usize) -> Vec<Line<'static>> {
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
                new_spans.push(Span { content: sp.content.clone(), style: st });
            }
            out.push(Line::from(new_spans));
        } else {
            out.push(ln);
        }
    }
    out
}

fn pad(s: &str, width: usize) -> String {
    // Truncate safely on char boundaries and add ellipsis when needed
    let char_count = s.chars().count();
    if char_count > width {
        if width == 0 { return String::new(); }
        if width == 1 { return "…".to_string(); }
        let truncated: String = s.chars().take(width - 1).collect();
        return format!("{}…", truncated);
    }
    // Pad with spaces to the right to reach target width (by char count)
    let mut out = String::with_capacity(width);
    out.push_str(s);
    let pad_spaces = width - char_count;
    for _ in 0..pad_spaces { out.push(' '); }
    out
}

fn render_message_lines(msgs: &[Message]) -> (Vec<Line<'static>>, Vec<String>) {
    let mut v = Vec::new();
    let mut t = Vec::new();
    for m in msgs {
        let (mut lines, mut txts) = lines_for_message(m);
        v.append(&mut lines);
        t.append(&mut txts);
    }
    (v, t)
}

fn export_html(title: &str, cwd: &str, msgs: &[Message]) -> Result<std::path::PathBuf> {
    fn esc(s: &str) -> String { s.replace('&', "&amp;").replace('<', "&lt;").replace('>', "&gt;").replace('"', "&quot;").replace('\'', "&#39;") }

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
            if let Some(rest) = content.strip_prefix("◀ Tool Result") { content = rest.trim_start().to_string(); }
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
                if m.role=="user"{"role-user"} else {"role-assistant"}, role_title, rendered
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

fn main() -> Result<()> {
    // Optional override for projects directory
    let args: Vec<String> = std::env::args().collect();
    let mut projects_override: Option<PathBuf> = None;
    let mut i = 0;
    while i < args.len() {
        if args[i] == "--projects-dir" && i + 1 < args.len() {
            projects_override = Some(PathBuf::from(args[i+1].clone()));
            i += 1;
        }
        i += 1;
    }
    let reader = if let Some(dir) = projects_override.clone() { ChatReader::with_projects_dir(dir) } else { ChatReader::new() };

    if args.iter().any(|a| a == "--oldest-path") {
        let mut list = reader.list_conversations()?;
        if list.is_empty() { println!("No conversations found."); return Ok(()); }
        sort_conversations_by_earliest(&mut list);
        let pick = &list[0];
        println!("{}", pick.file.display());
        return Ok(());
    }
    if args.iter().any(|a| a == "--oldest") {
        let mut list = reader.list_conversations()?;
        if list.is_empty() { println!("No conversations found."); return Ok(()); }
        sort_conversations_by_earliest(&mut list);
        let pick = &list[0];
        let (_, messages) = reader.get_messages_by_id(&pick.id)?;
        for m in messages { println!("{}:\n\n{}\n", m.role, m.content); }
        return Ok(());
    }

    if args.iter().any(|a| a == "--export-oldest") {
        let mut list = reader.list_conversations()?;
        if list.is_empty() { println!("No conversations found."); return Ok(()); }
        sort_conversations_by_earliest(&mut list);
        let pick = &list[0];
        let (meta, messages) = reader.get_messages_by_id(&pick.id)?;
        let path = export_html(&pick.id, &meta.path.unwrap_or_default(), &messages)?;
        println!("Exported: {}", path.display());
        return Ok(());
    }

    if let Some(id) = args.iter().position(|a| a == "--export-id").and_then(|idx| args.get(idx+1)).cloned() {
        let (meta, messages) = reader.get_messages_by_id(&id)?;
        let path = export_html(&id, &meta.path.unwrap_or_default(), &messages)?;
        println!("Exported: {}", path.display());
        return Ok(());
    }

    // Build app before entering alt screen to avoid blank wait
    let projects_dir = projects_override.unwrap_or_else(|| dirs::home_dir().unwrap_or_else(|| PathBuf::from("")).join(".claude").join("projects"));
    let mut app = App::new_with_projects(projects_dir)?;

    enable_raw_mode()?;
    let mut stdout = io::stdout();
    crossterm::execute!(stdout, crossterm::terminal::EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let tick_rate = Duration::from_millis(200);
    let mut last_tick = Instant::now();

    loop {
        // Apply async sort results if available
        if let Some(rx) = &app.sort_rx {
            match rx.try_recv() {
                Ok(new_rows) => { app.rows = new_rows; app.sort_rx = None; app.sorting_in_progress = false; }
                Err(TryRecvError::Empty) => {}
                Err(TryRecvError::Disconnected) => { app.sort_rx = None; app.sorting_in_progress = false; }
            }
        }
        // Apply async list search results if available
        if let Some(rx) = &app.list_search_rx {
            match rx.try_recv() {
                Ok(new_rows) => { app.rows = new_rows; app.list_search_rx = None; app.list_search_in_progress = false; app.selected = 0; app.top = 0; }
                Err(TryRecvError::Empty) => {}
                Err(TryRecvError::Disconnected) => { app.list_search_rx = None; app.list_search_in_progress = false; }
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
                        if let Some(fp) = &app.filter_path { app.rows = app.rows.iter().cloned().filter(|r| &r.path == fp).collect(); }
                        // resort current view
                        apply_sort(&mut app);
                    } else {
                        // run search again on fresh data
                        spawn_list_search(&mut app);
                    }
                    app.selected = 0; app.top = 0;
                }
                Err(TryRecvError::Empty) => {}
                Err(TryRecvError::Disconnected) => { app.update_rx = None; app.updating_in_progress = false; }
            }
        }
        // React to FS ping
        if let Some(prx) = &app.fs_ping_rx { if prx.try_recv().is_ok() && !app.updating_in_progress { spawn_full_refresh(&mut app); } }
        // Precompute size for lazy loading of first message previews
        if let Ok(sz) = terminal.size() { if let Mode::List = app.mode { ensure_first_msgs(&mut app, sz); } }
        terminal.draw(|f| {
            let size = f.size();
            match app.mode { Mode::List => render_list(f, size, &app), Mode::View => render_view(f, size, &app) }
        })?;

        let timeout = tick_rate.saturating_sub(last_tick.elapsed());
        if crossterm::event::poll(timeout)? {
            if let Event::Key(KeyEvent { code, modifiers, .. }) = event::read()? {
                match app.mode {
                    Mode::List => {
                        match code {
                            // Quit
                            KeyCode::Char('c') if modifiers.contains(KeyModifiers::CONTROL) => break,
                            KeyCode::Char('q') if !app.list_search_input => break,
                            // Cancel search input (do not quit)
                            KeyCode::Esc if app.list_search_input => { app.list_search_input = false; },
                            KeyCode::Char('s') => { cycle_sort_key(&mut app); let _ = save_sort_prefs(app.sort_key, app.sort_desc); spawn_sort(&mut app); },
                            KeyCode::Char('o') => { app.sort_desc = !app.sort_desc; let _ = save_sort_prefs(app.sort_key, app.sort_desc); spawn_sort(&mut app); },
                            KeyCode::Char('w') => {
                                // toggle watcher
                                if app.watcher_enabled {
                                    app.fs_ping_rx = None; app.watcher_enabled = false;
                                } else {
                                    spawn_fs_watcher(&mut app); app.watcher_enabled = app.fs_ping_rx.is_some();
                                }
                                let _ = save_watcher_pref(app.watcher_enabled);
                            },
                            KeyCode::Char('/') => { app.list_search_input = true; app.list_search_query.clear(); },
                            KeyCode::Enter if app.list_search_input => { spawn_list_search(&mut app); app.list_search_input = false; },
                            KeyCode::Backspace if app.list_search_input => { app.list_search_query.pop(); },
                            KeyCode::Char(c) if app.list_search_input => { app.list_search_query.push(c); },
                            KeyCode::Up => { if app.selected > 0 { app.selected -= 1; if app.selected < app.top { app.top = app.selected; } } },
                            KeyCode::Down => { if app.selected + 1 < app.rows.len() { app.selected += 1; let view_h = (terminal.size().unwrap().height.max(4) - 4) as usize; if app.selected >= app.top + view_h { app.top = app.selected - view_h + 1; } } },
                            KeyCode::PageUp => { let view_h = (terminal.size().unwrap().height.max(4) - 4) as usize; app.selected = app.selected.saturating_sub(view_h); app.top = app.top.saturating_sub(view_h); },
                            KeyCode::PageDown => { let view_h = (terminal.size().unwrap().height.max(4) - 4) as usize; app.selected = (app.selected + view_h).min(app.rows.len().saturating_sub(1)); app.top = (app.top + view_h).min(app.rows.len().saturating_sub(1)); },
                            KeyCode::Home | KeyCode::Char('g') => { app.selected = 0; app.top = 0; },
                            KeyCode::End | KeyCode::Char('G') => {
                                if !app.rows.is_empty() {
                                    app.selected = app.rows.len() - 1;
                                    let view_h = (terminal.size().unwrap().height.max(4) - 4) as usize;
                                    app.top = app.selected.saturating_sub(view_h.saturating_sub(1));
                                }
                            },
                            KeyCode::Enter => {
                                if let Some(row) = app.rows.get(app.selected) {
                                    let msgs = if let Some(m) = app.messages_cache.get(&row.id) { m.clone() } else {
                                        match app.reader.get_messages_by_id(&row.id) {
                                            Ok((_, m)) => { app.messages_cache.insert(row.id.clone(), m.clone()); m },
                                            Err(_) => Vec::new(),
                                        }
                                    };
                                    let (lines, text) = render_message_lines(&msgs);
                                    app.view_lines = lines;
                                    app.view_text = text;
                                    app.view_scroll = 0;
                                    app.search_query.clear();
                                    app.search_matches.clear();
                                    app.search_index = 0;
                                    app.mode = Mode::View;
                                }
                            },
                            KeyCode::Char('f') => {
                                if let Some(row) = app.rows.get(app.selected) {
                                    if !row.path.is_empty() {
                                        let chosen = row.path.clone();
                                        app.set_filter(Some(chosen.clone()));
                                        let _ = save_filter_pref(Some(&chosen));
                                    }
                                }
                            },
                            KeyCode::Char('x') => { app.set_filter(None); let _ = save_filter_pref(None); if !app.list_search_query.is_empty() { app.list_search_query.clear(); app.rows = app.all_rows.clone(); app.selected = 0; app.top = 0; } },
                            KeyCode::Char('e') => { if let Some(row) = app.rows.get(app.selected) { if let Some(msgs) = app.messages_cache.get(&row.id) { let title = &row.id; if let Ok(path) = export_html(title, &row.path, msgs) { let _ = open::that(path); } } } },
                            _ => {}
                        }
                    }
                    Mode::View => {
                        match code {
                            KeyCode::Char('q') | KeyCode::Esc if !app.search_input => { app.mode = Mode::List; },
                            KeyCode::Esc if app.search_input => { app.search_input = false; },
                            KeyCode::Up => { app.view_scroll = app.view_scroll.saturating_sub(1); },
                            KeyCode::Down => { app.view_scroll = app.view_scroll.saturating_add(1); },
                            KeyCode::PageUp => { let h = terminal.size().unwrap().height.saturating_sub(4); app.view_scroll = app.view_scroll.saturating_sub(h); },
                            KeyCode::PageDown => { let h = terminal.size().unwrap().height.saturating_sub(4); app.view_scroll = app.view_scroll.saturating_add(h); },
                            KeyCode::Home | KeyCode::Char('g') => { app.view_scroll = 0; },
                            KeyCode::End | KeyCode::Char('G') => {
                                let h = terminal.size().unwrap().height.saturating_sub(4);
                                let max = app.view_lines.len().saturating_sub(h as usize);
                                app.view_scroll = max as u16;
                            },
                            KeyCode::Char('/') => { app.search_input = true; app.search_query.clear(); },
                            KeyCode::Enter if app.search_input => {
                                // compute matches
                                app.search_matches.clear(); app.search_index = 0;
                                if !app.search_query.is_empty() {
                                    let needle = app.search_query.to_lowercase();
                                    for (i, line) in app.view_text.iter().enumerate() {
                                        if line.to_lowercase().contains(&needle) { app.search_matches.push(i); }
                                    }
                                    if let Some(&line_idx) = app.search_matches.first() { app.view_scroll = line_idx as u16; }
                                }
                                app.search_input = false;
                            },
                            KeyCode::Char('n') if !app.search_input && !app.search_matches.is_empty() => {
                                app.search_index = (app.search_index + 1) % app.search_matches.len();
                                let line_idx = app.search_matches[app.search_index];
                                app.view_scroll = line_idx as u16;
                            },
                            KeyCode::Char('N') if !app.search_input && !app.search_matches.is_empty() => {
                                if app.search_index == 0 { app.search_index = app.search_matches.len() - 1; } else { app.search_index -= 1; }
                                let line_idx = app.search_matches[app.search_index];
                                app.view_scroll = line_idx as u16;
                            },
                            KeyCode::Char(c) if app.search_input => { app.search_query.push(c); },
                            KeyCode::Backspace if app.search_input => { app.search_query.pop(); },
                            KeyCode::Char('e') => { if let Some(row) = app.rows.get(app.selected) { if let Some(msgs) = app.messages_cache.get(&row.id) { let title = &row.id; if let Ok(path) = export_html(title, &row.path, msgs) { let _ = open::that(path); } } } },
                            _ => {}
                        }
                    }
                }
            }
        }
        if last_tick.elapsed() >= tick_rate { last_tick = Instant::now(); }
    }

    disable_raw_mode()?;
    crossterm::execute!(terminal.backend_mut(), crossterm::event::DisableMouseCapture, crossterm::terminal::LeaveAlternateScreen)?;
    terminal.show_cursor()?;
    Ok(())
}

fn ensure_first_msgs(app: &mut App, area: Rect) {
    // Visible body height = total - (title 2 + header 1 + footer 1)
    if app.rows.is_empty() { return; }
    let body_h = area.height.saturating_sub(4) as usize;
    let end = (app.top + body_h).min(app.rows.len());
    for i in app.top..end {
        if app.rows[i].first_msg.is_empty() {
            let id = app.rows[i].id.clone();
            // Try cache first
            let msg_opt = app.messages_cache.get(&id).cloned().or_else(|| {
                if let Ok((_, m)) = app.reader.get_messages_by_id(&id) {
                    app.messages_cache.insert(id.clone(), m.clone());
                    Some(m)
                } else { None }
            });
            if let Some(msgs) = msg_opt {
                if let Some(first) = first_nonempty_text(&msgs) {
                    app.rows[i].first_msg = first;
                }
            }
        }
    }
}

fn first_nonempty_text(msgs: &[Message]) -> Option<String> {
    // Prefer the first non-empty USER message
    for m in msgs {
        if m.role == "user" {
            let t = m.content.trim();
            if !t.is_empty() { return Some(single_line_preview(t, 240)); }
        }
    }
    // Fallback to any first non-empty message
    for m in msgs {
        let t = m.content.trim();
        if !t.is_empty() { return Some(single_line_preview(t, 240)); }
    }
    None
}

fn single_line_preview(s: &str, max_chars: usize) -> String {
    let mut out = String::with_capacity(s.len());
    let mut last_space = false;
    for ch in s.chars() {
        if ch == '\n' || ch == '\r' { if !last_space { out.push(' '); last_space = true; } continue; }
        if ch.is_whitespace() { if !last_space { out.push(' '); last_space = true; } }
        else { out.push(ch); last_space = false; }
        if out.chars().count() >= max_chars { break; }
    }
    out.trim().to_string()
}

fn cycle_sort_key(app: &mut App) {
    app.sort_key = match app.sort_key { SortKey::Created => SortKey::Path, SortKey::Path => SortKey::LastMsg, SortKey::LastMsg => SortKey::Created };
}

fn order_label(app: &App) -> &'static str { if app.sort_desc { " desc" } else { " asc" } }
fn sort_label(app: &App) -> &'static str {
    match app.sort_key { SortKey::Created => "created", SortKey::Path => "path", SortKey::LastMsg => "last" }
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
            SortKey::Created => { if desc { rows.sort_by(|a,b| b.date_ms.cmp(&a.date_ms)); } else { rows.sort_by(|a,b| a.date_ms.cmp(&b.date_ms)); } }
            SortKey::Path => { if desc { rows.sort_by(|a,b| b.path.to_lowercase().cmp(&a.path.to_lowercase())); } else { rows.sort_by(|a,b| a.path.to_lowercase().cmp(&b.path.to_lowercase())); } }
            SortKey::LastMsg => { if desc { rows.sort_by(|a,b| b.last_msg_ms.unwrap_or(0).cmp(&a.last_msg_ms.unwrap_or(0))); } else { rows.sort_by(|a,b| a.last_msg_ms.unwrap_or(0).cmp(&b.last_msg_ms.unwrap_or(0))); } }
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
                if msgs.iter().any(|m| m.content.to_lowercase().contains(&query)) {
                    results.push(r);
                }
            }
        }
        let _ = tx.send(results);
    });
}

fn spawn_full_refresh(app: &mut App) {
    let (tx, rx) = mpsc::channel();
    app.update_rx = Some(rx);
    app.updating_in_progress = true;
    let reader = chat_reader::ChatReader::new();
    let mut rows: Vec<RowItem> = Vec::new();
    if let Ok(convos) = reader.list_conversations() {
        rows = convos.into_iter().map(|c| RowItem {
            id: c.id,
            date_ms: c.created_at.unwrap_or(0),
            path: c.path.unwrap_or_default(),
            first_msg: String::new(),
            file_path: c.file.display().to_string(),
            last_msg_ms: None,
        }).collect();
    }
    // Apply current sort key/order
    let key = app.sort_key; let desc = app.sort_desc;
    thread::spawn(move || {
        match key {
            SortKey::Created => { if desc { rows.sort_by(|a,b| b.date_ms.cmp(&a.date_ms)); } else { rows.sort_by(|a,b| a.date_ms.cmp(&b.date_ms)); } }
            SortKey::Path => { if desc { rows.sort_by(|a,b| b.path.to_lowercase().cmp(&a.path.to_lowercase())); } else { rows.sort_by(|a,b| a.path.to_lowercase().cmp(&b.path.to_lowercase())); } }
            SortKey::LastMsg => {
                for r in rows.iter_mut() { if r.last_msg_ms.is_none() { r.last_msg_ms = chat_reader::ChatReader::latest_message_time_ms(&r.file_path); } }
                if desc { rows.sort_by(|a,b| b.last_msg_ms.unwrap_or(0).cmp(&a.last_msg_ms.unwrap_or(0))); }
                else { rows.sort_by(|a,b| a.last_msg_ms.unwrap_or(0).cmp(&b.last_msg_ms.unwrap_or(0))); }
            }
        }
        let _ = tx.send(rows);
    });
}

fn apply_sort(app: &mut App) {
    match app.sort_key {
        SortKey::Created => { if app.sort_desc { app.rows.sort_by(|a,b| b.date_ms.cmp(&a.date_ms)); } else { app.rows.sort_by(|a,b| a.date_ms.cmp(&b.date_ms)); } }
        SortKey::Path => { if app.sort_desc { app.rows.sort_by(|a,b| b.path.to_lowercase().cmp(&a.path.to_lowercase())); } else { app.rows.sort_by(|a,b| a.path.to_lowercase().cmp(&b.path.to_lowercase())); } }
        SortKey::LastMsg => {
            for r in app.rows.iter_mut() { if r.last_msg_ms.is_none() { r.last_msg_ms = chat_reader::ChatReader::latest_message_time_ms(&r.file_path); } }
            if app.sort_desc { app.rows.sort_by(|a,b| b.last_msg_ms.unwrap_or(0).cmp(&a.last_msg_ms.unwrap_or(0))); }
            else { app.rows.sort_by(|a,b| a.last_msg_ms.unwrap_or(0).cmp(&b.last_msg_ms.unwrap_or(0))); }
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
            let mut watcher = notify::recommended_watcher(move |res: Result<notify::Event, notify::Error>| { let _ = txn.send(res); }).ok();
            if let Some(w) = watcher.as_mut() {
                let _ = w.watch(&projects, RecursiveMode::Recursive);
                while let Ok(evt) = rxn.recv() { if evt.is_ok() { let _ = ping_tx.send(()); } }
            }
        });
    }
}

// Spawn watcher at startup and return its ping receiver
fn spawn_initial_fs_watcher(projects: &PathBuf) -> Option<Receiver<()>> {
    if projects.exists() {
        let projects = projects.clone();
        let (ping_tx, ping_rx) = mpsc::channel();
        thread::spawn(move || {
            let (txn, rxn) = mpsc::channel::<Result<notify::Event, notify::Error>>();
            let mut watcher = notify::recommended_watcher(move |res: Result<notify::Event, notify::Error>| { let _ = txn.send(res); }).ok();
            if let Some(w) = watcher.as_mut() {
                let _ = w.watch(&projects, RecursiveMode::Recursive);
                while let Ok(evt) = rxn.recv() { if evt.is_ok() { let _ = ping_tx.send(()); } }
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
                if let Some(b) = v.get("watcher").and_then(|x| x.as_bool()) { return b; }
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
    let key = v.get("sort_key").and_then(|x| x.as_str()).unwrap_or("created");
    let sk = match key { "path" => SortKey::Path, "last" => SortKey::LastMsg, _ => SortKey::Created };
    let desc = v.get("sort_desc").and_then(|x| x.as_bool()).unwrap_or(true);
    (sk, desc)
}

fn save_sort_prefs(key: SortKey, desc: bool) -> Result<()> {
    let mut v = read_config_json();
    let key_str = match key { SortKey::Created => "created", SortKey::Path => "path", SortKey::LastMsg => "last" };
    v["sort_key"] = serde_json::json!(key_str);
    v["sort_desc"] = serde_json::json!(desc);
    write_config_json(&v)?;
    Ok(())
}

fn load_filter_pref() -> Option<String> {
    let v = read_config_json();
    v.get("filter_path").and_then(|x| x.as_str()).map(|s| s.to_string())
}

fn save_filter_pref(path: Option<&str>) -> Result<()> {
    let mut v = read_config_json();
    match path {
        Some(p) => v["filter_path"] = serde_json::json!(p),
        None => { let _ = v.as_object_mut().map(|m| m.remove("filter_path")); }
    }
    write_config_json(&v)?;
    Ok(())
}

fn read_config_json() -> serde_json::Value {
    if let Some(dir) = config_dir() {
        let file = dir.join("config.json");
        if let Ok(text) = fs::read_to_string(file) {
            if let Ok(v) = serde_json::from_str::<serde_json::Value>(&text) { return v; }
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
