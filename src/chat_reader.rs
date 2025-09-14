use anyhow::{anyhow, Context, Result};
use std::collections::HashMap;
use serde::Deserialize;
use serde_json::Value;
use std::cmp::Ordering;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use walkdir::WalkDir;
use similar::TextDiff;

#[derive(Debug, Clone)]
pub struct ConversationMeta {
    pub id: String,
    pub path: Option<String>,
    pub created_at: Option<i64>, // ms epoch
    pub file: PathBuf,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Message {
    pub role: String,
    pub content: String,
}

fn parse_epoch(v: &Value) -> Option<i64> {
    match v {
        Value::Number(n) => n.as_i64().or_else(|| n.as_f64().map(|f| f as i64)).map(|x| if x < 2_000_000_000 { x * 1000 } else { x }),
        Value::String(s) => chrono::DateTime::parse_from_rfc3339(s)
            .ok()
            .map(|dt| dt.timestamp_millis())
            .or_else(|| chrono::NaiveDateTime::parse_from_str(s, "%Y-%m-%d %H:%M:%S").ok().map(|ndt| ndt.and_utc().timestamp_millis()))
            .or_else(|| chrono::DateTime::parse_from_rfc2822(s).ok().map(|dt| dt.timestamp_millis())),
        _ => None,
    }
}

fn extract_text(val: &Value) -> Option<String> {
    if val.is_null() { return None; }
    if let Some(s) = val.as_str() { return Some(s.to_string()); }

    // Object case: support either direct text/content or nested content array
    if let Some(obj) = val.as_object() {
        if let Some(s) = obj.get("text").and_then(|v| v.as_str()) { return Some(s.to_string()); }
        if let Some(s) = obj.get("content").and_then(|v| v.as_str()) { return Some(s.to_string()); }
        if let Some(arr) = obj.get("content").and_then(|v| v.as_array()) {
            let parts: Vec<String> = arr.iter().filter_map(|p| {
                if let Some(s) = p.as_str() { return Some(s.to_string()); }
                if let Some(o) = p.as_object() {
                    // Skip tool_use/tool_result blocks for plaintext extraction
                    if let Some(t) = o.get("type").and_then(|v| v.as_str()) {
                        if t == "tool_use" || t == "tool_result" { return None; }
                    }
                    if let Some(s) = o.get("text").and_then(|v| v.as_str()) { return Some(s.to_string()); }
                    if let Some(s) = o.get("content").and_then(|v| v.as_str()) { return Some(s.to_string()); }
                }
                None
            }).collect();
            if !parts.is_empty() { return Some(parts.join("\n")); }
        }
    }

    // Top-level array case
    if let Some(arr) = val.as_array() {
        let parts: Vec<String> = arr.iter().filter_map(|p| {
            if let Some(s) = p.as_str() { return Some(s.to_string()); }
            if let Some(o) = p.as_object() {
                if let Some(t) = o.get("type").and_then(|v| v.as_str()) {
                    if t == "tool_use" || t == "tool_result" { return None; }
                }
                if let Some(s) = o.get("text").and_then(|v| v.as_str()) { return Some(s.to_string()); }
                if let Some(s) = o.get("content").and_then(|v| v.as_str()) { return Some(s.to_string()); }
            }
            None
        }).collect();
        if !parts.is_empty() { return Some(parts.join("\n")); }
    }
    None
}

fn extract_role(val: &Value) -> Option<String> {
    if let Some(s) = val.get("role").and_then(|v| v.as_str()) { return Some(s.to_string()); }
    if let Some(s) = val.get("author").and_then(|v| v.as_str()) { return Some(s.to_string()); }
    if let Some(s) = val.get("sender").and_then(|v| v.as_str()) { return Some(s.to_string()); }
    if let Some(s) = val.get("from").and_then(|v| v.as_str()) { return Some(s.to_string()); }
    if let Some(s) = val.get("type").and_then(|v| v.as_str()) { return Some(s.to_string()); }
    None
}

fn normalize_role(role: &str) -> String {
    let r = role.to_lowercase();
    if r.contains("user") { "user".into() }
    else if r.contains("assistant") || r.contains("claude") || r.contains("ai") { "assistant".into() }
    else { r }
}


fn home_dir() -> Option<PathBuf> {
    dirs::home_dir()
}

pub fn short_path(p: &str) -> String {
    if let Some(home) = home_dir() {
        if let Some(home_str) = home.to_str() {
            let norm_home = home_str.replace('\\', "/");
            let norm_p = p.replace('\\', "/");
            if norm_p == norm_home { return "~".into(); }
            if let Some(rest) = norm_p.strip_prefix(&(norm_home.clone() + "/")) { return format!("~{}{}", "/", rest); }
        }
    }
    p.into()
}

pub struct ChatReader {
    projects_root: PathBuf,
}

impl ChatReader {
    pub fn new() -> Self {
        let home = dirs::home_dir().unwrap_or_else(|| PathBuf::from(""));
        let projects = home.join(".claude").join("projects");
        Self { projects_root: projects }
    }

    pub fn with_projects_dir(dir: PathBuf) -> Self { Self { projects_root: dir } }

    pub fn list_conversations(&self) -> Result<Vec<ConversationMeta>> {
        let projects = self.projects_root.clone();
        if !projects.exists() { return Ok(vec![]); }
        let mut list = Vec::new();
        for entry in WalkDir::new(&projects).max_depth(2).into_iter().filter_map(|e| e.ok()) {
            let path = entry.path();
            if path.is_file() && path.extension().and_then(|e| e.to_str()) == Some("jsonl") {
                let id = path.file_stem().and_then(|s| s.to_str()).unwrap_or("").to_string();
                if id.is_empty() { continue; }
                // read first line
                let first_line = File::open(path).ok().and_then(|f| BufReader::new(f).lines().next().and_then(|l| l.ok()));
                let (cwd, created_at) = if let Some(line) = first_line {
                    let v: Value = serde_json::from_str(&line).unwrap_or(Value::Null);
                    let cwd = v.get("cwd").and_then(|x| x.as_str()).map(|s| s.to_string())
                        .or_else(|| v.get("workspace").and_then(|x| x.as_str()).map(|s| s.to_string()))
                        .or_else(|| v.get("path").and_then(|x| x.as_str()).map(|s| s.to_string()));
                    let created = ["timestamp","createdAt","created_at","created"].iter().filter_map(|k| v.get(*k)).filter_map(parse_epoch).min();
                    (cwd, created)
                } else { (None, None) };
                list.push(ConversationMeta { id, path: cwd, created_at: created_at, file: path.to_path_buf() });
            }
        }
        Ok(list)
    }

    pub fn get_messages_by_id(&self, id: &str) -> Result<(ConversationMeta, Vec<Message>)> {
        // find file
        let projects = self.projects_root.clone();
        let mut found: Option<PathBuf> = None;
        for entry in WalkDir::new(&projects).max_depth(2).into_iter().filter_map(|e| e.ok()) {
            let path = entry.path();
            if path.is_file() && path.extension().and_then(|e| e.to_str()) == Some("jsonl") {
                if path.file_stem().and_then(|s| s.to_str()) == Some(id) { found = Some(path.to_path_buf()); break; }
            }
        }
        let file = found.ok_or_else(|| anyhow!("Conversation not found: {}", id))?;
        let f = File::open(&file).with_context(|| format!("open {}", file.display()))?;
        let mut msgs = Vec::new();
        let mut first_line_val: Option<Value> = None;

        // Helper to flush any accumulated streaming content (call when not actively streaming)
        fn flush_stream(msgs: &mut Vec<Message>, stream_role: &mut Option<String>, stream_buf: &mut String) {
            if !stream_buf.trim().is_empty() {
                let role = stream_role.clone().unwrap_or_else(|| "assistant".into());
                msgs.push(Message { role: normalize_role(&role), content: stream_buf.trim().to_string() });
                stream_buf.clear();
                *stream_role = None;
            }
        }

        // Streaming reconstruction state for assistant messages
        let mut stream_role: Option<String> = None;
        let mut stream_buf = String::new();
        let mut streaming = false;

        // Track tool_use id -> name for pretty rendering of results
        let mut tool_names: HashMap<String, String> = HashMap::new();

        for (i, line) in BufReader::new(f).lines().enumerate() {
            let line = line?;
            let v: Value = match serde_json::from_str(&line) { Ok(v) => v, Err(_) => continue };
            if i == 0 { first_line_val = Some(v.clone()); }

            // (helper declared above)

            // 1) Direct message entries
            if v.get("message").is_some() || v.get("role").is_some() || v.get("content").is_some() {
                // End any previous stream before pushing a discrete message
                if streaming { streaming = false; }
                flush_stream(&mut msgs, &mut stream_role, &mut stream_buf);

                let mval = v.get("message").cloned().unwrap_or(v.clone());
                let role = extract_role(&mval).unwrap_or_else(|| "assistant".into());

                // If message has structured content, render tool calls/results prettily
                if let Some(arr) = mval.get("content").and_then(|x| x.as_array()) {
                    for item in arr {
                        if let Some(obj) = item.as_object() {
                            match obj.get("type").and_then(|x| x.as_str()) {
                                Some("tool_use") => {
                                    let id = obj.get("id").and_then(|x| x.as_str()).unwrap_or("");
                                    let name = obj.get("name").and_then(|x| x.as_str()).unwrap_or("tool");
                                    if !id.is_empty() {
                                        tool_names.insert(id.to_string(), name.to_string());
                                    }
                                    let mut pretty = String::new();
                                    if let Some(inp) = obj.get("input").and_then(|x| x.as_object()) {
                                        if name.eq_ignore_ascii_case("edit") {
                                            let file_path = inp.get("file_path").and_then(|x| x.as_str()).unwrap_or("");
                                            let old_s = inp.get("old_string").and_then(|x| x.as_str()).unwrap_or("");
                                            let new_s = inp.get("new_string").and_then(|x| x.as_str()).unwrap_or("");
                                            pretty.push_str(&format!("▶ Edit: {}\n\n", file_path));
                                            if !old_s.is_empty() || !new_s.is_empty() {
                                                let diff = TextDiff::from_lines(old_s, new_s);
                                                pretty.push_str("```diff\n");
                                                pretty.push_str("--- old\n");
                                                pretty.push_str("+++ new\n");
                                                for change in diff.iter_all_changes() {
                                                    let sign = match change.tag() { similar::ChangeTag::Delete => '-', similar::ChangeTag::Insert => '+', similar::ChangeTag::Equal => ' ' };
                                                    pretty.push(sign);
                                                    pretty.push_str(change.value());
                                                }
                                                pretty.push_str("\n```\n");
                                            }
                                        } else {
                                            pretty.push_str(&format!("▶ Tool: {}\n\n", name));
                                            if let Some(cmd) = inp.get("command").and_then(|x| x.as_str()) {
                                                pretty.push_str("Command:\n");
                                                pretty.push_str("```bash\n");
                                                pretty.push_str(cmd);
                                                pretty.push_str("\n```\n");
                                            }
                                            if let Some(desc) = inp.get("description").and_then(|x| x.as_str()) {
                                                pretty.push_str(&format!("Description: {}\n", desc));
                                            }
                                            for (k, v) in inp {
                                                if k == "command" || k == "description" { continue; }
                                                pretty.push_str(&format!("{}: {}\n", k, v));
                                            }
                                        }
                                    } else {
                                        pretty.push_str(&format!("▶ Tool: {}\n", name));
                                    }
                                    if !pretty.trim().is_empty() {
                                        msgs.push(Message { role: "assistant".into(), content: pretty });
                                    }
                                }
                                Some("tool_result") => {
                                    // Collate tool result body; only push if actual content present
                                    let mut body = String::new();
                                    if let Some(txt) = obj.get("content").and_then(|x| x.as_str()) {
                                        if !txt.trim().is_empty() {
                                            body.push_str("````\n");
                                            body.push_str(txt);
                                            body.push_str("\n````\n");
                                        }
                                    }
                                    if !body.trim().is_empty() {
                                        let mut pretty = String::from("◀ Tool Result\n\n");
                                        pretty.push_str(&body);
                                        msgs.push(Message { role: "tool".into(), content: pretty });
                                    }
                                }
                                _ => {
                                    // Fallback to text extraction per item
                                    if let Some(txt) = obj.get("text").and_then(|x| x.as_str()) {
                                        if !txt.trim().is_empty() {
                                            msgs.push(Message { role: normalize_role(&role), content: txt.to_string() });
                                        }
                                    }
                                }
                            }
                        } else if let Some(s) = item.as_str() {
                            if !s.trim().is_empty() {
                                msgs.push(Message { role: normalize_role(&role), content: s.to_string() });
                            }
                        }
                    }
                    // If top-level includes a structured toolUseResult, include stdout/stderr
                    if let Some(tr) = v.get("toolUseResult").and_then(|x| x.as_object()) {
                        let mut body = String::new();
                        if let Some(out) = tr.get("stdout").and_then(|x| x.as_str()) {
                            if !out.trim().is_empty() {
                                body.push_str("Stdout:\n\n```");
                                body.push_str("\n");
                                body.push_str(out);
                                body.push_str("\n```\n");
                            }
                        }
                        if let Some(err) = tr.get("stderr").and_then(|x| x.as_str()) {
                            if !err.trim().is_empty() {
                                body.push_str("Stderr:\n\n```");
                                body.push_str("\n");
                                body.push_str(err);
                                body.push_str("\n```\n");
                            }
                        }
                        if !body.trim().is_empty() {
                            let mut pretty = String::from("◀ Tool Result\n\n");
                            pretty.push_str(&body);
                            msgs.push(Message { role: "tool".into(), content: pretty });
                        }
                    } else {
                        // Fallback plain text
                        let content = extract_text(&mval).unwrap_or_default();
                        if !content.trim().is_empty() {
                            msgs.push(Message { role: normalize_role(&role), content });
                        }
                    }
                }
                continue;
            }

            // 2) Event-stream style entries
            if let Some(t) = v.get("type").and_then(|x| x.as_str()) {
                match t {
                    // Start of an assistant message
                    "message_start" => {
                        streaming = true;
                        stream_buf.clear();
                        stream_role = v.get("message").and_then(|m| extract_role(m)).or(Some("assistant".into()));
                    }
                    // Text chunks
                    "content_block_delta" => {
                        if let Some(delta) = v.get("delta") {
                            if let Some(txt) = delta.get("text").and_then(|x| x.as_str()) {
                                stream_buf.push_str(txt);
                            }
                        }
                    }
                    // Some logs include initial text in content_block_start
                    "content_block_start" => {
                        if let Some(cb) = v.get("content_block") {
                            if let Some(txt) = cb.get("text").and_then(|x| x.as_str()) { stream_buf.push_str(txt); }
                        }
                    }
                    // End of an assistant message
                    "message_stop" => {
                        streaming = false;
                        flush_stream(&mut msgs, &mut stream_role, &mut stream_buf);
                    }
                    _ => { /* ignore other event types */ }
                }
                continue;
            }

            // 3) Fallback: ignore unknown rows
        }

        // Flush any dangling stream buffer
        if !streaming { flush_stream(&mut msgs, &mut stream_role, &mut stream_buf); }
        let (cwd, created_at) = if let Some(v) = first_line_val {(
            v.get("cwd").and_then(|x| x.as_str()).map(|s| s.to_string()),
            ["timestamp","createdAt","created_at","created"].iter().filter_map(|k| v.get(*k)).filter_map(parse_epoch).min()
        )} else {(None, None)};
        let meta = ConversationMeta { id: id.to_string(), path: cwd, created_at, file };
        Ok((meta, msgs))
    }

    pub fn earliest_message_time_ms<P: AsRef<Path>>(file: P) -> Option<i64> {
        let f = File::open(file).ok()?;
        let mut best: Option<i64> = None;
        for line in BufReader::new(f).lines().flatten() {
            let v: Value = serde_json::from_str(&line).ok()?;
            let m = v.get("message").cloned().unwrap_or(v);
            // Try multiple timestamp fields
            let t = ["createdAt","created_at","created","timestamp","ts","time","date"]
                .iter().filter_map(|k| m.get(*k)).filter_map(parse_epoch).min();
            if let Some(ti) = t {
                best = Some(match best { Some(b) if b < ti => b, Some(b) => ti.min(b), None => ti });
            }
        }
        best
    }

    pub fn latest_message_time_ms<P: AsRef<Path>>(file: P) -> Option<i64> {
        let f = File::open(file).ok()?;
        let mut best: Option<i64> = None;
        for line in BufReader::new(f).lines().flatten() {
            let v: Value = serde_json::from_str(&line).ok()?;
            let m = v.get("message").cloned().unwrap_or(v);
            let t = ["createdAt","created_at","created","timestamp","ts","time","date"]
                .iter().filter_map(|k| m.get(*k)).filter_map(parse_epoch).max();
            if let Some(ti) = t {
                best = Some(match best { Some(b) if b > ti => b, Some(_) | None => ti });
            }
        }
        best
    }
}

pub fn sort_conversations_by_earliest(convos: &mut [ConversationMeta]) {
    convos.sort_by(|a, b| {
        let ta = ChatReader::earliest_message_time_ms(&a.file);
        let tb = ChatReader::earliest_message_time_ms(&b.file);
        match (ta, tb) {
            (Some(x), Some(y)) => x.cmp(&y),
            (Some(_), None) => Ordering::Less,
            (None, Some(_)) => Ordering::Greater,
            (None, None) => Ordering::Equal,
        }
    });
}
