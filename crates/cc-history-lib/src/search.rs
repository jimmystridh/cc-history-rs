use memchr::{memchr, memrchr};
use regex::bytes::{Regex, RegexBuilder};
use std::cmp::Reverse;
use std::fs::{self, File};
use std::io::Read;
use std::path::Path;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::thread;

use crate::reader::ChatReader;

const READ_CHUNK: usize = 1 << 20;

/// Case-insensitive substring search over rendered session messages.
///
/// The raw JSONL bytes are scanned first; only lines containing a raw hit are
/// parsed and rendered to confirm the match is in visible message content.
#[derive(Clone)]
pub struct SessionSearcher {
    raw: Regex,
    needle: String,
}

impl SessionSearcher {
    pub fn new(query: &str) -> Option<Self> {
        let quoted = serde_json::to_string(query).ok()?;
        let json_escaped = &quoted[1..quoted.len() - 1];
        let raw = RegexBuilder::new(&regex::escape(json_escaped))
            .case_insensitive(true)
            .build()
            .ok()?;
        Some(Self {
            raw,
            needle: query.to_lowercase(),
        })
    }

    pub fn matches_file(&self, path: &Path) -> bool {
        let Ok(mut file) = File::open(path) else {
            return false;
        };
        let mut buf: Vec<u8> = Vec::new();
        let mut filled = 0;
        loop {
            if buf.len() < filled + READ_CHUNK {
                buf.resize(filled + READ_CHUNK, 0);
            }
            let read = match file.read(&mut buf[filled..]) {
                Ok(n) => n,
                Err(_) => return false,
            };
            if read == 0 {
                return self.matches_lines(path, &buf[..filled]);
            }
            let unscanned = filled;
            filled += read;
            if let Some(offset) = memrchr(b'\n', &buf[unscanned..filled]) {
                let complete = unscanned + offset + 1;
                if self.matches_lines(path, &buf[..complete]) {
                    return true;
                }
                buf.copy_within(complete..filled, 0);
                filled -= complete;
            }
        }
    }

    fn matches_lines(&self, path: &Path, block: &[u8]) -> bool {
        let mut pos = 0;
        while pos < block.len() {
            let Some(hit) = self.raw.find_at(block, pos) else {
                return false;
            };
            let start = memrchr(b'\n', &block[..hit.start()]).map_or(0, |i| i + 1);
            let end = memchr(b'\n', &block[hit.end()..]).map_or(block.len(), |i| hit.end() + i);
            if self.rendered_line_matches(path, &block[start..end]) {
                return true;
            }
            pos = end + 1;
        }
        false
    }

    fn rendered_line_matches(&self, path: &Path, line: &[u8]) -> bool {
        ChatReader::parse_conversation_reader(path, line)
            .map(|(_, msgs)| {
                msgs.iter()
                    .any(|m| m.content.to_lowercase().contains(&self.needle))
            })
            .unwrap_or(false)
    }
}

/// Returns, for each path, whether the session contains `query` in its messages.
pub fn search_sessions<P: AsRef<Path> + Sync>(paths: &[P], query: &str) -> Vec<bool> {
    if query.is_empty() {
        return vec![true; paths.len()];
    }
    let Some(searcher) = SessionSearcher::new(query) else {
        return vec![false; paths.len()];
    };

    let mut largest_first: Vec<usize> = (0..paths.len()).collect();
    largest_first.sort_by_cached_key(|&i| {
        Reverse(
            fs::metadata(paths[i].as_ref())
                .map(|m| m.len())
                .unwrap_or(0),
        )
    });

    let next = AtomicUsize::new(0);
    let hits: Vec<AtomicBool> = paths.iter().map(|_| AtomicBool::new(false)).collect();
    let workers = thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(4)
        .min(paths.len().max(1));

    thread::scope(|scope| {
        for _ in 0..workers {
            let searcher = searcher.clone();
            let (next, hits, largest_first) = (&next, &hits, &largest_first);
            scope.spawn(move || {
                while let Some(&i) = largest_first.get(next.fetch_add(1, Ordering::Relaxed)) {
                    if searcher.matches_file(paths[i].as_ref()) {
                        hits[i].store(true, Ordering::Relaxed);
                    }
                }
            });
        }
    });

    hits.into_iter().map(AtomicBool::into_inner).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use std::path::PathBuf;
    use tempfile::TempDir;

    fn write_session(dir: &TempDir, name: &str, lines: &[&str]) -> PathBuf {
        let path = dir.path().join(format!("{name}.jsonl"));
        let mut file = File::create(&path).unwrap();
        for line in lines {
            writeln!(file, "{line}").unwrap();
        }
        path
    }

    fn user_line(text: &str) -> String {
        serde_json::json!({"type": "user", "message": {"role": "user", "content": text}})
            .to_string()
    }

    #[test]
    fn finds_matches_case_insensitively_in_input_order() {
        let dir = TempDir::new().unwrap();
        let a = write_session(&dir, "a", &[&user_line("Fix the Flaky parser")]);
        let b = write_session(&dir, "b", &[&user_line("unrelated")]);
        let c = write_session(&dir, "c", &[&user_line("x"), &user_line("FLAKY again")]);

        let hits = search_sessions(&[a, b, c], "flaky");

        assert_eq!(hits, vec![true, false, true]);
    }

    #[test]
    fn matches_text_that_is_escaped_in_json() {
        let dir = TempDir::new().unwrap();
        let path = write_session(
            &dir,
            "a",
            &[&user_line(r#"say "hello" to C:\temp räksmörgås"#)],
        );

        assert_eq!(search_sessions(&[&path], r#"say "hello""#), vec![true]);
        assert_eq!(search_sessions(&[&path], r"c:\temp"), vec![true]);
        assert_eq!(search_sessions(&[&path], "RÄKSMÖRGÅS"), vec![true]);
    }

    #[test]
    fn ignores_hits_outside_rendered_messages() {
        let dir = TempDir::new().unwrap();
        let line = serde_json::json!({
            "type": "user",
            "uuid": "needle-in-metadata",
            "message": {"role": "user", "content": "plain text"}
        })
        .to_string();
        let path = write_session(&dir, "a", &[&line]);

        assert_eq!(search_sessions(&[&path], "needle-in-metadata"), vec![false]);
    }

    #[test]
    fn finds_match_in_tool_output_and_final_line_without_newline() {
        let dir = TempDir::new().unwrap();
        let tool = serde_json::json!({
            "type": "user",
            "message": {"role": "user", "content": [{"type": "tool_result", "content": "ok"}]},
            "toolUseResult": {"stdout": "compiled Successfully", "stderr": ""}
        })
        .to_string();
        let path = dir.path().join("a.jsonl");
        fs::write(&path, format!("{}\n{}", user_line("first"), tool)).unwrap();

        assert_eq!(
            search_sessions(&[&path], "compiled successfully"),
            vec![true]
        );
    }

    #[test]
    fn finds_match_after_lines_longer_than_read_chunk() {
        let dir = TempDir::new().unwrap();
        let huge = "x".repeat(READ_CHUNK * 2 + 17);
        let path = write_session(&dir, "a", &[&user_line(&huge), &user_line("tail marker")]);

        assert_eq!(search_sessions(&[&path], "tail marker"), vec![true]);
        assert_eq!(search_sessions(&[&path], "absent"), vec![false]);
    }

    #[test]
    fn empty_query_matches_everything_and_missing_files_never_match() {
        let missing = PathBuf::from("/nonexistent/session.jsonl");

        assert_eq!(search_sessions(&[&missing], ""), vec![true]);
        assert_eq!(search_sessions(&[&missing], "anything"), vec![false]);
    }
}
