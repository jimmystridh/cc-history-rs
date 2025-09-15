fn main() {
    let file = "/Users/js/.claude/projects/-Users-js-code-cc-history-cc-history-rs/e4444ea6-b92f-4741-881a-6e5e3d44c156.jsonl";
    let first_line = std::fs::File::open(file).ok()
        .and_then(|f| std::io::BufRead::lines(std::io::BufReader::new(f)).next().and_then(|l| l.ok()));
    if let Some(line) = first_line {
        println!("First line: {}", line);
        let v: serde_json::Value = serde_json::from_str(&line).unwrap_or(serde_json::Value::Null);
        println!("JSON: {:#?}", v);
        let cwd = v.get("cwd").and_then(|x| x.as_str());
        println!("CWD: {:?}", cwd);
    }
}
