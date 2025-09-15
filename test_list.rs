use std::io::{BufRead, BufReader};
use std::fs::File;

fn main() {
    let reader = cc_history_rs::chat_reader::ChatReader::with_projects_dir("/Users/js/.claude/projects".into());
    match reader.list_conversations() {
        Ok(convos) => {
            println!("Found {} conversations", convos.len());
            for convo in convos {
                if convo.id.starts_with("e4444ea6") {
                    println!("Found e4444ea6: path={:?}, created_at={:?}", convo.path, convo.created_at);
                }
            }
        }
        Err(e) => println!("Error: {}", e),
    }
}
