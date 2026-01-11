# AGENTS.md

Guidelines for AI agents working on `cc-history-rs`, a TUI for exploring Claude Code session history.

## Project Overview

- **Purpose**: Terminal User Interface for browsing local Claude Code sessions from `~/.claude/projects/**/<sessionId>.jsonl`
- **Language**: Rust (Edition 2021)
- **Binary name**: `cc-history` (different from package name `cc-history-rs`)
- **Architecture**: Single-binary TUI with async background workers for sorting, searching, and file watching

## Build Commands

```bash
# Build debug
cargo build

# Build release
cargo build --release

# Run directly
cargo run

# Run release build
cargo run --release
```

## Test Commands

```bash
# Run all tests
cargo test

# Run a single test by name (substring match)
cargo test test_name

# Run a single test with exact name
cargo test -- --exact test_name

# Run tests in a specific module
cargo test chat_reader::tests

# Run tests with output visible
cargo test -- --nocapture

# Run ignored tests (integration tests that need real data)
cargo test -- --ignored

# Run ignored tests with output
cargo test -- --ignored --nocapture
```

## Linting and Formatting

```bash
# Format code (required before commit)
cargo fmt

# Check formatting without modifying
cargo fmt -- --check

# Run clippy (CI fails on warnings)
cargo clippy --all-targets --all-features -- -D warnings

# Run clippy with auto-fix
cargo clippy --fix --allow-dirty
```

## CI Pipeline

The CI runs on every push/PR to `main`:
1. Clippy with `-D warnings` (warnings = errors)
2. `cargo test --all --tests -- --nocapture`
3. Ignored tests on Linux only
4. Cross-platform builds (Linux amd64/arm64, Windows amd64/arm64, macOS amd64/arm64)

## Code Style Guidelines

### File Structure

1. Module declarations (`mod chat_reader;`) at top
2. External crate imports grouped together
3. Standard library imports
4. Local imports from modules
5. Constants and statics
6. Struct/enum definitions
7. Impl blocks
8. Helper functions
9. `main()` function
10. `#[cfg(test)] mod tests` at bottom

### Import Organization

```rust
mod chat_reader;  // Module declarations first

use anyhow::{anyhow, Context, Result};  // External crates
use serde::Deserialize;
use std::collections::{HashMap, HashSet};  // Std library
use std::path::PathBuf;

use chat_reader::{ChatReader, Message};  // Local modules last
```

### Naming Conventions

| Type | Convention | Example |
|------|------------|---------|
| Functions | `snake_case` | `format_time`, `spawn_fs_watcher` |
| Types/Structs/Enums | `PascalCase` | `RowItem`, `SortKey`, `Mode` |
| Constants | `SCREAMING_SNAKE_CASE` | `CODE_INDENT`, `MOUSE_SCROLL_LINES` |
| Statics | `SCREAMING_SNAKE_CASE` | `SYNTAX_SET`, `SYNTAX_THEME` |
| Enum variants | `PascalCase` | `Mode::List`, `SortKey::Created` |
| Local variables | `snake_case` | `first_msg`, `file_path` |

### Error Handling

Use `anyhow` for application errors:

```rust
use anyhow::{anyhow, Context, Result};

fn do_something() -> Result<String> {
    let file = fs::read_to_string(path)
        .with_context(|| format!("failed to read {}", path.display()))?;
    
    if file.is_empty() {
        return Err(anyhow!("file is empty"));
    }
    
    Ok(file)
}
```

- Return `Result<T>` from functions that can fail
- Use `.with_context()` to add context to errors
- Use `anyhow!()` macro for ad-hoc errors
- Use `?` operator for propagation

### Types and Optionals

- Use `Option<T>` for optional values, not sentinel values
- Use `PathBuf` for owned paths, `&Path` for borrowed
- Use `String` for owned strings, `&str` for borrowed
- Prefer `Vec<T>` over arrays for dynamic collections

### Concurrency Patterns

The codebase uses `std::sync::mpsc` channels for async operations:

```rust
use std::sync::mpsc::{self, Receiver, Sender};
use std::thread;

// Spawn background worker
let (tx, rx) = mpsc::channel();
thread::spawn(move || {
    let result = expensive_operation();
    let _ = tx.send(result);
});

// Non-blocking check in main loop
match rx.try_recv() {
    Ok(result) => handle_result(result),
    Err(TryRecvError::Empty) => {},  // Still processing
    Err(TryRecvError::Disconnected) => {},  // Worker done
}
```

### Struct Definitions

```rust
#[derive(Debug, Clone)]  // Common derives
struct RowItem {
    id: String,
    date_ms: i64,
    path: String,
    first_msg: String,
    file_path: String,
    last_msg_ms: Option<i64>,
}
```

### Test Style

Tests go in a `#[cfg(test)]` module at the end of each file:

```rust
#[cfg(test)]
mod tests {
    use super::*;  // Import from parent module

    #[test]
    fn descriptive_test_name() {
        // Arrange
        let input = "test input";
        
        // Act
        let result = function_under_test(input);
        
        // Assert
        assert_eq!(result, "expected");
    }
}
```

- Use `tempfile` crate for tests needing temporary files
- Test names should describe what is being tested
- Use `assert_eq!` with descriptive messages: `assert_eq!(len, 1, "expected one item")`

## Key Dependencies

| Crate | Purpose |
|-------|---------|
| `ratatui` | TUI framework |
| `crossterm` | Terminal handling |
| `serde`/`serde_json` | JSON serialization |
| `anyhow`/`thiserror` | Error handling |
| `chrono` | Date/time handling |
| `notify` | Filesystem watching |
| `syntect` | Syntax highlighting |
| `tempfile` | Test utilities (dev-dependency) |

## Project Layout

```
src/
  main.rs          # TUI app, keybindings, rendering (~4000 lines)
  chat_reader.rs   # JSONL parsing, conversation discovery (~800 lines)
.github/
  workflows/ci.yml # CI/CD pipeline
  release.yml      # Release notes config
```

## Commit Message Format

Use conventional commits:
```
<type>(<scope>): <subject>
```

Types: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`, `perf`

Examples:
- `feat(ui): add keyboard shortcut for export`
- `fix(parser): handle empty content arrays`
- `refactor(chat_reader): extract message normalization`
