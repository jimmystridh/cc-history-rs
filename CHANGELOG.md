# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.4] - 2025-09-15

### Added
- Download section in README with links to GitHub releases
- macOS notarization notice and workaround instructions

### Fixed
- Conversation parsing for sessions that start with summary lines (newer Claude sessions)
- Timezone display now shows local time instead of UTC in all views
- Missing conversations now appear correctly in the listing
- Info modal timestamps now display in local timezone

### Changed
- Improved conversation metadata detection to handle new Claude session format
- Applied clippy suggestions for cleaner code

## [1.0.3] - 2025-09-15

### Added
- Configurable quit-after-launch behavior in settings modal
- Multi-field settings navigation with ↑/↓ arrow keys
- Space/Y/N toggle for quit-after-launch setting
- Proper terminal state restoration when returning to TUI
- Settings modal now immediately editable without extra steps

### Changed
- Settings modal redesigned with better UX and visual indicators
- Settings are now saved/cancelled with Enter/Esc respectively
- Improved terminal handling for external command launches

### Fixed
- Terminal state corruption when returning to TUI after Claude exits
- Input conflicts during settings editing

## [1.0.2] - 2025-09-15

### Added
- Informational output after launching Claude sessions
- Display of command executed and working directory

### Changed
- Simplified session resumption logic
- Removed complex directory persistence attempts

### Fixed
- Working directory context information for users

## [1.0.1] - 2025-09-15

### Added
- Session resumption with 'r' key to launch `claude --resume <session_id>`
- Settings modal with Claude command configuration
- Session info modal with 'i' key showing detailed session metadata
- Terminal state management for external command launches
- Support for command switches (e.g., `claude --verbose`)
- Configuration persistence to `~/.config/cc-history/config.json`

### Fixed
- Duplicate message parsing in conversation display
- Input conflicts - hotkeys no longer active during text input
- Terminal corruption when launching external Claude sessions

### Changed
- Binary name from `cc-history-rs` to `cc-history` for CI compatibility
- Improved error handling and user feedback

## [0.1.0] - 2025-09-14

### Added
- Initial release of cc-history-rs
- Fast, keyboard-driven terminal UI for exploring Claude Code session history
- Conversation list with sortable columns (Time, CWD, First Message)
- Lazy loading of message previews for fast startup
- Global search across all conversations
- Live file watching for auto-updates
- Chat view with markdown rendering and inline code styling
- Search within conversations with match navigation
- Export conversations to clean HTML files
- Tool result handling with special Edit tool diff rendering
- Preferences persistence for sort order, filters, and watcher state
- Cross-platform support for macOS and Linux terminals

### Technical Features
- Reads session logs from `~/.claude/projects/**/<sessionId>.jsonl`
- Async sorting and searching for responsive UI
- File system watching with automatic refresh
- Home directory abbreviation (`~`) in paths
- TrueColor terminal support