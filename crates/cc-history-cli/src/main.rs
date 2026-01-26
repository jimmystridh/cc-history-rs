mod commands;
mod models;
mod output;

use anyhow::Result;
use cc_history_lib::ChatReader;
use clap::{Parser, Subcommand};

use commands::{archive, duplicate, list, rename, repath, search, show, unarchive};
use output::{Formattable, OutputFormat};

#[derive(Parser)]
#[command(name = "cch")]
#[command(about = "Claude Code History - CLI tool for managing conversation history")]
#[command(version)]
struct Cli {
    /// Output format
    #[arg(short, long, value_enum, default_value_t = OutputFormat::Markdown)]
    format: OutputFormat,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// List conversations
    List(list::ListArgs),

    /// Show a conversation's messages
    Show(show::ShowArgs),

    /// Search conversations by content or summary
    Search(search::SearchArgs),

    /// Rename a session (update summary)
    Rename(rename::RenameArgs),

    /// Archive a session
    Archive(archive::ArchiveArgs),

    /// Restore an archived session
    Unarchive(unarchive::UnarchiveArgs),

    /// Duplicate a session
    Duplicate(duplicate::DuplicateArgs),

    /// Change a session's project path
    Repath(repath::RepathArgs),
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let reader = ChatReader::new();

    let output = match &cli.command {
        Commands::List(args) => list::execute(&reader, args)?.format(cli.format)?,
        Commands::Show(args) => show::execute(&reader, args)?.format(cli.format)?,
        Commands::Search(args) => search::execute(&reader, args)?.format(cli.format)?,
        Commands::Rename(args) => rename::execute(&reader, args)?.format(cli.format)?,
        Commands::Archive(args) => archive::execute(&reader, args)?.format(cli.format)?,
        Commands::Unarchive(args) => unarchive::execute(&reader, args)?.format(cli.format)?,
        Commands::Duplicate(args) => duplicate::execute(&reader, args)?.format(cli.format)?,
        Commands::Repath(args) => repath::execute(&reader, args)?.format(cli.format)?,
    };

    print!("{}", output);
    Ok(())
}
