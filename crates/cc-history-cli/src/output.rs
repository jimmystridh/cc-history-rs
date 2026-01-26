use anyhow::Result;
use serde::Serialize;

#[derive(Debug, Clone, Copy, Default, clap::ValueEnum)]
pub enum OutputFormat {
    #[default]
    Markdown,
    Json,
    Toon,
}

pub trait Formattable: Serialize {
    fn format_markdown(&self) -> Result<String>;

    fn format_json(&self) -> Result<String> {
        Ok(serde_json::to_string_pretty(self)?)
    }

    fn format_toon(&self) -> Result<String>
    where
        Self: Sized,
    {
        Ok(serde_toon::to_string(self)?)
    }

    fn format(&self, fmt: OutputFormat) -> Result<String>
    where
        Self: Sized,
    {
        match fmt {
            OutputFormat::Markdown => self.format_markdown(),
            OutputFormat::Json => self.format_json(),
            OutputFormat::Toon => self.format_toon(),
        }
    }
}
