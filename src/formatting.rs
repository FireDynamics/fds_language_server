//! Add formatting support

use std::fmt::Display;

use crate::Backend;

use chumsky::error::Cheap;
use chumsky::prelude::*;
use chumsky::text::ident;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::{DocumentFormattingParams, TextEdit};

/// Errors that can occur when trying to display the code lens
#[derive(Debug)]
enum FormattingError {
    /// Unable to load version info
    NoVersion(String),
    /// Unable to load script
    NoScript(String),
    /// Error inside the User input
    ParseError,
}
impl Display for FormattingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FormattingError::NoVersion(document) => write!(
                f,
                "version for the current document could not be found '{document}'"
            ),
            FormattingError::NoScript(document) => write!(
                f,
                "script for the current document could not be found '{document}'"
            ),
            FormattingError::ParseError => write!(f, "some error inside user input"),
        }
    }
}
impl From<FormattingError> for Error {
    fn from(value: FormattingError) -> Self {
        Self {
            code: tower_lsp::jsonrpc::ErrorCode::ServerError(0),
            message: value.to_string(),
            data: None,
        }
    }
}

///Format the whole Backend
pub async fn formatting(
    backend: &Backend,
    params: DocumentFormattingParams,
) -> Result<Option<Vec<TextEdit>>> {
    let uri = params.text_document.uri.to_string();
    let Some(rope) = backend.document_map.get(&uri) else{
        todo!("Error")
    };
    let rope = rope.value();
    let Some(script) = backend.script_map.get(&uri) else{
        let err = FormattingError::NoScript(uri);
        backend.error(format!("{err}")).await;
        return Err(err.into());
    };
    let script = script.value();

    let text_edits = script
        .iter()
        .rev()
        .filter_map(|(block, range)| match block {
            crate::parser::Block::Comment => None,
            crate::parser::Block::Code(code) => Some((code, range)),
            crate::parser::Block::ParseError(_) => None,
        })
        .filter_map(|(code, range)| {
            let mut text = vec![];
            for (res, range) in code {
                let token = match res {
                    Ok(ok) => ok,
                    Err(_) => return None,
                };

                match token {
                    crate::parser::Token::Start => {}
                    crate::parser::Token::Class(class) => text.push(format!("&{class} ")),
                    crate::parser::Token::Property(property) => text.push(format!("{property} = ")),
                    crate::parser::Token::Number(n) => {
                        let start = rope.line_to_char(range.start.line as usize)
                            + range.start.character as usize;
                        let end = rope.line_to_char(range.end.line as usize)
                            + range.end.character as usize;
                        let n = rope.slice(start..end).to_string();
                        text.push(format!("{n}, "))
                    }
                    crate::parser::Token::Boolean(b) => {
                        if *b {
                            text.push(".TRUE., ".to_string())
                        } else {
                            text.push(".FALSE., ".to_string())
                        }
                    }
                    crate::parser::Token::String(s) => text.push(format!("\"{s}\", ")),
                    crate::parser::Token::Comma => {}
                    crate::parser::Token::Equal => {}
                    crate::parser::Token::End => text.push("/\n".to_string()),
                }
            }

            let len = text.iter().map(|f| f.len()).sum::<usize>();
            let new_text = if len > 80 {
                text.into_iter()
                    .enumerate()
                    .map(|(i, s)| {
                        if i > 2 && ident::<_, Cheap<char>>().padded().parse(&s as &str).is_ok() {
                            format!("\n      {}", s)
                        } else {
                            s
                        }
                    })
                    .collect::<String>()
            } else {
                text.into_iter().collect::<String>()
            };

            Some(TextEdit {
                range: *range,
                new_text,
            })
        })
        .collect::<Vec<_>>();

    Ok(Some(text_edits))
}
