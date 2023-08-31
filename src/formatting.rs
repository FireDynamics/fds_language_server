//! Add formatting support

use crate::{parser::Token, Backend};

use std::fmt::Display;
use tower_lsp::{
    jsonrpc::{Error, Result},
    lsp_types::{DocumentFormattingParams, TextEdit},
};

/// Errors that can occur when trying to display the code lens
#[derive(Debug)]
enum FormattingError {
    /// Unable to load document
    NoRope(String),
    /// Unable to load script
    NoScript(String),
}
impl Display for FormattingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FormattingError::NoRope(rope) => write!(f, "document could not be found '{rope}'"),
            FormattingError::NoScript(document) => write!(
                f,
                "script for the current document could not be found '{document}'"
            ),
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
        let err = FormattingError::NoRope(uri);
        backend.error(format!("{err}")).await;
        return Err(err.into());
    };
    let rope = rope.value();
    let Some(script) = backend.script_map.get(&uri) else{
        let err = FormattingError::NoScript(uri);
        backend.error(format!("{err}")).await;
        return Err(err.into());
    };
    let script = script.value();

    let mut text_edits = vec![];
    let mut iter = script.iter().map(|f| (f.span.lsp_span, &f.token));
    while let Some((start, token)) = iter.next() {
        if &Token::Start == token {
            let mut tokens = vec![];
            if let Some((_, Token::Class(name))) = iter.next() {
                tokens.push(format!("&{name} "))
            } else {
                continue;
            }
            for (end, token) in iter.by_ref() {
                match token {
                    Token::Start | Token::Class(_) | Token::Error | Token::Comment => break,
                    Token::Comma | Token::Equal => continue,
                    Token::Property(name) => tokens.push(format!("{name} = ")),
                    Token::Number(_) => {
                        //HACK To support numbers like 12E12 they have to be cut from the raw document
                        let start = rope.line_to_char(end.start.line as usize)
                            + end.start.character as usize;
                        let end =
                            rope.line_to_char(end.end.line as usize) + end.end.character as usize;
                        let n = rope.slice(start..end).to_string();
                        tokens.push(format!("{n}, "))
                    }
                    Token::Boolean(b) => match b {
                        true => tokens.push(".TRUE., ".to_string()),
                        false => tokens.push(".FALSE., ".to_string()),
                    },
                    Token::String(text) => tokens.push(format!("\"{text}\", ")),
                    Token::Variable(text) => tokens.push(format!("#{text}#, ")),
                    Token::End => {
                        tokens.push("/".to_string());
                        let new_text = if tokens.iter().map(|f| f.len()).sum::<usize>() > 100 {
                            let mut property_length = 0;
                            tokens
                                .into_iter()
                                .map(|f| {
                                    if f.starts_with('&') {
                                        f
                                    } else if f.chars().take(1).any(|f| f.is_ascii_alphabetic()) {
                                        let text = format!("\n\t{f}");
                                        property_length = text.len();
                                        text
                                    } else if f.starts_with('/') {
                                        "\n/".to_string()
                                    } else if property_length + f.len() > 100 {
                                        let text = format!("\n\t\t{f}");
                                        property_length = text.len();
                                        text
                                    } else {
                                        property_length += f.len();
                                        f
                                    }
                                })
                                .collect()
                        } else {
                            tokens.join("")
                        };

                        let text_edit = TextEdit {
                            range: tower_lsp::lsp_types::Range {
                                start: start.start,
                                end: end.end,
                            },
                            new_text,
                        };
                        text_edits.push(text_edit);
                        break;
                    }
                }
            }
        }
    }
    // Reverse so the edits are executed from end to start so there are no conflicts in the edited range.
    text_edits.reverse();

    Ok(Some(text_edits))
}
