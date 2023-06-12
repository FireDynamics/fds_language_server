//! Add code lens support for the server

use std::fmt::Display;

use crate::parser::{Script, Token};
use crate::{versions::Version, Backend};
use tower_lsp::jsonrpc::Error;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{CodeLens, CodeLensParams, Command, Position, Range};

/// Errors that can occur when trying to display the code lens
#[derive(Debug)]
enum CodeLensError {
    /// Unable to load version info
    NoVersion(String),
    /// Unable to load script
    NoScript(String),
}

impl Display for CodeLensError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodeLensError::NoVersion(document) => write!(
                f,
                "version for the current document could not be found '{document}'"
            ),
            CodeLensError::NoScript(document) => write!(
                f,
                "script for the current document could not be found '{document}'"
            ),
        }
    }
}

impl From<CodeLensError> for Error {
    fn from(value: CodeLensError) -> Self {
        Self {
            code: tower_lsp::jsonrpc::ErrorCode::ServerError(0),
            message: value.to_string(),
            data: None,
        }
    }
}

/// Add codelens to the current document
pub async fn code_lens(backend: &Backend, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
    let uri = params.text_document.uri.to_string();
    let Some(version) = backend.get_version(&uri) else {
        let err = CodeLensError::NoVersion(uri);
        backend.error(format!("{err}")).await;
        return Err(err.into());
    };
    let Some(script) = backend.script_map.get(&uri) else{
        let err = CodeLensError::NoScript(uri);
        backend.error(format!("{err}")).await;
        return Err(err.into());
    };
    let script = script.value();

    let mut code_lenses = vec![];

    version_lens(version, &mut code_lenses);
    cell_size_lens(script, &mut code_lenses);

    Ok(Some(code_lenses))
}

/// Display the fds version used for the current file.
fn version_lens(version: Version, code_lenses: &mut Vec<CodeLens>) {
    code_lenses.push(CodeLens {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 0,
            },
        },
        command: Some(Command {
            title: format!("LSP: FDS Version {}", version),
            command: String::default(),
            arguments: None,
        }),
        data: None,
    });
}

/// Calculate all infos necessary to display the cell size info
fn cell_size_lens(script: &Script, code_lenses: &mut Vec<CodeLens>){
    let mut total_cells = 0.0;

    script
        .iter()
        .rev()
        .filter_map(|(block, range)| match block {
            crate::parser::Block::Code(code) => {
                if block.is_class("MESH") {
                    Some((code, range))
                } else {
                    None
                }
            }
            _ => None,
        })
        .for_each(|(code, _)| {
            let mut cells_x = 0.0;
            let mut cells_y = 0.0;
            let mut cells_z = 0.0;
            let mut size_x = 0.0;
            let mut size_y = 0.0;
            let mut size_z = 0.0;

            let mut code = code.iter();
            code.next();
            let Some((_, range)) = code.next() else { return;};

            while let Some((Ok(token), _)) = code.next() {
                if let Token::Property(property) = token {
                    match property.as_str() {
                        "IJK" => {
                            if let (
                                Some((Ok(Token::Equal), _)),
                                Some((Ok(Token::Number(i)), _)),
                                Some((Ok(Token::Comma), _)),
                                Some((Ok(Token::Number(j)), _)),
                                Some((Ok(Token::Comma), _)),
                                Some((Ok(Token::Number(k)), _)),
                            ) = (
                                &code.next(),
                                &code.next(),
                                &code.next(),
                                &code.next(),
                                &code.next(),
                                &code.next(),
                            ) {
                                cells_x = *i;
                                cells_y = *j;
                                cells_z = *k;
                            }
                        }
                        "XB" => {
                            if let (
                                Some((Ok(Token::Equal), _)),
                                Some((Ok(Token::Number(x1)), _)),
                                Some((Ok(Token::Comma), _)),
                                Some((Ok(Token::Number(x2)), _)),
                                Some((Ok(Token::Comma), _)),
                                Some((Ok(Token::Number(y1)), _)),
                                Some((Ok(Token::Comma), _)),
                                Some((Ok(Token::Number(y2)), _)),
                                Some((Ok(Token::Comma), _)),
                                Some((Ok(Token::Number(z1)), _)),
                                Some((Ok(Token::Comma), _)),
                                Some((Ok(Token::Number(z2)), _)),
                            ) = (
                                &code.next(),
                                &code.next(),
                                &code.next(),
                                &code.next(),
                                &code.next(),
                                &code.next(),
                                &code.next(),
                                &code.next(),
                                &code.next(),
                                &code.next(),
                                &code.next(),
                                &code.next(),
                            ) {
                                size_x = x2 - x1;
                                size_y = y2 - y1;
                                size_z = z2 - z1;
                            }
                        }
                        _ => {}
                    }
                };
            }
        
            if size_x > 0.0 && size_y > 0.0 && size_z > 0.0{
                let cells = cells_x * cells_y * cells_z;
                let cell_size_x = size_x / cells_x;
                let cell_size_y = size_y / cells_y;
                let cell_size_z = size_z / cells_z;

                code_lenses.push(CodeLens {
                    range: Range {
                        start: Position {
                            line: range.start.line,
                            character: range.start.character + 30,
                        },
                        end: Position {
                            line: range.start.line,
                            character: range.start.character + 30,
                        },
                    },
                    command: Some(Command {
                        title: format!("Mesh Size: {size_x} x {size_y} x {size_z} m³ | Cell Size {cell_size_x} x {cell_size_y} x {cell_size_z} m³ | Cells: {cells}"),
                        command: String::default(),
                        arguments: None,
                    }),
                    data: None,
                });



                total_cells += cells;
            }
        });

        code_lenses.push(CodeLens {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 0,
                },
            },
            command: Some(Command {
                title: format!("Total Cells: {total_cells}",),
                command: String::default(),
                arguments: None,
            }),
            data: None,
        });

}
