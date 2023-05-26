use std::{error::Error, fmt::Display, ops::Index};

use serde_json::Value;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionParams, CompletionResponse, MessageType, Position,
};

use crate::{
    parser::{Block, Token, TokenError},
    versions::{Version, VersionValueError},
    Backend,
};

use tower_lsp::jsonrpc::Result;

#[derive(Debug)]
pub enum CompletionResponseError {
    NoScript(String),
    NoVersion(String),
    NoBlockAtPosition(Position),
}

impl Display for CompletionResponseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompletionResponseError::NoVersion(document) => write!(
                f,
                "version for the current document could not be found '{document}'"
            ),
            CompletionResponseError::NoScript(document) => write!(
                f,
                "script for the current document could not be found '{document}'"
            ),
            CompletionResponseError::NoBlockAtPosition(position) => {
                write!(f, "no block at position '{position:?}'")
            }
        }
    }
}

impl From<CompletionResponseError> for tower_lsp::jsonrpc::Error {
    fn from(value: CompletionResponseError) -> Self {
        Self {
            code: tower_lsp::jsonrpc::ErrorCode::ServerError(0),
            message: value.to_string(),
            data: None,
        }
    }
}

pub async fn get_completion_response(
    backend: &Backend,
    params: CompletionParams,
) -> Result<Option<CompletionResponse>> {
    let uri = params.text_document_position.text_document.uri.to_string();
    let Some(script) = backend.script_map.get(&uri) else {
        let err = CompletionResponseError::NoScript(uri);
        backend.error(format!("{err}")).await;
        return Err(err.into());
    };
    let Some(version) = backend.versions_map.get(&uri) else {
        let err = CompletionResponseError::NoVersion(uri);
        backend.error(format!("{err}")).await;
        return Err(err.into());
    };

    let pos = params.text_document_position.position;
    let Some(block) = script.get_block(pos) else {
        let err = CompletionResponseError::NoBlockAtPosition(pos);
        backend.error(format!("{err}")).await;
        return Err(err.into());

    };
    let result = block._get_token(pos);

    match result {
        //Classes
        Some((Ok(Token::Start), _)) | Some((Ok(Token::Class(_)), _)) => {
            let items = get_completion_classes(backend, &version);
            return Ok(items);
        }
        //Properties
        Some((Ok(Token::Property(_)), _))
        | Some((Err(TokenError::Property), _))
        | Some((Err(TokenError::End), _))
        | None => {
            let items = get_completion_properties(backend, &block, &version);
            return Ok(items);
        }
        Some((Ok(Token::Equal), _))
        | Some((Ok(Token::Comma), _))
        | Some((Err(TokenError::PropertyValue), _)) => {
            let items = get_completion_equal(backend, &block, pos, &uri, &version);
            return Ok(items);
        }
        _ => {
            backend
                .client
                .log_message(MessageType::INFO, "No completion item found.")
                .await;
        }
    }

    Ok(None)
}

fn get_completion_classes(backend: &Backend, version: &Version) -> Option<CompletionResponse> {
    let items = backend
        .fds_classes_map
        .get(version)?
        .values()
        .map(|f| f.get_completion_item(*version))
        .collect::<Vec<_>>();
    Some(items.into())
}

fn get_completion_properties(
    backend: &Backend,
    block: &Block,
    version: &Version,
) -> Option<CompletionResponse> {
    let name = block.get_name()?;
    let class = backend.fds_classes_map.get(&version)?;
    let class = class.get(&name)?;
    let items = class
        .properties
        .values()
        .map(|v| v.get_completion_item(class.label.clone(), *version))
        .collect::<Vec<_>>();
    Some(items.into())
}

fn get_completion_equal(
    backend: &Backend,
    block: &Block,
    pos: Position,
    uri: &String,
    version: &Version,
) -> Option<CompletionResponse> {
    let class = block.get_name()?;
    let (property, _) = block.get_recent_property(pos)?;

    let fds_defaults = backend.fds_defaults_map.get(&version)?;

    let mut completion_items = fds_defaults
        .iter()
        .enumerate()
        .filter(|(_, fds_default)| fds_default.is_item(&class, &property))
        .flat_map(|(i, fds_default)| fds_default.get_completion_items(i, *version))
        .collect::<Vec<_>>();

    if property.ends_with("_ID") {
        if let Some(context_map) = backend.context_map.get(uri) {
            let class = property[..(property.len() - 3)].to_string();
            let mut items = context_map.get_completion_items(&class);
            completion_items.append(&mut items);
        }
    }

    Some(completion_items.into())
}

// pub async fn get_completion_results(
//     backend: &Backend,
//     script: &Script,
//     version: &Version,
//     pos: Position,
// ) -> Option<CompletionResponse> {
//     let block = script.get_block(pos)?;
//     let result = block._get_token(pos);
//     match result {
//         //Classes
//         Some((Ok(Token::Start), _)) | Some((Ok(Token::Class(_)), _)) => {
//             let items = backend
//                 .fds_classes_map
//                 .get(version)?
//                 .values()
//                 .map(|f| f.get_completion_item(*version))
//                 .collect::<Vec<_>>();
//             return Some(items.into());
//         }
//         //Properties
//         Some((Ok(Token::Property(_)), _))
//         | Some((Err(TokenError::Property), _))
//         | Some((Err(TokenError::End), _))
//         | None => {
//             let name = block.get_name()?;
//             let class = backend.fds_classes_map.get(version)?;
//             let class = class.get(&name)?;
//             let items = class
//                 .properties
//                 .values()
//                 .map(|v| v.get_completion_item(class.label.clone(), *version))
//                 .collect::<Vec<_>>();
//             return Some(items.into());
//         }
//         Some((Ok(Token::Equal), _))
//         | Some((Ok(Token::Comma), _))
//         | Some((Err(TokenError::PropertyValue), _)) => {
//             let class = block.get_name()?;
//             let (property, _) = block.get_recent_property(pos)?;

//             let fds_defaults = backend.fds_defaults_map.get(version)?;

//             let completion_items = fds_defaults
//                 .iter()
//                 .enumerate()
//                 .filter(|(_, fds_default)| fds_default.is_item(&class, &property))
//                 .flat_map(|(i, fds_default)| fds_default.get_completion_items(i, *version))
//                 .collect::<Vec<_>>();

//             if property.ends_with("_ID") {
//                 let key = property[..(property.len() - 3)].to_string();
//                 if let Some(context_map) = backend.context_map.get(&key) {}
//             }

//             return Some(completion_items.into());
//         }
//         _ => {
//             backend
//                 .client
//                 .log_message(MessageType::INFO, "No completion item found.")
//                 .await;
//         }
//     }

//     None
// }

#[derive(Debug)]
pub enum CompletionItemValueError {
    NotAnArray,
    NoSupportedId(u64),
    NoId,
    NoVersion(VersionValueError),
    ClassNameMissing,
    DefaultNameIndexMissing,
    DefaultValueIndexMissing,
    LineNumberMissing,
    CharacterNumberMissing,
    ToManyValues,
}
impl Display for CompletionItemValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompletionItemValueError::NotAnArray => write!(f, "value is not an array"),
            CompletionItemValueError::NoSupportedId(id) => {
                write!(f, "'{id}' is not an valid id")
            }
            CompletionItemValueError::NoId => write!(f, "not an id"),
            CompletionItemValueError::NoVersion(_) => {
                write!(f, "not an valide version")
            }
            CompletionItemValueError::ClassNameMissing => {
                write!(f, "class name is missing")
            }
            CompletionItemValueError::DefaultNameIndexMissing => {
                write!(f, "default name index is missing")
            }
            CompletionItemValueError::DefaultValueIndexMissing => {
                write!(f, "default value index is missing")
            }
            CompletionItemValueError::LineNumberMissing => write!(f, "line number is missing"),
            CompletionItemValueError::CharacterNumberMissing => {
                write!(f, "character number is missing")
            }
            CompletionItemValueError::ToManyValues => {
                write!(f, "value array contains more than three elements")
            }
        }
    }
}
impl Error for CompletionItemValueError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            CompletionItemValueError::NoVersion(err) => Some(err),
            _ => None,
        }
    }
}

pub enum CompletionItemValue {
    Class {
        version: Version,
    },
    Property {
        version: Version,
        class_name: String,
    },
    Default {
        version: Version,
        name_index: usize,
        value_index: usize,
    },
    Context {
        class_name: String,
        line: u32,
        character: u32,
        //TODO Range
    },
}
impl TryFrom<&Value> for CompletionItemValue {
    type Error = CompletionItemValueError;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        let Value::Array(array) = value else{return Err(CompletionItemValueError::NotAnArray);};
        let mut array_iter = array.iter();

        let Some(Value::Number(id)) = array_iter.next() else {return Err(CompletionItemValueError::NoId);};
        let Some(id) = id.as_u64() else {return Err(CompletionItemValueError::NoId);};

        let item = match id {
            // Class
            0 => {
                let Some(version) = array_iter.next() else {return Err(CompletionItemValueError::NoVersion(VersionValueError::NotAnArray));};
                let version = match Version::try_from(version) {
                    Ok(ok) => ok,
                    Err(err) => return Err(CompletionItemValueError::NoVersion(err)),
                };
                CompletionItemValue::Class { version }
            }
            // Property
            1 => {
                let Some(version) = array_iter.next() else {return Err(CompletionItemValueError::NoVersion(VersionValueError::NotAnArray));};
                let version = match Version::try_from(version) {
                    Ok(ok) => ok,
                    Err(err) => return Err(CompletionItemValueError::NoVersion(err)),
                };
                let Some(Value::String(class_name)) = array_iter.next() else {return Err(CompletionItemValueError::ClassNameMissing)};
                CompletionItemValue::Property {
                    version,
                    class_name: class_name.clone(),
                }
            }
            // Defaults
            2 => {
                let Some(version) = array_iter.next() else {return Err(CompletionItemValueError::NoVersion(VersionValueError::NotAnArray));};
                let version = match Version::try_from(version) {
                    Ok(ok) => ok,
                    Err(err) => return Err(CompletionItemValueError::NoVersion(err)),
                };
                let Some(Value::Number(number)) = array_iter.next() else{ return Err(CompletionItemValueError::DefaultNameIndexMissing);};
                let Some(name_index) = number.as_u64() else{return Err(CompletionItemValueError::DefaultNameIndexMissing);};
                let name_index = name_index as usize;
                let Some(Value::Number(number)) = array_iter.next() else{ return Err(CompletionItemValueError::DefaultValueIndexMissing);};
                let Some(value_index) = number.as_u64() else{return Err(CompletionItemValueError::DefaultValueIndexMissing);};
                let value_index = value_index as usize;
                CompletionItemValue::Default {
                    version,
                    name_index,
                    value_index,
                }
            }
            3 => {
                let Some(Value::String(class_name)) = array_iter.next() else {return Err(CompletionItemValueError::ClassNameMissing)};
                let Some(Value::Number(line)) = array_iter.next() else {return Err(CompletionItemValueError::LineNumberMissing)};
                let Some(line) = line.as_u64() else {return Err(CompletionItemValueError::LineNumberMissing)};
                let Some(Value::Number(character)) = array_iter.next() else {return Err(CompletionItemValueError::CharacterNumberMissing)};
                let Some(character) = character.as_u64() else {return Err(CompletionItemValueError::CharacterNumberMissing)};
                CompletionItemValue::Context {
                    class_name: class_name.clone(),
                    line: line as u32,
                    character: character as u32,
                }
            }
            _ => return Err(CompletionItemValueError::NoSupportedId(id)),
        };

        if array_iter.next().is_some() {
            return Err(CompletionItemValueError::ToManyValues);
        }

        Ok(item)
    }
}

impl From<CompletionItemValue> for Value {
    fn from(val: CompletionItemValue) -> Self {
        let vec = match val {
            CompletionItemValue::Class { version } => vec![0.into(), version.into()],
            CompletionItemValue::Property {
                version,
                class_name,
            } => vec![1.into(), version.into(), class_name.into()],
            CompletionItemValue::Default {
                version,
                name_index,
                value_index,
            } => {
                vec![
                    2.into(),
                    version.into(),
                    name_index.into(),
                    value_index.into(),
                ]
            }
            CompletionItemValue::Context {
                class_name,
                line,
                character,
            } => {
                vec![3.into(), class_name.into(), line.into(), character.into()]
            }
        };
        Value::Array(vec)
    }
}

pub async fn set_completion_response(backend: &Backend, params: &mut CompletionItem) {
    let completion_item_value = match &params.data {
        Some(some) => some,
        None => {
            backend
                .client
                .log_message(
                    MessageType::WARNING,
                    "Tried to set completion response, but there was no data",
                )
                .await;
            return;
        }
    };

    let completion_item_value = match CompletionItemValue::try_from(completion_item_value) {
        Ok(ok) => ok,
        Err(err) => {
            backend
                .client
                .log_message(
                    MessageType::ERROR,
                    format!("Tried to set completion response, but the data was in the wrong format. {}", err),
                )
                .await;
            return;
        }
    };

    match completion_item_value {
        CompletionItemValue::Class { version } => {
            backend
                .client
                .log_message(MessageType::INFO, "Get Class")
                .await;
            let Some(class) = backend.fds_classes_map.get(&version) else {return};
            backend
                .client
                .log_message(MessageType::INFO, "Get Class")
                .await;
            let Some(class) = class.get(&params.label) else {return};
            backend
                .client
                .log_message(MessageType::INFO, "Got Class")
                .await;

            params.documentation = Some(tower_lsp::lsp_types::Documentation::MarkupContent(
                tower_lsp::lsp_types::MarkupContent {
                    kind: tower_lsp::lsp_types::MarkupKind::Markdown,
                    value: class.to_markdown_string(),
                },
            ));
            backend
                .client
                .log_message(MessageType::INFO, class.to_markdown_string())
                .await;
        }
        CompletionItemValue::Property {
            version,
            class_name,
        } => {
            let Some(class) = backend.fds_classes_map.get(&version)else{return};
            let Some(class) = class.get(&class_name) else {return};
            let Some(property )= class.properties.get(&params.label) else {return};

            params.documentation = Some(tower_lsp::lsp_types::Documentation::MarkupContent(
                tower_lsp::lsp_types::MarkupContent {
                    kind: tower_lsp::lsp_types::MarkupKind::Markdown,
                    value: property.to_markdown_string(),
                },
            ));
        }
        CompletionItemValue::Default {
            version,
            name_index,
            value_index,
        } => {
            let Some(fds_defaults) = &backend.fds_defaults_map.get(&version) else {return};

            let fds_default = fds_defaults.index(name_index);

            let Some(text) = fds_default.get_element_markdown(value_index) else {return};

            params.documentation = Some(tower_lsp::lsp_types::Documentation::MarkupContent(
                tower_lsp::lsp_types::MarkupContent {
                    kind: tower_lsp::lsp_types::MarkupKind::Markdown,
                    value: text,
                },
            ));
        }
        CompletionItemValue::Context {
            class_name,
            line,
            character,
        } => {
            let text = format!(
                "Custom element from `{class_name}`  \nat Line: {} Column: {}",
                line + 1,
                character + 1
            );
            params.documentation = Some(tower_lsp::lsp_types::Documentation::MarkupContent(
                tower_lsp::lsp_types::MarkupContent {
                    kind: tower_lsp::lsp_types::MarkupKind::Markdown,
                    value: text,
                },
            ));
        }
    }
}
