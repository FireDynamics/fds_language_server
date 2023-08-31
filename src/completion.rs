//! Add auto completion support for the server.

use std::{error::Error, fmt::Display, ops::Index};

use serde_json::Value;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionParams, CompletionResponse, MessageType, Range,
};

use crate::{
    parser::Token,
    versions::{Version, VersionValueError},
    Backend,
};

use tower_lsp::jsonrpc::Result;

/// The errors that can occur while auto completion
#[derive(Debug)]
#[allow(clippy::enum_variant_names)]
pub enum CompletionResponseError {
    /// The Script is missing
    NoScript(String),
    /// No Version could be found
    NoVersion(String),
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

/// Get the completion response for all supported systems
///
/// Currently supported:
/// - default classes and properties
/// - default values
/// - Context for classes with an `ID` property
pub async fn get_completion_response(
    backend: &Backend,
    params: CompletionParams,
) -> Result<Option<CompletionResponse>> {
    /// Helper function to get the closest property and class name.
    /// The iterator is wandered until the start of the current code block.
    /// the fist property and the class are returned.
    fn closest_property_and_class<'a>(
        iter: impl Iterator<Item = (Range, &'a Token)>,
    ) -> Option<(&'a str, &'a str)> {
        let mut iter = iter
            .map(|f| f.1)
            .take_while(|f| !matches!(f, Token::Start))
            .filter(|f| matches!(f, Token::Class(_) | Token::Property(_)));
        if let (Some(Token::Property(property_name)), Some(Token::Class(class_name))) =
            (iter.next(), iter.last())
        {
            Some((property_name, class_name))
        } else {
            None
        }
    }

    /// Helper function to get the closest class name.
    /// The iterator is wandered until the start of the current code block.
    /// The class is returned.
    fn closest_class<'a>(iter: impl Iterator<Item = (Range, &'a Token)>) -> Option<&'a str> {
        if let Some(Token::Class(class_name)) = iter
            .map(|f| f.1)
            .take_while(|f| !matches!(f, Token::Start))
            .find(|f| matches!(f, Token::Class(_)))
        {
            Some(class_name)
        } else {
            None
        }
    }

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

    let mut iter = script.iter().map(|f| (f.span.lsp_span, &f.token)).rev();

    while let Some((range, token)) = iter.next() {
        if range.start <= pos && range.end >= pos {
            match token {
                crate::Token::Start | crate::Token::Class(_) => {
                    let items = get_completion_classes(backend, &version);
                    return Ok(items);
                }
                crate::Token::Property(_) => {
                    if let Some(class_name) = closest_class(iter) {
                        let items = get_completion_properties(backend, class_name, &version)
                            .map(|f| f.into());
                        return Ok(items);
                    }
                }
                crate::Token::Equal => {
                    if let Some((property_name, class_name)) = closest_property_and_class(iter) {
                        let items = get_completion_equal(
                            backend,
                            class_name,
                            property_name,
                            &uri,
                            &version,
                        );
                        return Ok(items.map(|f| f.into()));
                    }
                }
                crate::Token::Comma => {
                    if let Some((property_name, class_name)) = closest_property_and_class(iter) {
                        let items_property =
                            get_completion_properties(backend, class_name, &version);
                        let items_equal = get_completion_equal(
                            backend,
                            class_name,
                            property_name,
                            &uri,
                            &version,
                        );

                        let items = match (items_property, items_equal) {
                            (None, None) => None,
                            (None, Some(items)) | (Some(items), None) => Some(items.into()),
                            (Some(items_property), Some(items_equal)) => {
                                Some([items_property, items_equal].concat().into())
                            }
                        };
                        return Ok(items);
                    }
                }
                crate::Token::Error => {
                    continue;
                }
                _ => {}
            }
            break;
        } else if range.end < pos {
            match token {
                // Wenn Comma, oder Klasse zuvor sind
                crate::Token::Class(class_name) => {
                    let items =
                        get_completion_properties(backend, class_name, &version).map(|f| f.into());
                    return Ok(items);
                }
                crate::Token::Property(_)
                | crate::Token::Number(_)
                | crate::Token::Boolean(_)
                | crate::Token::String(_) => {
                    if let Some(class_name) = closest_class(iter) {
                        let items = get_completion_properties(backend, class_name, &version)
                            .map(|f| f.into());
                        return Ok(items);
                    }
                }
                crate::Token::Equal => {
                    if let Some((property_name, class_name)) = closest_property_and_class(iter) {
                        let items = get_completion_equal(
                            backend,
                            class_name,
                            property_name,
                            &uri,
                            &version,
                        );
                        return Ok(items.map(|f| f.into()));
                    }
                }
                crate::Token::Comma => {
                    if let Some((property_name, class_name)) = closest_property_and_class(iter) {
                        let items_property =
                            get_completion_properties(backend, class_name, &version);
                        let items_equal = get_completion_equal(
                            backend,
                            class_name,
                            property_name,
                            &uri,
                            &version,
                        );

                        let items = match (items_property, items_equal) {
                            (None, None) => None,
                            (None, Some(items)) | (Some(items), None) => Some(items.into()),
                            (Some(items_property), Some(items_equal)) => {
                                Some([items_property, items_equal].concat().into())
                            }
                        };
                        return Ok(items);
                    }
                }
                _ => {}
            }
            break;
        }
    }

    Ok(None)
}

/// Get the completion response for classes
fn get_completion_classes(backend: &Backend, version: &Version) -> Option<CompletionResponse> {
    let items = backend
        .fds_classes_map
        .get(version)?
        .values()
        .map(|f| f.get_completion_item(*version))
        .collect::<Vec<_>>();
    Some(items.into())
}

/// Get the completion response for properties
fn get_completion_properties(
    backend: &Backend,
    class_name: &str,
    version: &Version,
) -> Option<Vec<CompletionItem>> {
    let class = backend.fds_classes_map.get(version)?;
    let class = class.get(class_name)?;
    Some(
        class
            .properties
            .values()
            .map(|v| v.get_completion_item(class.label.clone(), *version))
            .collect(),
    )
}

/// Get the completion response for property values
fn get_completion_equal(
    backend: &Backend,
    class_name: &str,
    property_name: &str,
    uri: &String,
    version: &Version,
) -> Option<Vec<CompletionItem>> {
    let fds_defaults = backend.fds_defaults_map.get(version)?;

    let mut completion_items = fds_defaults
        .iter()
        .enumerate()
        .filter(|(_, fds_default)| fds_default.is_item(class_name, property_name))
        .flat_map(|(i, fds_default)| fds_default.get_completion_items(i, *version))
        .collect::<Vec<_>>();

    if property_name.ends_with("_ID") {
        if let Some(context_map) = backend.context_map.get(uri) {
            let class = property_name
                .strip_suffix("_ID")
                .expect("Always has suffix.")
                .to_string();
            let mut items = context_map.get_completion_items(&class);
            completion_items.append(&mut items);
        }
    }

    Some(completion_items)
}

/// All errors that can occur by converting a [`Value`] to a [`CompletionItemValue`]
#[derive(Debug)]
pub enum CompletionItemValueError {
    /// The root value is not an Array
    NotAnArray,
    /// Unable to convert a value to a valide id
    NoSupportedId(u64),
    /// The id value is missing
    NoId,
    /// The version value is missing
    NoVersion(VersionValueError),
    /// The class name is missing
    ClassNameMissing,
    /// the index for a default name is missing
    DefaultNameIndexMissing,
    /// the index for a default value is missing
    DefaultValueIndexMissing,
    /// The line number is missing for the context
    LineNumberMissing,
    /// The char number is missing for the context
    CharacterNumberMissing,
    /// The array contains to many values
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

/// The the strictly typed value architecture  
pub enum CompletionItemValue {
    /// Architektur if the completion item is a class
    Class {
        /// the version of the document
        version: Version,
    },
    /// Architektur if the completion item is a property
    Property {
        /// the version of the document
        version: Version,
        /// the class name this property contains to
        class_name: String,
    },
    /// Architektur if the completion item is a default value
    Default {
        /// the version of the document
        version: Version,
        /// The default name index
        name_index: usize,
        /// The default value index
        value_index: usize,
    },
    /// Architektur if the completion item is a context value
    Context {
        /// The class name witch the context correspond to
        class_name: String,
        /// The line where the context value starts
        line: u32,
        /// The char where the context value starts
        character: u32,
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

/// Injects markdown to the selected completion response
pub async fn set_completion_response(backend: &Backend, params: &mut CompletionItem) {
    // Get the value of the current selected response
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

    // convert the value to the strong typed CompletionItemValue
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

    // Get the markdown based on the value
    match completion_item_value {
        CompletionItemValue::Class { version } => {
            let Some(class) = backend.fds_classes_map.get(&version) else {return};
            let Some(class) = class.get(&params.label) else {return};
            params.documentation = Some(tower_lsp::lsp_types::Documentation::MarkupContent(
                tower_lsp::lsp_types::MarkupContent {
                    kind: tower_lsp::lsp_types::MarkupKind::Markdown,
                    value: class.to_markdown_string(),
                },
            ));
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
