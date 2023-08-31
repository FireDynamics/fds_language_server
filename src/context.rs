//! Add auto completion support for user defined IDs

use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, Range};

use crate::{
    completion::CompletionItemValue,
    parser::{ScriptData, Token},
};

/// A item that can be referenced.
#[derive(Debug)]
pub struct Context {
    /// The value of the item.
    pub name: String,
    /// The position of the item.
    pub range: Range,
}

/// A wrapper for [`HashMap<String, Vec<Context>>`]. Containing a Hash map of the class name and all corresponding ids.
#[derive(Debug, Default)]
pub struct ContextMap(HashMap<String, Vec<Context>>);

impl ContextMap {
    /// Get all defined contexts for a given class as completion items
    pub fn get_completion_items(&self, class: &String) -> Vec<CompletionItem> {
        if let Some(vec) = self.get(class) {
            vec.iter()
                .map(|f| CompletionItem {
                    label: format!("\"{}\"", f.name),
                    data: Some(
                        CompletionItemValue::Context {
                            class_name: class.clone(),
                            line: f.range.start.line,
                            character: f.range.start.character,
                        }
                        .into(),
                    ),
                    kind: Some(CompletionItemKind::VARIABLE),
                    ..Default::default()
                })
                .collect::<Vec<CompletionItem>>()
        } else {
            vec![]
        }
    }
}

impl Deref for ContextMap {
    type Target = HashMap<String, Vec<Context>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for ContextMap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Get the [`ContextMap`] from the script.
pub fn get_context(script: &ScriptData) -> ContextMap {
    let mut content_map = ContextMap::default();

    let mut iter = script.iter().map(|f| (f.span.lsp_span, &f.token));

    while let Some((_, token)) = iter.next() {
        if let Token::Class(class_name) = token {
            while let Some((_, token)) = iter.next() {
                match token {
                    Token::Property(property_name) => {
                        if property_name == "ID" {
                            iter.next(); // Equal
                            let Some((range,Token::String(name))) = iter.next() else {continue;};
                            let context = Context {
                                name: name.clone(),
                                range,
                            };
                            if let Some(vec) = content_map.get_mut(class_name) {
                                vec.push(context);
                            } else {
                                content_map.insert(class_name.clone(), vec![context]);
                            }
                        }
                    }
                    Token::End => break,
                    _ => {}
                }
            }
        }
    }

    content_map
}
