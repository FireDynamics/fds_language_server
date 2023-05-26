use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, Range};

use crate::{
    completion::CompletionItemValue,
    parser::{Script, Token},
};

#[derive(Debug)]
pub struct Context {
    pub name: String,
    pub range: Range,
}

#[derive(Debug, Default)]
pub struct ContextMap(HashMap<String, Vec<Context>>);

impl ContextMap {
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

pub fn get_context(script: &Script) -> ContextMap {
    let mut content_map = ContextMap::default();

    script
        .iter()
        .filter_map(|(block, _)| {
            if let crate::parser::Block::Code(code) = block {
                Some(code)
            } else {
                None
            }
        })
        .for_each(|code| {
            let mut code = code.iter();

            code.next();
            let Some((Ok(Token::Class(class)), _)) = code.next() else {
                return;
            };

            while let Some((Ok(token), _)) = code.next() {
                if let Token::Property(prop) = token {
                    if prop == "ID" {
                        code.next();
                        if let Some((Ok(Token::String(id)), range)) = code.next() {
                            let context = match content_map.get_mut(class) {
                                Some(some) => some,
                                None => {
                                    let vec = vec![];
                                    content_map.insert(class.clone(), vec);
                                    content_map.get_mut(class).expect("error not possible since a vector is inserted right before")
                                }
                            };
                            context.push(Context { name: id.clone(), range: *range });
                            return;
                        }
                        return;
                    }
                }
            }
        });

    content_map
}
