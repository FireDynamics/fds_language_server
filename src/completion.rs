use dashmap::DashMap;
use serde_json::Value;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionResponse, CompletionTextEdit, MessageType, Position, TextEdit,
};

use crate::{
    fds_classes::FDSClass,
    fds_defaults,
    parser::{Script, Token, TokenError},
    Backend,
};

pub async fn get_completion_results(
    backend: &Backend,
    script: &Script,
    pos: Position,
) -> Option<CompletionResponse> {
    let block = script.get_block(pos)?;
    let result = block._get_token(pos);
    match result {
        //Classes
        Some((Ok(Token::Start), _)) | Some((Ok(Token::Class(_)), _)) => {
            let items = backend
                .fds_classes
                .iter()
                .map(|f| f.value().get_completion_item())
                .collect::<Vec<_>>();
            return Some(items.into());
        }
        //Properties
        Some((Ok(Token::Property(_)), _))
        | Some((Err(TokenError::Property), _))
        | Some((Err(TokenError::End), _))
        | None => {
            let name = block.get_name()?;
            let class = backend.fds_classes.get(&name)?;
            let items = class
                .properties
                .iter()
                .map(|(_, v)| v.get_completion_item(class.value().label.clone()))
                .collect::<Vec<_>>();
            return Some(items.into());
        }
        Some((Ok(Token::Equal), _))
        | Some((Ok(Token::Comma), _))
        | Some((Err(TokenError::PropertyValue), _)) => {
            backend
                .client
                .log_message(MessageType::INFO, format!("Autocomplete Defaults"))
                .await;

            let class = block.get_name()?;
            backend
                .client
                .log_message(MessageType::INFO, format!("Class: {}", class))
                .await;
            let (property, _) = block.get_recent_property(pos)?;
            backend
                .client
                .log_message(MessageType::INFO, format!("Property: {}", property))
                .await;

            let fds_defaults = backend.fds_defaults.lock().await;

            let completion_items = fds_defaults
                .iter()
                .enumerate()
                .filter(|(_, fds_default)| fds_default.is_item(&class, &property))
                .flat_map(|(i, fds_default)| fds_default.get_completion_items(i))
                .collect::<Vec<_>>();

            backend
                .client
                .log_message(MessageType::INFO, format!("Items: {:?}", completion_items))
                .await;

            return Some(completion_items.into());
        }
        _ => {}
    }

    None
}

pub async fn set_completion_response(backend: &Backend, params: &mut CompletionItem) {
    let Some(Value::Array(array)) = &params.data else{return};

    let mut array_iter = array.iter();

    let Some(Value::Number(id)) = array_iter.next()else{return};
    let Some(id) = id.as_u64() else {return};

    match id {
        0 => {
            // Class
            let Some(class) = backend.fds_classes.get(&params.label) else {return};
            let class = class.value();

            params.documentation = Some(tower_lsp::lsp_types::Documentation::MarkupContent(
                tower_lsp::lsp_types::MarkupContent {
                    kind: tower_lsp::lsp_types::MarkupKind::Markdown,
                    value: class.to_markdown_string(),
                },
            ));
        }
        1 => {
            //Property
            let Some(Value::String(class)) = array_iter.next() else {return};
            let Some(class) = &backend.fds_classes.get(class) else {return};
            let class = class.value();
            let Some(property )= class.properties.get(&params.label) else {return};

            params.documentation = Some(tower_lsp::lsp_types::Documentation::MarkupContent(
                tower_lsp::lsp_types::MarkupContent {
                    kind: tower_lsp::lsp_types::MarkupKind::Markdown,
                    value: property.to_markdown_string(),
                },
            ));
        }
        2 => {
            //Defaults
            let Some(Value::Number(number)) = array_iter.next() else{return};
            let Some(index) = number.as_u64() else {return};
            let index = index as usize;

            let fds_defaults = &backend.fds_defaults.lock().await;

            let fds_default = &fds_defaults[index];
            let Some(text) = fds_default.get_element_markdown(&params.label) else {return};

            params.documentation = Some(tower_lsp::lsp_types::Documentation::MarkupContent(
                tower_lsp::lsp_types::MarkupContent {
                    kind: tower_lsp::lsp_types::MarkupKind::Markdown,
                    value: text,
                },
            ));

            // params.text_edit = Some(CompletionTextEdit::Edit(TextEdit {
            //     new_text: format!("\"{}\"", params.label),
            //     ..Default::default()
            // }));
        }

        _ => return,
    }
}
