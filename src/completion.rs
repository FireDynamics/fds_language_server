use dashmap::DashMap;
use serde_json::Value;
use tower_lsp::lsp_types::{CompletionItem, CompletionResponse, Position};

use crate::{
    fds_classes::FDSClass,
    parser::{Script, Token, TokenError},
};

pub fn get_completion_results(
    classes: &DashMap<String, FDSClass>,
    script: &Script,
    pos: Position,
) -> Option<CompletionResponse> {
    let block = script.get_block(pos)?;
    let result = block._get_token(pos);
    match result {
        Some((Ok(Token::Start), _)) | Some((Ok(Token::Class(_)), _)) => {
            let items = classes
                .iter()
                .map(|f| f.value().get_completion_item())
                .collect::<Vec<_>>();
            return Some(items.into());
        }
        Some((Ok(Token::Property(_)), _))
        | Some((Err(TokenError::Property), _))
        | Some((Err(TokenError::End), _))
        | None => {
            let name = block.get_name()?;
            let class = classes.get(&name)?;
            let items = class
                .properties
                .iter()
                .map(|(_, v)| v.get_completion_item(class.value().label.clone()))
                .collect::<Vec<_>>();
            return Some(items.into());
        }
        _ => {}
    }

    None
}

pub fn set_completion_response(
    classes: &DashMap<String, FDSClass>,
    params: &mut CompletionItem,
    //data: Option<Value>,
) -> Option<()> {
    let Some(data) = &params.data else{return None};

    let markdown = match data {
        Value::String(value) => {
            let class = classes.get(value)?;
            let class = class.value();

            class.to_markdown_string()
        }
        Value::Array(value) => {
            let mut iter = value.into_iter();
            let Value::String( class_name) = iter.next()? else {return None};
            let class = classes.get(class_name)?;
            let class = class.value();
            let Value::String(property_name) = iter.next()? else {return None};
            let property = class.properties.get(property_name)?;

            property.to_markdown_string()
        }

        _ => return None,
    };

    params.documentation = Some(tower_lsp::lsp_types::Documentation::MarkupContent(
        tower_lsp::lsp_types::MarkupContent {
            kind: tower_lsp::lsp_types::MarkupKind::Markdown,
            value: markdown,
        },
    ));

    None
}
