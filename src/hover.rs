//! Add hover support vor the server.

use tower_lsp::{
    jsonrpc::Error,
    lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind},
};

use crate::{parser::Token, Backend};

/// Determine the markdown content for namespaces and properties when hovered over.
pub async fn hover(
    backend: &Backend,
    params: HoverParams,
) -> std::result::Result<Option<Hover>, Error> {
    let uri = params.text_document_position_params.text_document.uri;

    let hover = || -> Option<Hover> {
        let script = backend.script_map.get(&uri.to_string())?;
        let script = script.value();
        let pos = params.text_document_position_params.position;

        let tokens = script
            .iter()
            .filter(|f| matches!(f.token, Token::Class(_) | Token::Property(_)))
            .collect::<Vec<_>>();

        let (index, range) = tokens
            .iter()
            .enumerate()
            .map(|(i, token_data)| (i, token_data.span.lsp_span))
            .find(|(_, range)| range.start <= pos && range.end >= pos)?;

        let version = backend.versions_map.get(&uri.to_string())?;
        let version = version.value();
        let fds_classes = backend.fds_classes_map.get(version)?;
        let fds_classes = fds_classes.value();

        let markdown = match &tokens[index].token {
            Token::Class(name) => {
                let class = fds_classes.get(name)?;
                Some(class.to_markdown_string())
            }
            Token::Property(name) => {
                if let Some(Token::Class(class_name)) = tokens
                    .iter()
                    .take(index)
                    .map(|f| &f.token)
                    .filter(|f| matches!(f, Token::Class(_)))
                    .last()
                {
                    let class = fds_classes.get(class_name)?;
                    let property = class.properties.get(name)?;
                    Some(property.to_markdown_string())
                } else {
                    None
                }
            }
            _ => None,
        }?;

        let hover = Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: markdown,
            }),
            range: Some(range),
        };

        Some(hover)
    }();

    Ok(hover)
}
