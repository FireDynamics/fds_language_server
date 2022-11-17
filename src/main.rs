mod completion;
mod fds_classes;
mod parser;
mod semantic_token;

use completion::{get_completion_results, set_completion_response};
use dashmap::DashMap;
use fds_classes::FDSClass;
use parser::{Block, Script, Token};
use ropey::{self, Rope};
use semantic_token::convert_script_to_sematic_tokens;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    document_map: DashMap<String, Rope>,
    fds_classes: DashMap<String, FDSClass>,
    script_map: DashMap<String, Script>,
}

impl Backend {
    async fn on_change(&self, params: TextDocumentItem) {
        let rope = Rope::from_str(&params.text);
        self.document_map
            .insert(params.uri.to_string(), rope.clone());
        self.script_map
            .insert(params.uri.to_string(), Script::new(&rope));
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        self.client
            .log_message(MessageType::INFO, format! {"{:?}", std::env::args()})
            .await;

        let Some(path) = std::env::args().find(|f| f.starts_with("RUST_FDS_CLASSES_PATH")) else {return Err(tower_lsp::jsonrpc::Error::invalid_params(format!("Environment variable RUST_FDS_CLASSES_PATH was not found")))};

        let Some(path) = path.split('=').last() else{ return Err(tower_lsp::jsonrpc::Error::invalid_params(format!("Environment variable RUST_FDS_CLASSES_PATH was defined wrong. '{path}'")))};

        for class in fds_classes::get_classes(path) {
            self.fds_classes.insert(class.label.clone(), class);
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                // selection_range_provider: (),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(true),
                    trigger_characters: Some(vec!["&".to_string()]),
                    ..Default::default()
                }),
                // signature_help_provider: (),
                // definition_provider: (),
                // type_definition_provider: (),
                // implementation_provider: (),
                // references_provider: (),
                // document_highlight_provider: (),
                // document_symbol_provider: (),
                // workspace_symbol_provider: (),
                // code_action_provider: (),
                // code_lens_provider: (),
                // document_formatting_provider: (),
                // document_range_formatting_provider: (),
                // document_on_type_formatting_provider: (),
                // rename_provider: (),
                // document_link_provider: (),
                // color_provider: (),
                // folding_range_provider: (),
                // declaration_provider: (),
                // execute_command_provider: (),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                // call_hierarchy_provider: (),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                token_types: semantic_token::SEMANTIC_TOKEN_LEGEND.to_vec(),
                                token_modifiers: vec![],
                            },
                            full: Some(SemanticTokensFullOptions::Delta { delta: Some(true) }),

                            ..Default::default()
                        },
                    ),
                ),
                // moniker_provider: (),
                // linked_editing_range_provider: (),
                // experimental: (),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!!!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
            language_id: String::default(),
        })
        .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        self.on_change(TextDocumentItem {
            uri: uri.clone(),
            text: std::mem::take(&mut params.content_changes[0].text),
            version: params.text_document.version,
            language_id: String::default(),
        })
        .await;

        if let Some(script) = self.script_map.get(&uri.to_string()) {
            let script = script.value();

            let diagnostics = script
                .iter()
                .filter_map(|(block, _)| match block {
                    Block::Code(value) => Some(value),
                    _ => None,
                })
                .map(|f| {
                    f.iter()
                        .filter_map(|(token, range)| match token {
                            Ok(_) => None,
                            Err(value) => Some((value, range)),
                        })
                        .map(|(token, range)| token.to_diagnostic(*range))
                })
                .flatten()
                .collect::<Vec<_>>();

            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        self.document_map.remove(&uri);
        self.script_map.remove(&uri);
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let pos = params.text_document_position.position;
        if let Some(script) = self.script_map.get(&uri) {
            return Ok(get_completion_results(
                &self.fds_classes,
                script.value(),
                pos,
            ));
        }
        Ok(None)
    }

    async fn completion_resolve(&self, mut params: CompletionItem) -> Result<CompletionItem> {
        set_completion_response(&self.fds_classes, &mut params);
        Ok(params)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;

        let result = match self.script_map.get(&uri.to_string()) {
            Some(script) => {
                let tokens = convert_script_to_sematic_tokens(script.value());
                let tokens = SemanticTokens {
                    data: tokens,
                    result_id: None,
                };
                Some(SemanticTokensResult::Tokens(tokens))
            }
            None => None,
        };

        Ok(result)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;

        let hover = || -> Option<Hover> {
            let script = self.script_map.get(&uri.to_string())?;
            let pos = params.text_document_position_params.position;
            let script = script.value();
            let block = script.get_block(pos)?;
            let (token, range) = &block._get_token(pos)?;
            let token = match token {
                Ok(value) => Some(value),
                _ => None,
            }?;
            let markdown = match token {
                Token::Class(value) => {
                    let class = self.fds_classes.get(value)?;
                    Some(class.value().to_markdown_string())
                }
                Token::Property(value) => {
                    let class_name = block.get_name()?;
                    let class = self.fds_classes.get(&class_name)?;
                    let property = class.value().properties.get(value)?;
                    Some(property.to_markdown_string())
                }
                _ => None,
            }?;

            let hover = Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: markdown,
                }),
                range: Some(*range),
            };

            Some(hover)
        }();
        Ok(hover)
    }
}
#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        document_map: DashMap::default(),
        fds_classes: DashMap::default(),
        script_map: DashMap::default(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
