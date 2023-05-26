//! A Language Server for [FDS](https://github.com/firemodels/fds)

#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]
#![warn(clippy::missing_errors_doc)]
#![warn(clippy::missing_panics_doc)]
#![warn(clippy::doc_markdown)]

mod code_lens;
mod completion;
mod context;
mod fds_classes;
mod fds_defaults;
mod parser;
mod semantic_token;
mod versions;

use completion::{get_completion_response, set_completion_response};
use context::ContextMap;
use dashmap::DashMap;
use fds_classes::FDSClasses;
use fds_defaults::FDSDefaults;
use parser::{Block, Script, Token};
use ropey::{self, Rope};
use semantic_token::convert_script_to_sematic_tokens;
use std::fmt::Display;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use versions::Version;

/// Default FDS classes file name for wich the program seaches in the version direcoty.
const FDS_CLASSES_FILE_NAME: &str = "fds_classes.csv";
/// Default FDS defaults file name for wich the program seaches in the version direcoty.
const FDS_DEFAULTS_FILE_NAME: &str = "fds_defaults.csv";

/// The backend of the server containing and handling all resources for code highelighting, autocomplete, etc.
#[derive(Debug)]
pub struct Backend {
    /// The client wich started the server.
    pub client: Client,
    /// All documents this server currently handles.
    pub document_map: DashMap<String, Rope>,

    /// All document converted to tokens.
    pub script_map: DashMap<String, Script>,
    /// All custom ids inside a document.
    pub context_map: DashMap<String, ContextMap>,

    /// The version of a document.
    pub versions_map: DashMap<String, Version>,
    /// All class info loaded from a file mapped to a version.
    pub fds_classes_map: DashMap<Version, FDSClasses>,
    /// All defaults info loaded from a file mapped to a version.
    pub fds_defaults_map: DashMap<Version, FDSDefaults>,
}

impl Backend {
    /// Get the version of a document.
    pub fn get_version(&self, uri: &String) -> Option<Version> {
        let version = self.versions_map.get(uri)?;
        Some(*version.value())
    }

    /// Shortcut for logging messages
    pub async fn log<M: Display>(&self, message: M) {
        self.client.log_message(MessageType::LOG, message).await
    }
    /// Shortcut for logging info messages
    pub async fn info<M: Display>(&self, message: M) {
        self.client.log_message(MessageType::INFO, message).await
    }
    /// Shortcut for logging warning messages
    pub async fn warning<M: Display>(&self, message: M) {
        self.client.log_message(MessageType::WARNING, message).await
    }
    /// Shortcut for logging error messages
    pub async fn error<M: Display>(&self, message: M) {
        self.client.log_message(MessageType::ERROR, message).await
    }

    /// Update the backend, when the document was changed.
    async fn on_change(&self, params: TextDocumentItem) {
        // Converts the current document to a Rope, so huge files can better handled.
        let rope = Rope::from_str(&params.text);
        let uri = params.uri.to_string();
        self.document_map.insert(uri.clone(), rope.clone());

        let script = Script::new(&rope);

        self.context_map
            .insert(uri.clone(), context::get_context(&script));
        self.script_map.insert(uri.clone(), script);

        //FIXME recovery if line count = 0
        //TODO message, when no version was found
        // Get the version of the current document. If no version was explicitly set, a default version is used. If the set Version could not befound, a less desting version is used (e.g. 6.1.4 -> 6). If no directory could be found either the fallback version is used too.
        match versions::get_version(rope.line(0).to_string()) {
            Ok((version, path_buf)) => {
                self.versions_map.insert(uri.clone(), version);
                if !self.fds_classes_map.contains_key(&version) {
                    self.fds_classes_map.insert(
                        version,
                        fds_classes::get_classes2(path_buf.join(FDS_CLASSES_FILE_NAME)),
                    );
                    self.fds_defaults_map.insert(
                        version,
                        fds_defaults::get_defaults2(path_buf.join(FDS_DEFAULTS_FILE_NAME)),
                    );
                    self.client
                        .log_message(
                            MessageType::INFO,
                            format! {"Loaded version {} data.", version},
                        )
                        .await;
                }
            }
            Err(err) => {
                self.client
                    .log_message(MessageType::INFO, format! {"Version error {}", err})
                    .await;
            }
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        // Log all environment variables
        self.log(format! {"{:?}", std::env::args()}).await;

        // Set all available services and their settings
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                // selection_range_provider: (),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(true),
                    trigger_characters: Some(vec!["&".to_string(), "=".to_string()]),
                    all_commit_characters: None,
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
                code_lens_provider: Some(CodeLensOptions {
                    resolve_provider: Some(true),
                }),
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
        self.log("server initialized!!!").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.info("file opened!").await;
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

        //TODO Move to on change function and the block to semantic tokens
        if let Some(script) = self.script_map.get(&uri.to_string()) {
            let script = script.value();

            let mut diagnostics = vec![];
            let list = script
                .iter()
                .filter_map(|(block, _)| match block {
                    Block::Code(value) => Some(value),
                    _ => None,
                })
                .collect::<Vec<_>>();

            if let Some(first) = list.first() {
                if let Some((Ok(Token::Class(class)), range)) = first.get(1) {
                    if class != "HEAD" {
                        diagnostics.push(Diagnostic {
                            range: *range,
                            message: "`&HEAD /` should be at the start.".to_string(),
                            ..Default::default()
                        });
                    }
                }
            } else {
                diagnostics.push(Diagnostic {
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
                    message: "`&HEAD /` is missing".to_string(),
                    ..Default::default()
                });
            }

            if let Some(last) = list.last() {
                if let Some((Ok(Token::Class(class)), range)) = last.get(1) {
                    if class != "TAIL" {
                        diagnostics.push(Diagnostic {
                            range: *range,
                            message: "`&TAIL /` should be at the end.".to_string(),
                            ..Default::default()
                        });
                    }
                }
            } else {
                diagnostics.push(Diagnostic {
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
                    message: "`&TAIL /` is missing".to_string(),
                    ..Default::default()
                });
            }

            list.iter()
                .flat_map(|f| {
                    f.iter()
                        .filter_map(|(token, range)| match token {
                            Ok(_) => None,
                            Err(value) => Some((value, range)),
                        })
                        .map(|(token, range)| token.get_diagnostic(*range))
                })
                .for_each(|f| diagnostics.push(f));

            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        self.document_map.remove(&uri);
        self.script_map.remove(&uri);
        self.context_map.remove(&uri);
        self.versions_map.remove(&uri);
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        get_completion_response(self, params).await
    }

    async fn completion_resolve(&self, mut params: CompletionItem) -> Result<CompletionItem> {
        set_completion_response(self, &mut params).await;
        Ok(params)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;

        let result = match self.script_map.get(&uri.to_string()) {
            Some(script) => {
                // Load the code highlight for the script.
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

        //TODO move to own file
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

            let version = self.versions_map.get(&uri.to_string())?;
            let version = version.value();
            let fds_classes = self.fds_classes_map.get(version)?;
            let fds_classes = fds_classes.value();

            let markdown = match token {
                Token::Class(value) => {
                    let class = fds_classes.get(value)?;
                    Some(class.to_markdown_string())
                }
                Token::Property(value) => {
                    let class_name = block.get_name()?;
                    let class = fds_classes.get(&class_name)?;
                    let property = class.properties.get(value)?;
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

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        code_lens::code_lens(self, params).await
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    // Creates and starts a new server.
    let (service, socket) = LspService::new(|client| Backend {
        client,
        document_map: DashMap::default(),
        script_map: DashMap::default(),
        context_map: DashMap::default(),
        versions_map: DashMap::default(),
        fds_classes_map: DashMap::default(),
        fds_defaults_map: DashMap::default(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
