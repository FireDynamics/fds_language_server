//! Semantic Tokens to handle code highlight.

use std::mem::Discriminant;

use tower_lsp::lsp_types::{self, Position, SemanticToken};

use crate::parser::{self, Block, Script, Token};

/// Array of all possible semantic tokens wich can be represented
pub const SEMANTIC_TOKEN_LEGEND: [lsp_types::SemanticTokenType; 8] = [
    lsp_types::SemanticTokenType::new("start"),
    lsp_types::SemanticTokenType::CLASS,
    lsp_types::SemanticTokenType::PROPERTY,
    lsp_types::SemanticTokenType::new("boolean"),
    lsp_types::SemanticTokenType::STRING,
    lsp_types::SemanticTokenType::NUMBER,
    lsp_types::SemanticTokenType::new("end"),
    lsp_types::SemanticTokenType::COMMENT,
];

/// Enum version auf the semantic tokens
pub enum SemanticTokenTypeIndex {
    /// The Start Token represented by `&`
    Start,
    /// The Class Token, also known as name list right after the start token
    Class,
    /// The Property Token wich is always before a `=`
    Property,
    /// The Boolean Token written as "T", "F", ".TRUE." or ".FALSE."
    Bool,
    /// The String Token delimitered  by `"` or `'`
    String,
    /// The Number Token wich represents any number
    Number,
    /// The End Token represented by `/`
    End,
    /// The Comment Token. Every Line that does not start with a Start Token or everything after the End Token.
    Comment,
}

impl SemanticTokenTypeIndex {
    /// Converts itself to the corresponding Token number.
    pub fn value(self) -> u32 {
        /// Helper union to convert from Token to number
        union Value {
            ///
            discriminant: Discriminant<SemanticTokenTypeIndex>,
            ///
            value: u32,
        }

        unsafe {
            Value {
                discriminant: std::mem::discriminant::<SemanticTokenTypeIndex>(&self),
            }
            .value
        }
    }
}

/// Get all semantic tokens from the script.
pub fn convert_script_to_sematic_tokens(script: &Script) -> Vec<SemanticToken> {
    let mut last_line = 0;
    let mut last_character = 0;
    let mut last_length = 0;

    /// Helper function to convert [`tower_lsp::lsp_types::Range`] to `delta_line`, `delta_start` and `length`.
    fn values_from_range(
        last_line: &mut u32,
        last_character: &mut u32,
        last_length: &mut u32,
        range: tower_lsp::lsp_types::Range,
    ) -> (u32, u32, u32) {
        let Position {
            line: start_line,
            character: start_character,
        } = range.start;
        let Position {
            character: end_character,
            ..
        } = range.end;

        let (delta_line, delta_start) = if start_line == *last_line {
            (0, start_character - *last_character + *last_length)
        } else {
            (start_line - *last_line, start_character)
        };

        let length = end_character.saturating_sub(start_character);

        *last_line = start_line;
        *last_character = end_character;
        *last_length = length;

        (delta_line, delta_start, length)
    }

    script
        .iter()
        .flat_map(|(block, range)| match block {
            Block::Comment => {
                let (delta_line, delta_start, length) = values_from_range(
                    &mut last_line,
                    &mut last_character,
                    &mut last_length,
                    *range,
                );

                let token = SemanticToken {
                    delta_line,
                    delta_start,
                    length,
                    token_type: SemanticTokenTypeIndex::Comment.value(),
                    token_modifiers_bitset: 0,
                };

                vec![token]
            }
            Block::Code(code) => code
                .iter()
                .filter_map(|(token, range)| match token {
                    Ok(token) => Some((token, range)),
                    Err(_) => None,
                })
                .filter_map(|(token, range)| {
                    let token_type = match token {
                        Token::Class(_) => Some(SemanticTokenTypeIndex::Class.value()),
                        Token::Property(_) => Some(SemanticTokenTypeIndex::Property.value()),
                        Token::Boolean(_) => Some(SemanticTokenTypeIndex::Bool.value()),
                        Token::String(_) => Some(SemanticTokenTypeIndex::String.value()),
                        Token::Number(_) => Some(SemanticTokenTypeIndex::Number.value()),
                        Token::Start => Some(SemanticTokenTypeIndex::Start.value()),
                        Token::End => Some(SemanticTokenTypeIndex::End.value()),
                        _ => None,
                    };

                    match token_type {
                        Some(token_type) => {
                            let (delta_line, delta_start, length) = values_from_range(
                                &mut last_line,
                                &mut last_character,
                                &mut last_length,
                                *range,
                            );

                            Some(SemanticToken {
                                delta_line,
                                delta_start,
                                length,
                                token_type,
                                token_modifiers_bitset: 0,
                            })
                        }
                        None => None,
                    }
                })
                .collect::<Vec<_>>(),
            parser::Block::ParseError(_) => vec![],
        })
        .collect::<Vec<_>>()
}

#[test]
fn test_token_conversion() {
    assert_eq!(0, SemanticTokenTypeIndex::Start.value());
    assert_eq!(1, SemanticTokenTypeIndex::Class.value());
    assert_eq!(2, SemanticTokenTypeIndex::Property.value());
    assert_eq!(3, SemanticTokenTypeIndex::Bool.value());
    assert_eq!(4, SemanticTokenTypeIndex::String.value());
    assert_eq!(5, SemanticTokenTypeIndex::Number.value());
    assert_eq!(6, SemanticTokenTypeIndex::End.value());
}

#[test]
fn test_convert_script_to_sematic_tokens() {
    use parser::Script;
    use ropey::Rope;
    use tower_lsp::lsp_types::SemanticToken;
    let script_text = r#"Comment
&CLASS STR="string" BOOL=T NUMBS=1,2,3 / Comment
Comment
"#;
    let script = Script::new(&Rope::from_str(script_text));

    let tokens = convert_script_to_sematic_tokens(&script);

    let right = vec![
        SemanticToken {
            delta_line: 0,
            delta_start: 0,
            length: 7,
            token_type: 7,
            token_modifiers_bitset: 0,
        },
        SemanticToken {
            delta_line: 1,
            delta_start: 1,
            length: 5,
            token_type: 1,
            token_modifiers_bitset: 0,
        },
        SemanticToken {
            delta_line: 0,
            delta_start: 6,
            length: 3,
            token_type: 2,
            token_modifiers_bitset: 0,
        },
        SemanticToken {
            delta_line: 0,
            delta_start: 4,
            length: 8,
            token_type: 4,
            token_modifiers_bitset: 0,
        },
        SemanticToken {
            delta_line: 0,
            delta_start: 9,
            length: 4,
            token_type: 2,
            token_modifiers_bitset: 0,
        },
        SemanticToken {
            delta_line: 0,
            delta_start: 5,
            length: 1,
            token_type: 3,
            token_modifiers_bitset: 0,
        },
        SemanticToken {
            delta_line: 0,
            delta_start: 2,
            length: 5,
            token_type: 2,
            token_modifiers_bitset: 0,
        },
        SemanticToken {
            delta_line: 0,
            delta_start: 6,
            length: 1,
            token_type: 5,
            token_modifiers_bitset: 0,
        },
        SemanticToken {
            delta_line: 0,
            delta_start: 2,
            length: 1,
            token_type: 5,
            token_modifiers_bitset: 0,
        },
        SemanticToken {
            delta_line: 0,
            delta_start: 2,
            length: 1,
            token_type: 5,
            token_modifiers_bitset: 0,
        },
        SemanticToken {
            delta_line: 0,
            delta_start: 4,
            length: 7,
            token_type: 7,
            token_modifiers_bitset: 0,
        },
        SemanticToken {
            delta_line: 1,
            delta_start: 0,
            length: 7,
            token_type: 7,
            token_modifiers_bitset: 0,
        },
    ];

    assert_eq!(tokens, right);
}
