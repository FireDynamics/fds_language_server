use std::mem::Discriminant;

use tower_lsp::lsp_types::{self, Position, SemanticToken};

use crate::parser::{self, Block, Script, Token};

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

pub enum SemanticTokenTypeIndex {
    START,
    CLASS,
    PROPERTY,
    BOOL,
    STRING,
    NUMBER,
    END,
    COMMENT,
}

impl SemanticTokenTypeIndex {
    pub fn value(self) -> u32 {
        union Value {
            discriminant: Discriminant<SemanticTokenTypeIndex>,
            value: u32,
        }

        unsafe {
            Value {
                discriminant: std::mem::discriminant::<SemanticTokenTypeIndex>(&self),
            }
            .value
        }
    }

    pub const _COUNT: usize = 7;

    pub fn _get_token_index_vector() -> Vec<u32> {
        (0u32..Self::_COUNT as u32).into_iter().collect()
    }
}

pub fn convert_script_to_sematic_tokens(script: &Script) -> Vec<SemanticToken> {
    let mut last_line = 0;
    let mut last_character = 0;
    let mut last_length = 0;

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

        let length = end_character - start_character;

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
                    token_type: SemanticTokenTypeIndex::COMMENT.value(),
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
                        Token::Class(_) => Some(SemanticTokenTypeIndex::CLASS.value()),
                        Token::Property(_) => Some(SemanticTokenTypeIndex::PROPERTY.value()),
                        Token::Boolean(_) => Some(SemanticTokenTypeIndex::BOOL.value()),
                        Token::String(_) => Some(SemanticTokenTypeIndex::STRING.value()),
                        Token::Number(_) => Some(SemanticTokenTypeIndex::NUMBER.value()),
                        Token::Start => Some(SemanticTokenTypeIndex::START.value()),
                        Token::End => Some(SemanticTokenTypeIndex::END.value()),
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
    assert_eq!(0, SemanticTokenTypeIndex::START.value());
    assert_eq!(1, SemanticTokenTypeIndex::CLASS.value());
    assert_eq!(2, SemanticTokenTypeIndex::PROPERTY.value());
    assert_eq!(3, SemanticTokenTypeIndex::BOOL.value());
    assert_eq!(4, SemanticTokenTypeIndex::STRING.value());
    assert_eq!(5, SemanticTokenTypeIndex::NUMBER.value());
    assert_eq!(6, SemanticTokenTypeIndex::END.value());
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
