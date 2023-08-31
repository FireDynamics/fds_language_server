//! Semantic Tokens to handle code highlight.

use std::mem::Discriminant;

use tower_lsp::lsp_types::SemanticTokenType;

/// Array of all possible semantic tokens wich can be represented
pub const SEMANTIC_TOKEN_LEGEND: [SemanticTokenType; 9] = [
    SemanticTokenType::new("start"),
    SemanticTokenType::CLASS,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::new("boolean"),
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::new("end"),
    SemanticTokenType::COMMENT,
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
    /// The Variable token, represented by text padded with #
    Variable,
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

#[test]
fn test_token_conversion() {
    assert_eq!(0, SemanticTokenTypeIndex::Start.value());
    assert_eq!(1, SemanticTokenTypeIndex::Class.value());
    assert_eq!(2, SemanticTokenTypeIndex::Property.value());
    assert_eq!(3, SemanticTokenTypeIndex::Bool.value());
    assert_eq!(4, SemanticTokenTypeIndex::String.value());
    assert_eq!(5, SemanticTokenTypeIndex::Number.value());
    assert_eq!(6, SemanticTokenTypeIndex::Variable.value());
    assert_eq!(7, SemanticTokenTypeIndex::End.value());
}
