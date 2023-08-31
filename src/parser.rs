//! Parse the document to tokens

use std::{error::Error, fmt::Display, ops::Deref};

use chumsky::{error::Cheap, Parser};
use ropey::Rope;
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticSeverity, Position, Range as LspSpan, SemanticToken,
};

use crate::semantic_token::SemanticTokenTypeIndex;
/// Alias for [`Range<usize>`] for easier type recognition
type RopeSpan = std::ops::Range<usize>;

/// Wrapper for an error that may occur if the parsing was implemented incorrectly.
#[derive(Debug)]
pub struct ScriptParseError(Vec<Cheap<char>>);

impl Display for ScriptParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "failed to parse text. {:?}", self.0)
    }
}
impl Error for ScriptParseError {}

/// All possible Tokens the text can consist of.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// The start token represented by `&`
    Start,
    /// The class token, any text after the start token
    Class(String),
    /// The property toke, any text before `=`
    Property(String),
    /// The number token, any kind of number
    Number(f32),
    /// The bool token, one of `.TRUE.`, `.FALSE.`, `T` or `F`.
    Boolean(bool),
    /// The string token, delimitered by `"` or `'`
    String(String),
    /// The Variable token, represented by text padded with #
    Variable(String),
    /// The comma token represented by `,`
    Comma,
    /// The equal token represented by `=`
    Equal,
    /// The end token represented by `/`
    End,
    /// Empty token wich should always be pared with at least on TokenInfo
    Error,
    /// Text that is marked as Comment and should be ignored in the most cases.
    Comment,
}

/// Container for the rope span, which is the total char position for start and end, and lsp span, which is consist of line and char for start and end.
#[derive(Debug, Clone)]
pub struct Span {
    /// Span which is the total char position for start and end.
    pub rope_span: RopeSpan,
    /// Span which is consist of line and char for start and end.
    pub lsp_span: LspSpan,
}

/// All additional data that a token can have.
#[derive(Debug)]
pub struct TokenData {
    /// The token itself.
    pub token: Token,
    /// The position of the token.
    pub span: Span,
    /// Some information about the token, like hints, warnings and errors.
    pub info: Option<Vec<TokenInfo>>,
}

/// Continer for all tokens inside a file.
#[derive(Debug)]
pub struct ScriptData(Vec<TokenData>);
impl Deref for ScriptData {
    type Target = Vec<TokenData>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl ScriptData {
    /// Convert text from a file in [`ScriptData`].
    ///
    /// # Errors
    ///
    /// This function will return an error if the text could not be parsed in to tokens.
    pub fn try_from_rope(rope: &Rope) -> Result<ScriptData, ScriptParseError> {
        let result = match parse_helper::script_parser().parse(rope.to_string()) {
            Ok(ok) => ok,
            Err(err) => return Err(ScriptParseError(err)),
        };

        let mut data = result
            .into_iter()
            .map(|(token, rope_span, info)| {
                let start = {
                    let pos = rope_span.start;
                    let l = rope.char_to_line(pos);
                    let c = pos - rope.line_to_char(l);

                    Position::new(l as u32, c as u32)
                };
                let end = {
                    let pos = rope_span.end;
                    let l = rope.char_to_line(pos);
                    let c = pos - rope.line_to_char(l);

                    Position::new(l as u32, c as u32)
                };

                let lsp_span = tower_lsp::lsp_types::Range::new(start, end);

                TokenData {
                    token,
                    span: Span {
                        rope_span,
                        lsp_span,
                    },
                    info,
                }
            })
            .collect::<Vec<TokenData>>();

        {
            let mut iter = data
                .iter_mut()
                .filter(|f| matches!(f.token, Token::Class(_)));

            // Check if Head is the first class
            if let Some(token_data) = iter.next() {
                if Token::Class("HEAD".to_string()) != token_data.token {
                    match token_data.info.as_mut() {
                        Some(vec) => vec.insert(0, TokenInfo::WarnHeadMissing),
                        None => token_data.info = Some(vec![TokenInfo::WarnHeadMissing]),
                    }
                }
            }
            // Check if TAIL is the last class
            if let Some(token_data) = iter.last() {
                if Token::Class("TAIL".to_string()) != token_data.token {
                    match token_data.info.as_mut() {
                        Some(vec) => vec.insert(0, TokenInfo::WarnTailMissing),
                        None => token_data.info = Some(vec![TokenInfo::WarnTailMissing]),
                    }
                }
            }
        }

        Ok(ScriptData(data))
    }

    /// Determind the [`SemanticToken`] inside this [`ScriptData`], with the relative positions.
    pub fn semantic_tokens(&self) -> Vec<SemanticToken> {
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

        self.iter()
            .filter_map(|f| {
                let token_type = match f.token {
                    crate::parser::Token::Start => SemanticTokenTypeIndex::Start,
                    crate::parser::Token::Class(_) => SemanticTokenTypeIndex::Class,
                    crate::parser::Token::Property(_) => SemanticTokenTypeIndex::Property,
                    crate::parser::Token::Number(_) => SemanticTokenTypeIndex::Number,
                    crate::parser::Token::Boolean(_) => SemanticTokenTypeIndex::Bool,
                    crate::parser::Token::String(_) => SemanticTokenTypeIndex::String,
                    crate::parser::Token::Variable(_) => SemanticTokenTypeIndex::Variable,
                    crate::parser::Token::Comma => return None,
                    crate::parser::Token::Equal => return None,
                    crate::parser::Token::End => SemanticTokenTypeIndex::End,
                    crate::parser::Token::Error => return None,
                    crate::parser::Token::Comment => SemanticTokenTypeIndex::Comment,
                }
                .value();
                let range = f.span.lsp_span;
                Some((range, token_type))
            })
            .map(|(range, token_type)| {
                let (delta_line, delta_start, length) =
                    values_from_range(&mut last_line, &mut last_character, &mut last_length, range);

                SemanticToken {
                    delta_line,
                    delta_start,
                    length,
                    token_type,
                    token_modifiers_bitset: 0,
                }
            })
            .collect()
    }
}

/// All possible hints, infos, warnings and errors this language server can find.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenInfo {
    /// When a string is padded in single qoutes instead of double qoutes.
    HintUseDoubleQuotes,
    /// When a float number ends with a dot e.g. 1.
    HintFloatNumber,
    /// When T and F are used as booleans instead of .TRUE. and .FALSE.
    HintUseVerboseBoolean,
    /// When after the property assignment a comma is missing.
    HintComma,
    /// When the first namespace is not &HEAD.
    WarnHeadMissing,
    /// When the last namespace is not &TAIL.
    WarnTailMissing,
    /// When the class name does not consist of 4 chars.
    WarnClassNameLength,
    /// When the class name consist of some lower case letters.
    WarnClassSomeLowercase,
    /// When the property name consist of some lower case letters.
    WarnPropertySomeLowercase,
    /// When the class name is missing.
    ErrorClassNameMissing,
    /// When the property was defined, but no assignment is happening.
    ErrorPropertyAssignmentMissing,
    /// The value of a property has an unknown type.
    ErrorUnknownPropertyValue,
    /// The end char is missing
    ErrorEndCharMissing,
}
impl TokenInfo {
    /// Determine the diagnostig for [`TokenInfo`]
    pub fn diagnostic(&self, range: tower_lsp::lsp_types::Range) -> Diagnostic {
        match self {
            TokenInfo::HintUseDoubleQuotes => Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::HINT),
                message: "It is recommended to use double quotes to define a string.".to_string(),
                ..Default::default()
            },
            TokenInfo::HintFloatNumber => Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::HINT),
                message: "It is recommended to add at least one number after a dot to define a float value.".to_string(),
                ..Default::default()
            },
            TokenInfo::HintUseVerboseBoolean => Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::HINT),
                message: "It is recommended to use verbose booleans `.TRUE.` and `.FALSE.`.".to_string(),
                ..Default::default()
            },
            TokenInfo::HintComma => Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::HINT),
                message: "It is recommended to the property assignment with a comma.".to_string(),
                ..Default::default()
            },
            TokenInfo::WarnHeadMissing => Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::WARNING),
                message: "First namespace should be `&HEAD`.".to_string(),
                ..Default::default()
            },
            TokenInfo::WarnTailMissing => Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::WARNING),
                message: "Last namespace should be `&TAIL`.".to_string(),
                ..Default::default()
            },
            TokenInfo::WarnClassNameLength => Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::WARNING),
                message: "Namespace does not consist of four letters, therefor might not be valide.".to_string(),
                ..Default::default()
            },
            TokenInfo::WarnClassSomeLowercase => Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::WARNING),
                message: "Namespace consist of some lowercase letters, therefor might not be valide.".to_string(),
                ..Default::default()
            },
            TokenInfo::WarnPropertySomeLowercase => Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::WARNING),
                message: "Property consist of some lowercase letters, therefor might not be valide.".to_string(),
                ..Default::default()
            },
            TokenInfo::ErrorClassNameMissing => Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                message: "Namespace is missing.".to_string(),
                ..Default::default()
            },
            TokenInfo::ErrorPropertyAssignmentMissing => Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                message: "Property assignment is missing.".to_string(),
                ..Default::default()
            },
            TokenInfo::ErrorUnknownPropertyValue => Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                message: "Property has an unknown value and could not be parsed.".to_string(),
                ..Default::default()
            },
            TokenInfo::ErrorEndCharMissing => Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                message: "End char `/` is missing.".to_string(),
                ..Default::default()
            },
        }
    }
}

mod parse_helper {
    //! Helper module with the hard of the parser.
    #![allow(unused)]
    #![allow(missing_docs)]
    use chumsky::{chain::Chain, error::Cheap, prelude::*, text::whitespace};

    /// Alias for [`Range<usize>`] for easier type recognition
    type RopeSpan = std::ops::Range<usize>;

    use super::{Span, Token, TokenInfo};

    /// Parser for a class name with the start token.
    #[inline]
    fn class_parser(
    ) -> impl Parser<char, Vec<(Token, RopeSpan, Option<Vec<TokenInfo>>)>, Error = Cheap<char>>
    {
        let start = whitespace()
            .then(just('&').map_with_span(|_, span| (Token::Start, span, None)))
            .map(|(_, s)| s);
        let class = filter(char::is_ascii_alphabetic)
            .repeated()
            .collect()
            .map_with_span(|text: String, span| {
                let vec = if text.len() == 4 {
                    if text.chars().any(|c| c.is_lowercase()) {
                        Some(vec![TokenInfo::WarnClassSomeLowercase])
                    } else {
                        None
                    }
                } else {
                    Some(vec![if text.is_empty() {
                        TokenInfo::ErrorClassNameMissing
                    } else {
                        TokenInfo::WarnClassNameLength
                    }])
                };
                (Token::Class(text), span, vec)
            });
        start.chain(class)
    }

    /// Parse a number
    ///
    /// Supported:
    /// - `12345`
    /// - `-1234`
    /// - `12.45`
    /// - `1245.`
    /// - `12E45`
    /// - `12e45`
    /// - `12E-5`
    #[inline]
    fn number_parser(
    ) -> impl Parser<char, (Token, RopeSpan, Option<Vec<TokenInfo>>), Error = Cheap<char>> {
        just('-')
            .or_not()
            .chain::<char, _, _>(
                filter(char::is_ascii_digit)
                    .repeated()
                    .at_least(1)
                    .chain(
                        just('.')
                            .chain(filter(char::is_ascii_digit).repeated())
                            .or_not()
                            .flatten(),
                    )
                    .chain(
                        just('e')
                            .or(just('E'))
                            .chain(just('-').or_not())
                            .chain(filter(char::is_ascii_digit).repeated().at_least(1))
                            .or_not()
                            .flatten(),
                    ),
            )
            .map_with_span(|text: Vec<char>, span: RopeSpan| {
                let mut info = None;
                let text = text.into_iter().collect::<String>();
                if text.ends_with('.') {
                    info = Some(vec![TokenInfo::HintFloatNumber])
                }
                let number = text.parse::<f32>().unwrap();
                (Token::Number(number), span, info)
            })
    }

    /// Parse a boolean
    ///
    /// Supported:
    /// - `T`
    /// - `F`
    /// - `.TRUE.`
    /// - `.FALSE.`
    #[inline]
    fn boolean_parser(
    ) -> impl Parser<char, (Token, RopeSpan, Option<Vec<TokenInfo>>), Error = Cheap<char>> {
        just("T")
            .or(just("F"))
            .or(just(".TRUE."))
            .or(just(".FALSE."))
            .map_with_span(|text, span| {
                let mut info = None;
                let boolean = match text {
                    "T" => {
                        info = Some(vec![TokenInfo::HintUseVerboseBoolean]);
                        true
                    }
                    "F" => {
                        info = Some(vec![TokenInfo::HintUseVerboseBoolean]);
                        false
                    }
                    ".TRUE." => true,
                    _ => false,
                };

                (Token::Boolean(boolean), span, info)
            })
    }

    /// Parse a string
    ///
    /// Supported:
    /// - `""`
    /// - `''`
    #[inline]
    fn string_parser(
    ) -> impl Parser<char, (Token, RopeSpan, Option<Vec<TokenInfo>>), Error = Cheap<char>> {
        let single_quote = take_until(
            none_of('\\')
                .then(just('\'').ignored().rewind())
                .map(|(c, _)| c),
        )
        .map(|(mut s, c)| {
            s.push(c);
            s
        })
        .padded_by(just('\''))
        .collect::<String>()
        .map(|s| ('\'', s));

        let double_quote = take_until(
            none_of('\\')
                .then(just('"').ignored().rewind())
                .map(|(c, _)| c),
        )
        .map(|(mut s, c)| {
            s.push(c);
            s
        })
        .padded_by(just('"'))
        .collect::<String>()
        .map(|s| ('\"', s));

        just("''")
            .to('\'')
            .or(just("\"\"").to('"'))
            .map(|c| (c, "".to_string()))
            .or(single_quote)
            .or(double_quote)
            .map_with_span(|(c, text): (char, String), span: RopeSpan| {
                let mut info = None;
                if c == '\'' {
                    info = Some(vec![TokenInfo::HintUseDoubleQuotes])
                }
                (Token::String(text), span, info)
            })
    }

    /// Parse a variable
    ///
    /// Supported:
    /// - `#TEXT#`
    #[inline]
    fn variable_parser(
    ) -> impl Parser<char, (Token, RopeSpan, Option<Vec<TokenInfo>>), Error = Cheap<char>> {
        take_until(
            none_of('#')
                .then(just('#').ignored().rewind())
                .map(|(c, _)| c),
        )
        .map(|(mut s, c)| {
            s.push(c);
            s
        })
        .padded_by(just('#'))
        .collect::<String>()
        .map_with_span(|text, span: RopeSpan| (Token::Variable(text), span, None))
    }

    /// Parse the end, if it is an error.
    #[inline]
    fn end_error_parser(
        err: &'static TokenInfo,
    ) -> impl Parser<char, (Token, RopeSpan, Option<Vec<TokenInfo>>), Error = Cheap<char>> {
        take_until(
            just('/')
                .ignored()
                .rewind()
                .or(just('&').ignored().rewind())
                .or(end()),
        )
        .map_with_span(|_, span| (Token::Error, span, Some(vec![*err])))
    }

    /// Parse one value of a property
    ///
    /// Supported:
    /// - number
    /// - boolean
    /// - string
    #[inline]
    fn property_value_parser(
    ) -> impl Parser<char, (Token, RopeSpan, Option<Vec<TokenInfo>>), Error = Cheap<char>> {
        boolean_parser()
            .or(number_parser())
            .or(string_parser())
            .or(variable_parser())
    }

    /// Parse a property name
    ///
    /// Supported
    /// - `NAME`
    /// - `NAME(3)`
    /// - `NAME(3:3)`
    /// - `NAME(3,3)`
    //MAYBE Split Property content
    #[inline]
    fn property_name_parser(
    ) -> impl Parser<char, (Token, RopeSpan, Option<Vec<TokenInfo>>), Error = Cheap<char>> {
        let name = filter(char::is_ascii_digit)
            .repeated()
            .at_least(1)
            .chain(just(':').or(just(',')).or_not())
            .repeated()
            .at_least(1)
            .flatten();

        filter(char::is_ascii_alphabetic)
            .chain(filter(|c| char::is_ascii_alphanumeric(c) || *c == '_').repeated())
            .chain::<char, _, _>(just('(').chain(name).chain(just(')')).or_not().flatten())
            .collect::<String>()
            .map_with_span(|text: String, span: RopeSpan| {
                let info = if text.chars().any(|c| c.is_lowercase()) {
                    Some(vec![TokenInfo::WarnPropertySomeLowercase])
                } else {
                    None
                };
                (Token::Property(text), span, info)
            })
    }

    /// Parse a property with every assigned values.
    #[inline]
    fn property_assignment_parser(
    ) -> impl Parser<char, Vec<(Token, RopeSpan, Option<Vec<TokenInfo>>)>, Error = Cheap<char>>
    {
        // HACK If the input is like `NAME = T, F = 1` the last property is seen as a boolean
        let check_bool = none_of('=').padded().ignored().or(end().padded()).rewind();

        let comma = just(',')
            .ignored()
            .map_with_span(|_, span| (Token::Comma, span, None))
            .padded();

        let property_assignment = just('=')
            .ignored()
            .map_with_span(|_, span| (Token::Equal, span, None))
            .padded()
            .chain(
                property_value_parser().or(end_error_parser(&TokenInfo::ErrorUnknownPropertyValue)),
            )
            .chain(
                comma
                    .chain(
                        // HACK Currently the property_value_parser is executed 2x if the bool_check fails.
                        property_value_parser().then_ignore(check_bool),
                    )
                    .repeated()
                    .flatten(),
            )
            .chain(comma.or_not())
            .map(|mut vec: Vec<(Token, _, Option<Vec<TokenInfo>>)>| {
                if let Some((token, _, vec)) = vec.last_mut() {
                    if token != &Token::Comma {
                        if vec.is_some() {
                            vec.as_mut().unwrap().push(TokenInfo::HintComma)
                        } else {
                            *vec = Some(vec![TokenInfo::HintComma])
                        }
                    }
                }
                vec
            });

        property_name_parser().chain(property_assignment.or(empty().map_with_span(|_, span| {
            vec![(
                Token::Error,
                span,
                Some(vec![TokenInfo::ErrorPropertyAssignmentMissing]),
            )]
        })))
    }

    /// Parse the end of a command block.
    #[inline]
    fn end_parser(
    ) -> impl Parser<char, (Token, RopeSpan, Option<Vec<TokenInfo>>), Error = Cheap<char>> {
        filter(char::is_ascii_whitespace)
            .repeated()
            .ignored()
            .then(just('/').to(true).or(empty().to(false)))
            .map_with_span(|(_, b), span: std::ops::Range<usize>| {
                if b {
                    (Token::End, (span.end - 1)..span.end, None)
                } else {
                    (
                        Token::Error,
                        span.start..(span.start + 1),
                        Some(vec![TokenInfo::ErrorEndCharMissing]),
                    )
                }
            })
    }

    /// Parse a whole code block.
    #[inline]
    fn script_block_code_parser(
    ) -> impl Parser<char, Vec<(Token, RopeSpan, Option<Vec<TokenInfo>>)>, Error = Cheap<char>>
    {
        class_parser()
            .chain(property_assignment_parser().padded().repeated().flatten())
            .chain(end_parser())
    }

    /// Parse a comment.
    #[inline]
    fn script_block_comment_parser(
    ) -> impl Parser<char, Vec<(Token, RopeSpan, Option<Vec<TokenInfo>>)>, Error = Cheap<char>>
    {
        none_of('\n')
            .then(take_until(
                just::<_, _, _>('\n').rewind().ignored().or(end()),
            ))
            .ignored()
            .map_with_span(|_, span| vec![(Token::Comment, span, None)])
            .then_ignore(just('\n').or_not())
    }

    /// Parse the whole script.
    #[inline]
    pub(super) fn script_parser(
    ) -> impl Parser<char, Vec<(Token, RopeSpan, Option<Vec<TokenInfo>>)>, Error = Cheap<char>>
    {
        script_block_code_parser()
            .or(script_block_comment_parser().padded())
            .repeated()
            .at_least(1)
            .flatten()
    }

    #[cfg(test)]
    mod test {
        use std::{ops::Range, result};

        use chumsky::Parser;

        use crate::parser::{
            parse_helper::{boolean_parser, script_block_code_parser},
            Token, TokenInfo,
        };

        use super::{
            class_parser, number_parser, property_assignment_parser, property_name_parser,
            string_parser,
        };

        fn get_value<O, E>(result: Result<O, E>) -> O {
            match result {
                Ok(ok) => ok,
                Err(_) => panic!("An Ok value is expected!"),
            }
        }
        fn unzip(
            result: Vec<(Token, Range<usize>, Option<Vec<TokenInfo>>)>,
        ) -> (Vec<Token>, Vec<TokenInfo>) {
            let (t, s): (Vec<Token>, Vec<Option<Vec<TokenInfo>>>) =
                result.into_iter().map(|f| (f.0, f.2)).unzip();
            let s = s.into_iter().flatten().flatten().collect();
            (t, s)
        }

        #[test]
        fn test_class_parser() {
            let parser = class_parser();

            let result = get_value(parser.parse("&MATL"));
            let tokens = result.into_iter().map(|(t, _, _)| t).collect::<Vec<_>>();
            assert_eq!(vec![Token::Start, Token::Class("MATL".to_string())], tokens);

            let result = get_value(parser.parse("& MATL"));
            let suggestions = result
                .into_iter()
                .filter_map(|(_, _, s)| s)
                .flatten()
                .collect::<Vec<_>>();
            assert_eq!(vec![TokenInfo::ErrorClassNameMissing], suggestions);

            let result = get_value(parser.parse("&Material"));
            let suggestions = result
                .into_iter()
                .filter_map(|(_, _, s)| s)
                .flatten()
                .collect::<Vec<_>>();
            assert_eq!(vec![TokenInfo::WarnClassNameLength], suggestions);
        }

        #[test]
        fn test_number_parser() {
            let parser = number_parser();

            let result = get_value(parser.parse("-1.2345e10"));
            let number = result.0;
            assert_eq!(Token::Number(-1.2345e10), number);
        }

        #[test]
        fn test_boolean_parser() {
            let parser = boolean_parser();
            let result = get_value(parser.parse("T"));
            assert_eq!(Token::Boolean(true), result.0);
            assert_eq!(Some(vec![TokenInfo::HintUseVerboseBoolean]), result.2);

            let result = get_value(parser.parse(".TRUE."));
            assert_eq!(Token::Boolean(true), result.0);
            assert_eq!(None, result.2);

            let result = get_value(parser.parse("F"));
            assert_eq!(Token::Boolean(false), result.0);
            assert_eq!(Some(vec![TokenInfo::HintUseVerboseBoolean]), result.2);

            let result = get_value(parser.parse(".FALSE."));
            assert_eq!(Token::Boolean(false), result.0);
            assert_eq!(None, result.2);

            if parser.parse(".False.").is_ok() {
                panic!("An Err value is expected!")
            }
        }

        #[test]
        fn test_string_parser() {
            let parser = string_parser();

            let result = get_value(parser.parse("\"text\""));
            assert_eq!(Token::String("text".to_string()), result.0);
            assert_eq!(None, result.2);

            let result = get_value(parser.parse("'text'"));
            assert_eq!(Token::String("text".to_string()), result.0);
            assert_eq!(Some(vec![TokenInfo::HintUseDoubleQuotes]), result.2);
        }

        #[test]
        fn test_property_name_parser() {
            let parser = property_name_parser();

            let result = get_value(parser.parse("NAME"));
            assert_eq!(Token::Property("NAME".to_string()), result.0);

            let result = get_value(parser.parse("NAME(3)"));
            assert_eq!(Token::Property("NAME(3)".to_string()), result.0);

            let result = get_value(parser.parse("NAME(3:3)"));
            assert_eq!(Token::Property("NAME(3:3)".to_string()), result.0);

            let result = get_value(parser.parse("NAME(3,3)"));
            assert_eq!(Token::Property("NAME(3,3)".to_string()), result.0);
        }

        #[test]
        fn test_property_assignment_parser() {
            let parser = property_assignment_parser();

            let result = get_value(parser.parse("NAME = 1"));
            let (t, s) = unzip(result);
            assert_eq!(
                vec![
                    Token::Property("NAME".to_string()),
                    Token::Equal,
                    Token::Number(1.0)
                ],
                t
            );

            let result = get_value(parser.parse("NAME = 1,"));
            let (t, s) = unzip(result);
            assert_eq!(
                vec![
                    Token::Property("NAME".to_string()),
                    Token::Equal,
                    Token::Number(1.0),
                    Token::Comma
                ],
                t
            );

            let result = get_value(parser.parse("NAME = 1,2,3"));
            let (t, s) = unzip(result);
            assert_eq!(
                vec![
                    Token::Property("NAME".to_string()),
                    Token::Equal,
                    Token::Number(1.0),
                    Token::Comma,
                    Token::Number(2.0),
                    Token::Comma,
                    Token::Number(3.0),
                ],
                t
            );

            let result = get_value(parser.parse("NAME = 1,2,3,"));
            let (t, s) = unzip(result);
            assert_eq!(
                vec![
                    Token::Property("NAME".to_string()),
                    Token::Equal,
                    Token::Number(1.0),
                    Token::Comma,
                    Token::Number(2.0),
                    Token::Comma,
                    Token::Number(3.0),
                    Token::Comma,
                ],
                t
            );

            let result = get_value(parser.parse("NAME = 1 OTHER = 1"));
            let (t, s) = unzip(result);
            assert_eq!(
                vec![
                    Token::Property("NAME".to_string()),
                    Token::Equal,
                    Token::Number(1.0)
                ],
                t
            );

            let result = get_value(parser.parse("NAME = 1, OTHER = 1"));
            let (t, s) = unzip(result);
            assert_eq!(
                vec![
                    Token::Property("NAME".to_string()),
                    Token::Equal,
                    Token::Number(1.0),
                    Token::Comma,
                ],
                t
            );

            let result = get_value(parser.parse("NAME = 1,2,3 OTHER = 1"));
            let (t, s) = unzip(result);
            assert_eq!(
                vec![
                    Token::Property("NAME".to_string()),
                    Token::Equal,
                    Token::Number(1.0),
                    Token::Comma,
                    Token::Number(2.0),
                    Token::Comma,
                    Token::Number(3.0),
                ],
                t
            );

            let result = get_value(parser.parse("NAME = 1,2,3, OTHER = 1"));
            let (t, s) = unzip(result);
            assert_eq!(
                vec![
                    Token::Property("NAME".to_string()),
                    Token::Equal,
                    Token::Number(1.0),
                    Token::Comma,
                    Token::Number(2.0),
                    Token::Comma,
                    Token::Number(3.0),
                    Token::Comma,
                ],
                t
            );

            let result = get_value(parser.parse("NAME = F"));
            let (t, s) = unzip(result);
            assert_eq!(
                vec![
                    Token::Property("NAME".to_string()),
                    Token::Equal,
                    Token::Boolean(false)
                ],
                t
            );

            let result = get_value(parser.parse("NAME = F,"));
            let (t, s) = unzip(result);
            assert_eq!(
                vec![
                    Token::Property("NAME".to_string()),
                    Token::Equal,
                    Token::Boolean(false),
                    Token::Comma
                ],
                t
            );
            let result = get_value(parser.parse("NAME = F, F = 1.0"));
            let (t, s) = unzip(result);
            assert_eq!(
                vec![
                    Token::Property("NAME".to_string()),
                    Token::Equal,
                    Token::Boolean(false),
                    Token::Comma
                ],
                t
            );

            let result = get_value(parser.parse("NAME = F F = 1.0"));
            let (t, s) = unzip(result);
            assert_eq!(
                vec![
                    Token::Property("NAME".to_string()),
                    Token::Equal,
                    Token::Boolean(false)
                ],
                t
            );

            let result = get_value(parser.parse("NAME = Unknown"));
            let (t, s) = unzip(result);
            assert_eq!(vec![TokenInfo::ErrorUnknownPropertyValue], s);

            let result = get_value(parser.parse("NAME = "));
            let (t, s) = unzip(result);
            assert_eq!(vec![TokenInfo::ErrorUnknownPropertyValue], s);

            let result = get_value(parser.parse("NAME = ,"));
            let (t, s) = unzip(result);
            assert_eq!(vec![TokenInfo::ErrorUnknownPropertyValue], s);
        }

        #[test]
        fn test_script_block_code_parser() {
            let parser = script_block_code_parser();

            let result = get_value(parser.parse("&NAME PROP = 1, PROP = 2, /"));
            let (t, s) = unzip(result);
            assert_eq!(
                vec![
                    Token::Start,
                    Token::Class("NAME".to_string()),
                    Token::Property("PROP".to_string()),
                    Token::Equal,
                    Token::Number(1.0),
                    Token::Comma,
                    Token::Property("PROP".to_string()),
                    Token::Equal,
                    Token::Number(2.0),
                    Token::Comma,
                    Token::End
                ],
                t
            );
            assert_eq!(Vec::<TokenInfo>::new(), s);

            let result = get_value(parser.parse("&NAME PROP = 1, PROP = 2,"));
            let (t, s) = unzip(result);
            assert_eq!(
                vec![
                    Token::Start,
                    Token::Class("NAME".to_string()),
                    Token::Property("PROP".to_string()),
                    Token::Equal,
                    Token::Number(1.0),
                    Token::Comma,
                    Token::Property("PROP".to_string()),
                    Token::Equal,
                    Token::Number(2.0),
                    Token::Comma,
                    Token::Error
                ],
                t
            );
            assert_eq!(vec![TokenInfo::ErrorEndCharMissing], s);

            let result = get_value(parser.parse("&NAME PROP = T,F, F = 1.0"));
            let (t, s) = unzip(result);
            assert_eq!(
                vec![
                    Token::Start,
                    Token::Class("NAME".to_string()),
                    Token::Property("PROP".to_string()),
                    Token::Equal,
                    Token::Boolean(true),
                    Token::Comma,
                    Token::Boolean(false),
                    Token::Comma,
                    Token::Property("F".to_string()),
                    Token::Equal,
                    Token::Number(1.0),
                    Token::Error
                ],
                t
            );
            assert_eq!(
                vec![
                    TokenInfo::HintUseVerboseBoolean,
                    TokenInfo::HintUseVerboseBoolean,
                    TokenInfo::ErrorEndCharMissing
                ],
                s
            );
        }
    }
}
