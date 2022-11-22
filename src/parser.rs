use anyhow::Result;
use chumsky::{error::Cheap, prelude::*, Error, Stream};
use ropey::Rope;
use std::ops::Range;
use tower_lsp::lsp_types::{Diagnostic, Position};

use self::parse_helper::script_parser;

#[derive(Debug, PartialEq, Clone)]
enum ScriptBlock<E: Error<char>> {
    Comment,
    Code(Vec<(Result<Token, TokenError>, E::Span)>),
}
impl ScriptBlock<Cheap<char>> {
    fn convert_to_block(self, rope: &Rope) -> Block {
        match self {
            ScriptBlock::Comment => Block::Comment,
            ScriptBlock::Code(code) => Block::Code(
                code.into_iter()
                    .map(|(script_block, span)| (script_block, convert_span(span, rope)))
                    .collect::<Vec<_>>(),
            ),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Block {
    Comment,
    Code(Vec<(Result<Token, TokenError>, tower_lsp::lsp_types::Range)>),
    ParseError(Option<String>),
}
impl Block {
    pub fn _get_token(
        &self,
        pos: Position,
    ) -> Option<(Result<Token, TokenError>, tower_lsp::lsp_types::Range)> {
        match self {
            Block::Comment => None,
            Block::Code(code) => {
                for (token, range) in code.iter() {
                    if range.start <= pos && range.end >= pos {
                        return Some((token.clone(), range.clone()));
                    }
                }
                None
            }
            Block::ParseError(_) => None,
        }
    }

    pub fn get_name(&self) -> Option<String> {
        if let Block::Code(code) = self {
            if code.len() >= 2 {
                let (token, _) = &code[1];
                if let Ok(Token::Class(name)) = token {
                    return Some(name.clone());
                }
            }
        }
        None
    }

    pub fn get_recent_property(
        &self,
        pos: Position,
    ) -> Option<(String, tower_lsp::lsp_types::Range)> {
        let Block::Code( code) =self else { return None};
        code.iter()
            .filter_map(|(token, span)| match token {
                Ok(Token::Property(property)) => Some((property, span)),
                _ => None,
            })
            .rev()
            .find_map(|(property, range)| {
                if range.start <= pos {
                    Some((property.clone(), range.clone()))
                } else {
                    None
                }
            })
    }
}

#[derive(Debug)]
pub struct Script(Vec<(Block, tower_lsp::lsp_types::Range)>);
impl Script {
    pub fn new(rope: &Rope) -> Self {
        let content = match parse(rope.to_string()) {
            Ok(ok) => ok
                .into_iter()
                .map(|(script_block, span)| {
                    (
                        script_block.convert_to_block(rope),
                        convert_span(span, rope),
                    )
                })
                .collect::<Vec<_>>(),
            Err(err) => err
                .into_iter()
                .map(|f| {
                    (
                        Block::ParseError(match f.label() {
                            Some(some) => Some(some.to_string()),
                            None => None,
                        }),
                        { convert_span(f.span(), rope) },
                    )
                })
                .collect::<Vec<_>>(),
        };

        Self(content)
    }

    pub fn get_block(&self, pos: Position) -> Option<Block> {
        for (block, range) in self.0.iter() {
            if range.start <= pos && range.end >= pos {
                return Some(block.clone());
            }
        }
        None
    }

    pub fn iter(&self) -> std::slice::Iter<(Block, tower_lsp::lsp_types::Range)> {
        self.0.iter()
    }
}
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Start,
    Class(String),
    Property(String),
    Number(f32),
    Boolean(bool),
    String(String),
    Comma,
    Equal,
    End,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenError {
    Class,
    Property,
    PropertyAssignment,
    PropertyValue,
    End,
}
impl TokenError {
    pub fn to_diagnostic(&self, range: tower_lsp::lsp_types::Range) -> Diagnostic {
        let message = match self {
            TokenError::Class => "Immediately after a '&' a class is expected.",
            TokenError::Property => "A property is expected.",
            TokenError::PropertyAssignment => "The property was assigned wrong.",
            TokenError::PropertyValue => "Unknown value type",
            TokenError::End => "An end char could not be found. Did you forgot a '/'?",
        }
        .to_string();
        let diagnostic = Diagnostic {
            range,
            message,
            ..Default::default()
        };
        diagnostic
    }
}

fn parse<'a, Iter, S>(
    stream: S,
) -> Result<Vec<(ScriptBlock<Cheap<char>>, Range<usize>)>, Vec<Cheap<char>>>
where
    Iter: Iterator<Item = (char, Range<usize>)> + 'a,
    S: Into<Stream<'a, char, Range<usize>, Iter>>,
{
    script_parser::<Cheap<char>>().parse(stream)
}

fn convert_position(pos: usize, rope: &Rope) -> Position {
    let l = rope.char_to_line(pos);
    let c = pos - rope.line_to_char(l);

    Position::new(l as u32, c as u32)
}

fn convert_span(span: Range<usize>, rope: &Rope) -> tower_lsp::lsp_types::Range {
    tower_lsp::lsp_types::Range::new(
        convert_position(span.start, rope),
        convert_position(span.end, rope),
    )
}

mod parse_helper {
    #![allow(private_in_public)]
    use super::{ScriptBlock, Token, TokenError};
    use chumsky::{prelude::*, text::whitespace, Error};

    #[inline]
    fn class_parser<E: Error<char>>() -> impl Parser<char, String, Error = E> {
        filter(char::is_ascii_alphabetic).repeated().collect()
    }

    #[inline]
    fn number_parser<E: Error<char>>() -> impl Parser<char, String, Error = E> {
        just('-')
            .or_not()
            .chain::<char, _, _>(
                filter(char::is_ascii_digit).repeated().at_least(1).chain(
                    just('.')
                        .chain(filter(char::is_ascii_digit).repeated().at_least(1))
                        .or_not()
                        .flatten(),
                ),
            )
            .collect()
    }

    #[inline]
    fn bool_parser<E: Error<char>>() -> impl Parser<char, String, Error = E> {
        just("T")
            .or(just("F"))
            .or(just(".TRUE."))
            .or(just(".FALSE."))
            .map(|s| s.to_string())
    }

    #[inline]
    fn string_parser<E: Error<char>>() -> impl Parser<char, String, Error = E> {
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
        .collect::<String>();

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
        .collect::<String>();

        just("''")
            .or(just("\"\""))
            .to("".to_string())
            .or(single_quote)
            .or(double_quote)
    }

    //MAYBE Split Property content
    #[inline]
    fn property_parser<E: Error<char>>() -> impl Parser<char, String, Error = E> {
        let content = filter(char::is_ascii_digit)
            .repeated()
            .at_least(1)
            .chain(just(':').or(just(',')).or_not())
            .repeated()
            .at_least(1)
            .flatten();

        filter(char::is_ascii_alphabetic)
            .chain(filter(|c| char::is_ascii_alphanumeric(c) || *c == '_').repeated())
            .chain::<char, _, _>(just('(').chain(content).chain(just(')')).or_not().flatten())
            .collect()
    }

    #[inline]
    fn property_value_parser<E: Error<char>>(
    ) -> impl Parser<char, Vec<(Result<Token, TokenError>, E::Span)>, Error = E> {
        let number_parser = number_parser::<E>()
            .map_with_span(|s, span| vec![(Ok(Token::Number(s.parse().unwrap())), span)]);
        let boolean_parser = bool_parser::<E>().map_with_span(|s, span| {
            vec![(
                Ok(Token::Boolean(match s.as_str() {
                    "T" => true,
                    "F" => false,
                    ".TRUE." => true,
                    ".FALSE." => false,
                    _ => false,
                })),
                span,
            )]
        });
        let string_parser =
            string_parser::<E>().map_with_span(|s, span| vec![(Ok(Token::String(s)), span)]);

        boolean_parser
            .or(number_parser)
            .or(string_parser)
            .or(end_error_parser(&TokenError::PropertyValue).map(|v| vec![v]))
    }

    #[inline]
    fn property_assignment_parser<E: Error<char>>(
    ) -> impl Parser<char, Vec<(Result<Token, TokenError>, E::Span)>, Error = E> {
        let property_parser = property_parser::<E>()
            .map_with_span(|s, span| (Ok(Token::Property(s.parse().unwrap())), span));

        let property_assignment = just('=')
            .ignored()
            .padded()
            .map_with_span(|_, span| (Ok(Token::Equal), span))
            .chain(
                property_value_parser::<E>()
                    .chain(
                        just(',')
                            .padded()
                            .ignored()
                            .map_with_span(|_, span| (Ok(Token::Comma), span))
                            .chain(
                                just('T')
                                    .or(just('F'))
                                    .or(just('.'))
                                    .or(just('-'))
                                    .or(filter(char::is_ascii_digit))
                                    .or(just('\'').or(just('"')))
                                    .rewind()
                                    .to(None),
                            )
                            .chain(property_value_parser::<E>())
                            .repeated()
                            .flatten(),
                    )
                    .chain(
                        whitespace()
                            .then(
                                just(',')
                                    .map_with_span(|_, span| (Ok(Token::Comma), span))
                                    .or_not(),
                            )
                            .map(|(_, v)| v),
                    ),
            )
            .or(end_error_parser(&TokenError::PropertyAssignment).map(|v| vec![v]));

        property_parser.chain(property_assignment)
    }

    #[inline]
    fn end_parser<E: Error<char>>(
    ) -> impl Parser<char, (Result<Token, TokenError>, E::Span), Error = E> {
        whitespace()
            .then(just('/').map_with_span(|_, span| (Ok(Token::End), span)))
            .map(|(_, v)| v)
            .or(take_until(just('&').ignored().rewind().or(end()))
                .map_with_span(|_, span| (Err(TokenError::End), span)))
    }

    #[inline]
    fn end_error_parser<E: Error<char>>(
        err: &'static TokenError,
    ) -> impl Parser<char, (Result<Token, TokenError>, E::Span), Error = E> {
        take_until(
            just('/')
                .ignored()
                .rewind()
                .or(just('&').ignored().rewind())
                .or(end()),
        )
        .map_with_span(|_, span| (Err(*err), span))
    }

    #[inline]
    fn script_block_code_parser<E: Error<char> + Clone>(
    ) -> impl Parser<char, (ScriptBlock<E>, E::Span), Error = E> {
        let code_start_parser = whitespace()
            .then(just('&').map_with_span(|_, span| (Ok(Token::Start), span)))
            .map(|(_, v)| v);

        let property_parser = property_assignment_parser::<E>();

        let class_parser =
            class_parser::<E>().map_with_span(|s, span| vec![(Ok(Token::Class(s)), span)]);

        code_start_parser
            .chain(
                class_parser
                    .chain(
                        property_parser
                            .padded()
                            .repeated()
                            .flatten()
                            .or(end_error_parser(&TokenError::Property).map(|v| vec![v])),
                    )
                    .or(end_error_parser(&TokenError::Class).map(|v| vec![v])),
            )
            .chain(end_parser())
            .map_with_span(|v, span| (ScriptBlock::<E>::Code(v), span))
    }

    #[inline]
    fn script_block_comment_parser<E: Error<char> + Clone>(
    ) -> impl Parser<char, (ScriptBlock<E>, E::Span), Error = E> {
        none_of('\n')
            .then(take_until(
                just::<_, _, E>('\n').rewind().ignored().or(end()),
            ))
            .ignored()
            .map_with_span(|_, span| (ScriptBlock::Comment, span))
            .then(just('\n').or_not())
            .map(|(v, _)| v)
    }

    #[inline]
    pub(super) fn script_parser<E: Error<char> + Clone>(
    ) -> impl Parser<char, Vec<(ScriptBlock<E>, E::Span)>, Error = E> {
        script_block_code_parser()
            .or(script_block_comment_parser().padded())
            .repeated()
            .at_least(1)
    }

    #[test]
    fn test_class_parser() {
        let parser = class_parser::<Simple<char>>();
        assert_eq!(parser.parse("TEST"), Ok("TEST".to_string()),);
    }

    #[test]
    fn test_number_parser() {
        let parser = number_parser::<Simple<char>>();
        assert_eq!(
            parser.parse("10"),
            Ok("10".to_string()),
            "Tested '10', expected '10'"
        );
        assert_eq!(
            parser.parse("-10"),
            Ok("-10".to_string()),
            "Tested '-10', expected '-10'"
        );
        assert_eq!(
            parser.parse("-10.2345"),
            Ok("-10.2345".to_string()),
            "Tested '-10.2345', expected '-10.2345'"
        );
        assert!(
            parser.parse("- 10.2345").is_err(),
            "Tested '- 10.2345', expected Error"
        );
    }

    #[test]
    fn test_boolean_parser() {
        let parser = bool_parser::<Simple<char>>();
        assert_eq!(
            parser.parse("T"),
            Ok("T".to_string()),
            "Tested 'T', expected 'T'"
        );
        assert_eq!(
            parser.parse("F"),
            Ok("F".to_string()),
            "Tested 'F', expected 'F'"
        );
        assert_eq!(
            parser.parse(".TRUE."),
            Ok(".TRUE.".to_string()),
            "Tested  '.TRUE.', expected '.TRUE.'"
        );
        assert_eq!(
            parser.parse(".FALSE."),
            Ok(".FALSE.".to_string()),
            "Tested ' .FALSE.', expected '.FALSE'"
        );
        assert!(
            parser.parse(".True.").is_err(),
            "Tested '.True.' expected Error, got {:?}",
            parser.parse(".True.")
        )
    }

    #[test]
    fn test_string_parser() {
        let parser = string_parser::<Simple<char>>();
        assert_eq!(
            parser.parse("''"),
            Ok("".to_string()),
            "Tested '''', expected ''"
        );
        assert_eq!(
            parser.parse("\"\""),
            Ok("".to_string()),
            "Tested '\"\"', expected ''"
        );
        assert_eq!(
            parser.parse("'some text'"),
            Ok("some text".to_string()),
            "Tested ''some text'', expected 'some text'"
        );
        assert_eq!(
            parser.parse("\"some text\""),
            Ok("some text".to_string()),
            "Tested '\"some text\"', expected 'some text'"
        );
        //FIXME Fore some reason \' gets converted to \\' so the test fails:
        //  left: `Ok("some \\'quote\\' text")`,
        // right: `Ok("some 'quote' text")`
        // assert_eq!(
        //     parser.parse(r"'some \'quote\' text'"),
        //     Ok("some 'quote' text".to_string()),
        //     "Tested  ''some \'quote\' text'', expected 'some 'quote' text'"
        // );
    }

    #[test]
    fn test_property_parser() {
        let parser = property_parser::<Simple<char>>();
        assert_eq!(
            parser.parse("TEST_PROPERTY"),
            Ok("TEST_PROPERTY".to_string()),
            "Tested 'TEST_PROPERTY', expected 'TEST_PROPERTY'"
        );
        assert_eq!(
            parser.parse("TEST_PROPERTY(1)"),
            Ok("TEST_PROPERTY(1)".to_string()),
            "Tested '\"\"', expected ''"
        );
        assert_eq!(
            parser.parse("TEST_PROPERTY(1:6,1)"),
            Ok("TEST_PROPERTY(1:6,1)".to_string()),
            "Tested 'TEST_PROPERTY(1:6,1)', expected 'TEST_PROPERTY(1:6,1)'"
        );
        assert_eq!(
            parser.parse("TEST_PROPERTY(1:2)"),
            Ok("TEST_PROPERTY(1:2)".to_string()),
            "Tested 'TEST_PROPERTY(1:2)', expected 'TEST_PROPERTY(1:2)'"
        );
        assert_eq!(
            parser.parse("TEST_PROPERTY(1,1)"),
            Ok("TEST_PROPERTY(1,1)".to_string()),
            "Tested  'TEST_PROPERTY(1,1)', expected 'TEST_PROPERTY(1,1)'"
        );
    }

    #[test]
    fn test_end_parser() {
        let parser = end_parser::<Simple<char>>();
        assert_eq!(
            parser.parse("   / Some Comment"),
            Ok((Ok(Token::End), 3..4))
        );
        assert_eq!(
            parser.parse("Some Text that could not be parsed / Some Comment"),
            Ok((Err(TokenError::End), 0..49))
        );
    }
    #[test]

    fn test_end_error_parser() {
        let parser = end_error_parser::<Simple<char>>(&TokenError::End);
        assert_eq!(
            parser.parse("Some Text that could not be parsed / Some Comment"),
            Ok((Err(TokenError::End), 0..35))
        );
    }

    #[test]
    fn test_script_comment_parser() {
        let parser = script_block_comment_parser::<Simple<char>>();
        assert_eq!(
            parser.parse("Some Comment\nMore Comment"),
            Ok((ScriptBlock::Comment, 0..12))
        );
        assert_eq!(
            parser.repeated().parse("Some Comment\nMore Comment"),
            Ok(vec![
                (ScriptBlock::Comment, 0..12),
                (ScriptBlock::Comment, 13..25)
            ])
        );
    }

    #[test]
    fn test_property_value_parser() {
        let parser = property_value_parser::<Simple<char>>();

        let parsed = parser.parse("T");
        assert_eq!(parsed, Ok(vec![(Ok(Token::Boolean(true)), 0..1)]));
        let parsed = parser.parse("'string'");
        assert_eq!(
            parsed,
            Ok(vec![(Ok(Token::String("string".to_string())), 0..8)])
        );
        let parsed = parser.parse("-100");
        assert_eq!(parsed, Ok(vec![(Ok(Token::Number(-100.0)), 0..4)]));
        let parsed = parser.parse("fail");
        assert_eq!(parsed, Ok(vec![(Err(TokenError::PropertyValue), 0..4),]));
    }

    #[test]
    fn test_property_assignment_parser() {
        let parser = property_assignment_parser::<Simple<char>>();

        let parsed = parser.parse("PROP=1");
        assert_eq!(
            parsed,
            Ok(vec![
                (Ok(Token::Property("PROP".to_string())), 0..4),
                (Ok(Token::Equal), 4..5),
                (Ok(Token::Number(1.0)), 5..6)
            ])
        );
        let parsed = parser.parse("PROP=1,2");
        assert_eq!(
            parsed,
            Ok(vec![
                (Ok(Token::Property("PROP".to_string())), 0..4),
                (Ok(Token::Equal), 4..5),
                (Ok(Token::Number(1.0)), 5..6),
                (Ok(Token::Comma), 6..7),
                (Ok(Token::Number(2.0)), 7..8)
            ])
        );
        let parsed = parser.parse("PROP=1,2 PROP=1,2");
        assert_eq!(
            parsed,
            Ok(vec![
                (Ok(Token::Property("PROP".to_string())), 0..4),
                (Ok(Token::Equal), 4..5),
                (Ok(Token::Number(1.0)), 5..6),
                (Ok(Token::Comma), 6..7),
                (Ok(Token::Number(2.0)), 7..8)
            ])
        );
        let parsed = parser.parse("PROP=1,2,");
        assert_eq!(
            parsed,
            Ok(vec![
                (Ok(Token::Property("PROP".to_string())), 0..4),
                (Ok(Token::Equal), 4..5),
                (Ok(Token::Number(1.0)), 5..6),
                (Ok(Token::Comma), 6..7),
                (Ok(Token::Number(2.0)), 7..8),
                (Ok(Token::Comma), 8..9)
            ])
        );
        let parsed = parser.parse("PROP=1,2 , PROP=1,2");
        assert_eq!(
            parsed,
            Ok(vec![
                (Ok(Token::Property("PROP".to_string())), 0..4),
                (Ok(Token::Equal), 4..5),
                (Ok(Token::Number(1.0)), 5..6),
                (Ok(Token::Comma), 6..7),
                (Ok(Token::Number(2.0)), 7..8),
                (Ok(Token::Comma), 9..10)
            ])
        );
        let parsed = parser.parse("PROP='string");
        assert_eq!(
            parsed,
            Ok(vec![
                (Ok(Token::Property("PROP".to_string())), 0..4),
                (Ok(Token::Equal), 4..5),
                (Err(TokenError::PropertyValue), 5..12),
            ])
        );
    }

    #[test]
    fn test_script_block_code_parser() {
        let parsed = script_block_code_parser::<Simple<char>>().parse("&CLASS PROPERTY=.TRUE. /");
        assert_eq!(
            parsed,
            Ok((
                ScriptBlock::Code(vec![
                    (Ok(Token::Start), 0..1),
                    (Ok(Token::Class("CLASS".to_string())), 1..6),
                    (Ok(Token::Property("PROPERTY".to_string())), 7..15),
                    (Ok(Token::Equal), 15..16),
                    (Ok(Token::Boolean(true)), 16..22),
                    (Ok(Token::End), 23..24)
                ]),
                0..24
            ))
        );
        let parsed = script_block_code_parser::<Simple<char>>().parse("&CLASSPROPERTY=.TRUE./");
        assert_eq!(
            parsed,
            Ok((
                ScriptBlock::Code(vec![
                    (Ok(Token::Start), 0..1),
                    (Ok(Token::Class("CLASSPROPERTY".to_string())), 1..14),
                    (Err(TokenError::Property), 14..21),
                    (Ok(Token::End), 21..22)
                ]),
                0..22
            ))
        );
        let parsed =
            script_block_code_parser::<Simple<char>>().parse("  &CLASS  PROPERTY = 1 , 2 , 3/");
        assert_eq!(
            parsed,
            Ok((
                ScriptBlock::Code(vec![
                    (Ok(Token::Start), 0..3),
                    (Ok(Token::Class("CLASS".to_string())), 3..8),
                    (Ok(Token::Property("PROPERTY".to_string())), 10..18),
                    (Ok(Token::Equal), 18..21),
                    (Ok(Token::Number(1.0)), 21..22),
                    (Ok(Token::Comma), 22..25),
                    (Ok(Token::Number(2.0)), 25..26),
                    (Ok(Token::Comma), 26..29),
                    (Ok(Token::Number(3.0)), 29..30),
                    (Ok(Token::End), 30..31)
                ]),
                0..31
            ))
        );
    }

    #[test]
    fn test_script_parser() {
        let parser = script_parser::<Simple<char>>();
        let parsed = parser.parse("Comment\nComment");
        assert_eq!(
            parsed,
            Ok(vec![
                (ScriptBlock::Comment, 0..7),
                (ScriptBlock::Comment, 8..15)
            ])
        );
        let parsed = parser.parse(
            "Comment\n&CLASS PROPERTY=T/ Comment\n&CLASS PROPERTY=T/\nComment\n&CLASS PROPERTY=T/\nComment",
        );
        assert_eq!(
            parsed,
            Ok(vec![
                (ScriptBlock::Comment, 0..7),
                (
                    ScriptBlock::Code(vec![
                        (Ok(Token::Start), 8..9),
                        (Ok(Token::Class("CLASS".to_string())), 9..14),
                        (Ok(Token::Property("PROPERTY".to_string())), 15..23),
                        (Ok(Token::Equal), 23..24),
                        (Ok(Token::Boolean(true)), 24..25),
                        (Ok(Token::End), 25..26)
                    ]),
                    8..26
                ),
                (ScriptBlock::Comment, 27..34),
                (
                    ScriptBlock::Code(vec![
                        (Ok(Token::Start), 35..36),
                        (Ok(Token::Class("CLASS".to_string())), 36..41),
                        (Ok(Token::Property("PROPERTY".to_string())), 42..50),
                        (Ok(Token::Equal), 50..51),
                        (Ok(Token::Boolean(true)), 51..52),
                        (Ok(Token::End), 52..53)
                    ]),
                    35..53
                ),
                (ScriptBlock::Comment, 54..61),
                (
                    ScriptBlock::Code(vec![
                        (Ok(Token::Start), 62..63),
                        (Ok(Token::Class("CLASS".to_string())), 63..68),
                        (Ok(Token::Property("PROPERTY".to_string())), 69..77),
                        (Ok(Token::Equal), 77..78),
                        (Ok(Token::Boolean(true)), 78..79),
                        (Ok(Token::End), 79..80)
                    ]),
                    62..80
                ),
                (ScriptBlock::Comment, 81..88)
            ])
        );
    }
}
