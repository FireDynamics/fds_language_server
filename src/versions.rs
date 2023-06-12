//! Submodule for construction and parsing [`Version`]s

use std::{env, fmt::Display, num::ParseIntError, path::PathBuf, str::FromStr};

use chumsky::{
    prelude::*,
    text::{digits, whitespace},
    Error,
};
use serde_json::Value;

/// Definiens the Version that should be used when not version vas set by the document.
const FALLBACK_VERSION: Version = Version {
    major: 6,
    minor: None,
    micro: None,
};

/// The environment Variable that was passed to get the path to the save location of the data.
const DATA_PATH_KEY: &str = "RUST_FDS_DATA_PATH";

/// Error that occurs when the path for the files needs to be determined based on the [Version].
#[derive(Debug, PartialEq, Eq)]
pub enum VersionPathError {
    /// The environment variable is not found
    MissingEnvironmentVariable,
    /// The environment variable has an incorrect format
    EnvironmentVariableFormatting(String),
    /// The version folder is not found, even the fallback version.
    MissingDataDirectory(Vec<Version>),
}

impl Display for VersionPathError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VersionPathError::MissingEnvironmentVariable => write!(
                f,
                "environment variable '{DATA_PATH_KEY}' was not passed in",
            ),
            VersionPathError::EnvironmentVariableFormatting(s) => write!(
                f,
                "environment variable '{DATA_PATH_KEY}' was assigned in a wrong format '{s}'",
            ),
            VersionPathError::MissingDataDirectory(p) => {
                let data_path = env::args().find(|f| f.starts_with(DATA_PATH_KEY)).unwrap();
                let data_path = data_path.split('=').last().unwrap();

                let p = p
                    .iter()
                    .map(|f| PathBuf::from(data_path).join(f.to_string()))
                    .collect::<Vec<PathBuf>>();

                write!(f, "a directory for the fds data was not found at {:?}", p)
            }
        }
    }
}
impl std::error::Error for VersionPathError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

/// Error that occurs when parsing a [`Version`] from text.
#[derive(Debug)]
pub enum VersionParseError {
    /// No major version number could be found.
    NoMajorVersion,
    /// The version is constructed of to many numbers.
    ToManyNumbers,
    /// Major number could not be converted to int
    ParseMajor(ParseIntError),
    /// Minor number could not be converted to int
    ParseMinor(ParseIntError),
    /// Micro number could not be converted to int
    ParseMicro(ParseIntError),
}
impl Display for VersionParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VersionParseError::NoMajorVersion => write!(f, "no major version number found"),
            VersionParseError::ToManyNumbers => write!(f, "version consist of to many numbers"),
            VersionParseError::ParseMajor(_) => write!(f, "unable to parse major number"),
            VersionParseError::ParseMinor(_) => write!(f, "unable to parse minor number"),
            VersionParseError::ParseMicro(_) => write!(f, "unable to parse micro number"),
        }
    }
}
impl std::error::Error for VersionParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            VersionParseError::ParseMajor(err)
            | VersionParseError::ParseMinor(err)
            | VersionParseError::ParseMicro(err) => Some(err),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
/// An enumeration representing the possible levels of detail for a version number.
enum DetailLevel {
    Major = 0,
    Minor = 1,
    Micro = 2,
}

/// Error that occurs when a [`Value`] could not be converted to a [`Version`]
#[derive(Debug)]
pub enum VersionValueError {
    /// The root value is not an Array
    NotAnArray,
    /// The first argument could not be converted to a major number
    NoMajor,
    /// The second argument could not be converted to a minor number
    NoMinor,
    /// The third argument could not be converted to a micro number
    NoMicro,
    /// The root value array contains to many values
    ToManyValues,
}
impl Display for VersionValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VersionValueError::NotAnArray => write!(f, "the passed value is not an array"),
            VersionValueError::NoMajor => {
                write!(f, "the first array value is missing or not a number")
            }
            VersionValueError::NoMinor => write!(f, "the second array value is not a number"),
            VersionValueError::NoMicro => write!(f, "the third array value is not a number"),
            VersionValueError::ToManyValues => write!(f, "the array has to many values"),
        }
    }
}
impl std::error::Error for VersionValueError {}

/// The data model of a version. With a mandatory major number and optional minor and micro number.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Version {
    /// The mandatory major number.
    major: u8,
    /// The optional minor number.
    minor: Option<u8>,
    /// The optional micro number.
    micro: Option<u8>,
}
impl Version {
    /// Determines if a given `stream` string starts with the same characters as the current instance of the `Self` type, which is assumed to represent a version number.
    // fn stream_is_version(&self, stream: String) -> bool {
    //     stream.starts_with(self.to_string().as_str())
    // }

    /// Determines the level of detail of the version represented by the `Self` type.
    fn get_detail_level(&self) -> DetailLevel {
        if self.minor.is_some() {
            if self.micro.is_some() {
                return DetailLevel::Micro;
            }
            return DetailLevel::Minor;
        }
        DetailLevel::Major
    }

    /// Returns a [`Vec<Version>`] based on the detail level of the current version.
    ///
    /// # Return
    /// * If detail level is Major, returns only the Major version
    /// * If detail level is Minor, returns Minor and Major version
    /// * If detail level is Micro, returns Micro, Minor and Major version
    fn get_possible_versions(&self) -> Vec<Version> {
        let detail = self.get_detail_level();
        let mut vec = Vec::with_capacity(detail as usize + 1);
        if detail == DetailLevel::Micro {
            vec.push(*self);
        }
        if detail >= DetailLevel::Minor {
            vec.push(Version {
                major: self.major,
                minor: self.minor,
                micro: None,
            });
        }
        vec.push(Version {
            major: self.major,
            minor: None,
            micro: None,
        });
        vec
    }

    /// Returns the used version and folder path based on the current version.
    ///
    /// # Return
    /// * Returns a tuple of `(Version, PathBuf)` if the version folder is found
    ///
    /// # Errors
    /// * Returns a `VersionPathError` with a value of `MissingEnvironmentVariable` if the environment variable is not found
    /// * Returns a `VersionPathError` with a value of `EnvironmentVariableFormatting` if the environment variable has an incorrect format
    /// * Returns a `VersionPathError` with a value of `MissingDataDirectory` if the version folder is not found, even the fallback version.
    pub fn get_used_version_and_dir_path(&self) -> Result<(Version, PathBuf), VersionPathError> {
        self.get_used_version_and_dir_path_custom_args(&mut std::env::args())
    }

    /// Intern function for `get_used_version_and_dir_path`.
    fn get_used_version_and_dir_path_custom_args(
        &self,
        args: &mut impl Iterator<Item = String>,
    ) -> Result<(Version, PathBuf), VersionPathError> {
        let Some(data_path) = args.find(|f| f.starts_with(DATA_PATH_KEY)) else {return Err(VersionPathError::MissingEnvironmentVariable)};
        let Some(data_path) = data_path.split('=').last() else {
            return Err(VersionPathError::EnvironmentVariableFormatting(data_path))
        };

        let mut versions = self.get_possible_versions();
        versions.push(FALLBACK_VERSION);
        for version in versions {
            let path = PathBuf::from(data_path).join(version.to_string());

            if path.exists() {
                return Ok((version, path));
            }
        }

        let mut versions = self.get_possible_versions();
        versions.push(FALLBACK_VERSION);
        Err(VersionPathError::MissingDataDirectory(versions))
    }
}
impl FromStr for Version {
    type Err = VersionParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut splits = s.split('.');

        let Some(major) = splits.next() else{ return Err(VersionParseError::NoMajorVersion)};
        let major = major.parse::<u8>().map_err(VersionParseError::ParseMajor)?;

        let minor = if let Some(minor) = splits.next() {
            Some(minor.parse::<u8>().map_err(VersionParseError::ParseMinor)?)
        } else {
            None
        };
        let micro = if let Some(micro) = splits.next() {
            Some(micro.parse::<u8>().map_err(VersionParseError::ParseMicro)?)
        } else {
            None
        };

        if splits.next().is_some() {
            return Err(VersionParseError::ToManyNumbers);
        }

        Ok(Version {
            major,
            minor,
            micro,
        })
    }
}
impl Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.major,
            if let Some(minor) = self.minor {
                format!(
                    ".{}{}",
                    minor,
                    if let Some(micro) = self.micro {
                        format!(".{}", micro)
                    } else {
                        "".to_string()
                    }
                )
            } else {
                "".to_string()
            }
        )
    }
}
impl TryFrom<&Value> for Version {
    type Error = VersionValueError;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        let Value::Array(array) = value else {return Err(VersionValueError::NotAnArray);};
        let mut array = array.iter();

        let Some(Value::Number(major)) = array.next() else {return Err(VersionValueError::NoMajor);};
        let Some(major) = major.as_u64() else {return Err(VersionValueError::NoMajor);};
        let major = major as u8;

        let minor = if let Some(minor) = array.next() {
            let Value::Number(minor) = minor else {return  Err(VersionValueError::NoMinor);};
            let Some(minor) = minor.as_u64() else {return Err(VersionValueError::NoMinor);};
            Some(minor as u8)
        } else {
            None
        };

        let micro = if let Some(micro) = array.next() {
            let Value::Number(micro) = micro else {return  Err(VersionValueError::NoMinor);};
            let Some(micro) = micro.as_u64() else {return Err(VersionValueError::NoMinor);};
            Some(micro as u8)
        } else {
            None
        };

        if array.next().is_some() {
            return Err(VersionValueError::ToManyValues);
        }

        Ok(Version {
            major,
            minor,
            micro,
        })
    }
}
impl From<Version> for Value {
    fn from(val: Version) -> Self {
        let mut vec = vec![val.major.into()];
        if let Some(minor) = val.minor {
            vec.push(minor.into());
            if let Some(micro) = val.micro {
                vec.push(micro.into())
            }
        }
        Value::Array(vec)
    }
}
impl From<(u8, u8, u8)> for Version {
    fn from((d1, d2, d3): (u8, u8, u8)) -> Self {
        Version {
            major: d1,
            minor: Some(d2),
            micro: Some(d3),
        }
    }
}
impl From<(u8, u8)> for Version {
    fn from((d1, d2): (u8, u8)) -> Self {
        Version {
            major: d1,
            minor: Some(d2),
            micro: None,
        }
    }
}
impl From<u8> for Version {
    fn from(d1: u8) -> Self {
        Version {
            major: d1,
            minor: None,
            micro: None,
        }
    }
}

/// Parses a text with a starting version definition to a [`Version`].
/// 
/// The following leading names are possible to use
/// - version 6.1.4
/// - Version 6.1.4
/// - VERSION 6.1.4
/// - FDS 6.1.4
/// 
/// The version can be constructed like
/// - 6
/// - 6.1
/// - 6.1.4
pub fn version_parser<E: Error<char> + 'static>() -> impl Parser<char, Option<Version>, Error = E> {
    let parser = choice::<_, E>((
        just::<_, _, E>("version"),
        just("Version"),
        just("VERSION"),
        just("FDS"),
    ))
    .then(whitespace())
    // .then(digits(10))
    // .to(None);
    .then(
        digits(10).then(
            just('.')
                .then(
                    digits(10).then(
                        just('.')
                            .then(digits(10))
                            .map(|(_, d3)| d3.parse::<u8>().ok())
                            .or_not(),
                    ),
                )
                .map(|(_, (d2, o))| {
                    let d3 = o.flatten();
                    (d2.parse::<u8>().ok(), d3)
                })
                .or_not(),
        ),
    )
    .map(|(_, (d1, o))| {
        let (d2, d3) = if let Some((d2, d3)) = o {
            (d2, d3)
        } else {
            (None, None)
        };

        if let Ok(d1) = d1.parse::<u8>() {
            Some(Version {
                major: d1,
                minor: d2,
                micro: d3,
            })
        } else {
            None
        }
    });

    parser
}

/// Gets the version wich should be used for the current file. The whole text 
/// stream is passed to [`version_parser`]-function. If no version was not or 
/// wrongly defined a fallback [`Version`] is used.
pub fn get_version(stream: String) -> Result<(Version, PathBuf), VersionPathError> {
    let version = if let Ok(Some(version)) = version_parser::<Simple<char>>().parse(stream) {
        version
    } else {
        FALLBACK_VERSION
    };

    version.get_used_version_and_dir_path()
}

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use chumsky::{prelude::Simple, Parser};

    use crate::versions::{Version, DATA_PATH_KEY, FALLBACK_VERSION};

    use super::version_parser;

    #[test]
    fn test_parser() {
        let parser = version_parser::<Simple<char>>();

        assert_eq!(
            Ok(Some(Version {
                major: 6,
                minor: Some(7),
                micro: Some(9),
            })),
            parser.parse("FDS6.7.9-0-gec52dee-HEAD")
        );
        assert_eq!(
            Ok(Some(Version {
                major: 6,
                minor: None,
                micro: None,
            })),
            parser.parse("Version 6")
        );
        assert_eq!(
            Ok(Some(Version {
                major: 6,
                minor: Some(7),
                micro: None,
            })),
            parser.parse("Version 6.7")
        );
        assert_eq!(
            Ok(Some(Version {
                major: 6,
                minor: Some(7),
                micro: Some(9),
            })),
            parser.parse("Version 6.7.9")
        );
        assert_eq!(
            Ok(Some(Version {
                major: 6,
                minor: None,
                micro: None,
            })),
            parser.parse("Version 6-0-gec52dee-HEAD")
        );
        assert_eq!(
            Ok(Some(Version {
                major: 6,
                minor: Some(7),
                micro: None,
            })),
            parser.parse("Version 6.7-0-gec52dee-HEAD")
        );
        assert_eq!(
            Ok(Some(Version {
                major: 6,
                minor: Some(7),
                micro: Some(9),
            })),
            parser.parse("Version 6.7.9-0-gec52dee-HEAD")
        );
    }

    #[test]
    fn test_order_versions() {
        let mut versions = vec![
            Version::from(1),
            Version::from((1, 2)),
            Version::from((1, 1, 1)),
            Version::from((3, 9, 10)),
            Version::from((2, 4, 3)),
        ];
        versions.sort();
        versions.reverse();

        let ordert_versions = vec![
            Version::from((3, 9, 10)),
            Version::from((2, 4, 3)),
            Version::from((1, 2)),
            Version::from((1, 1, 1)),
            Version::from(1),
        ];
        assert_eq!(ordert_versions, versions);
    }

    #[test]
    fn test_get_path() {
        let mut args = std::iter::once(format!("{}=./data/", DATA_PATH_KEY));
        let version = Version::from((7, 3, 2));
        assert_eq!(
            version.get_used_version_and_dir_path_custom_args(&mut args.clone()),
            Ok((
                FALLBACK_VERSION,
                PathBuf::from("./data/").join(FALLBACK_VERSION.to_string())
            ))
        );

        let version = Version::from((6, 1, 4));
        assert_eq!(
            version.get_used_version_and_dir_path_custom_args(&mut args),
            Ok((
                Version::from((6, 1, 4)),
                PathBuf::from("./data/").join(version.to_string())
            ))
        );
    }
}
