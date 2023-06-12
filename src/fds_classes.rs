//! Add hover and auto completion support for classes and properties

use anyhow::Result;
use std::{
    collections::HashMap,
    fs,
    ops::{Deref, DerefMut},
    path::Path,
};
use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind};

use crate::{completion::CompletionItemValue, versions::Version};

/// A wrapper for [`HashMap<String, FDSClass>`]
#[derive(Debug, Default)]
pub struct FDSClasses(HashMap<String, FDSClass>);
impl Deref for FDSClasses {
    type Target = HashMap<String, FDSClass>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for FDSClasses {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}


/// A class FDS supports. Also known as namelist
#[derive(Debug)]
pub struct FDSClass {
    /// The name of the class.
    pub label: String,
    /// Information about the class.
    definition: String,
    /// Reference information about the class.
    reference: String,
    /// All supported properties of a class.
    pub properties: HashMap<String, FDSClassProperty>,
}

impl FDSClass {
    /// Tries to create a fds class from a text chunk.
    /// 
    /// The chunk has to look like the following:
    /// ```text
    /// \t22.6\tCTRL\t(Control Function Parameters)
    /// Reference:\tSection 20.5.
    /// CONSTANT\tReal\tSection 20.5.6
    /// DELAY\tReal\tSection 20.5.10\ts\t0.
    /// ```
    ///
    /// # Errors
    ///
    /// This function will return an error if
    /// - The first and second line is missing.
    fn try_new(content: &str) -> Result<FDSClass> {
        let mut splits = content.split('\n');

        let Some(first_line) =  splits.next() else {return Err(anyhow::Error::msg("First line is missing"))};
        let mut first_line = first_line.split('\t');
        first_line.next();
        first_line.next(); // Number
        let label = first_line.next().unwrap_or("ERROR").to_string();
        let definition = first_line.next().unwrap_or("No Definition").to_string();

        let Some(second_line) = splits.next() else {return  Err(anyhow::Error::msg("Second line is missing"))};
        let mut second_line = second_line.split('\t');
        second_line.next();
        let reference = second_line.next().unwrap_or("No Reference").to_string();

        let mut properties = HashMap::default();
        for split in splits {
            if !split.is_empty() {
                if let Ok(property) = FDSClassProperty::try_new(split) {
                    properties.insert(property.label.clone(), property);
                }
            }
        }

        Ok(FDSClass {
            label,
            reference,
            definition,
            properties,
        })
    }

    /// Get the markdown representation from this class
    pub fn to_markdown_string(&self) -> String {
        format!(
            "## **`{}` {}**  \nFor more information see {}  \n\n## Properties  \n{}",
            self.label,
            self.definition,
            self.reference,
            self.properties
                .iter()
                .map(|f| f.1.to_markdown_string())
                .collect::<Vec<String>>()
                .join("\n"),
        )
    }

    /// Get the class name as completion item
    pub fn get_completion_item(&self, version: Version) -> CompletionItem {
        let kind = Some(CompletionItemKind::CLASS);
        CompletionItem {
            label: self.label.clone(),
            //documentation,
            kind,
            data: Some(CompletionItemValue::Class { version }.into()),
            ..Default::default()
        }
    }
}

/// A property a class can have.
#[derive(Debug)]
pub struct FDSClassProperty {
    /// The name of the property
    pub label: String,
    /// The user input type
    kind: Option<String>,
    /// The reference information for the property
    reference: Option<String>,
    /// The unit of the property
    unit: Option<String>,
    /// The default value of the property
    default: Option<String>,
}

impl FDSClassProperty {
    /// Tries to create a property from a text chunk.
    ///
    /// The chunk has to look like the following:
    /// ```text
    /// CONSTANT\tReal\tSection 20.5.6
    /// ```
    /// 
    /// # Errors
    ///
    /// This function will return an error if 
    /// - The string is empty.
    fn try_new(content: &str) -> Result<FDSClassProperty> {
        /// Helper function to get the optional value from the next element in `splits`
        fn get_next(next: Option<&str>) -> Option<String> {
            if let Some(value) = next {
                if value.is_empty() {
                    None
                } else {
                    Some(value.to_string())
                }
            } else {
                None
            }
        }

        let mut splits = content.split('\t');
        let Some(label) = splits.next() else {return Err(anyhow::Error::msg("String is empty"))};
        let label = label.to_string();

        let kind = get_next(splits.next());
        let reference = get_next(splits.next());
        let unit = get_next(splits.next());
        let default = get_next(splits.next());

        Ok(FDSClassProperty {
            label,
            kind,
            reference,
            unit,
            default,
        })
    }

    /// Get the markdown representation from this property
    pub fn to_markdown_string(&self) -> String {
        format!(
            "`{}`  \n{}{}{}{}",
            self.label,
            if let Some(kind) = &self.kind {
                format!("Type: {}  \n", kind)
            } else {
                "".to_string()
            },
            if let Some(reference) = &self.reference {
                format!("Reference: {}  \n", reference)
            } else {
                "".to_string()
            },
            if let Some(unit) = &self.unit {
                format!("Unit: {}  \n", unit)
            } else {
                "".to_string()
            },
            if let Some(default) = &self.default {
                format!("Default: {}  \n", default)
            } else {
                "".to_string()
            }
        )
    }

    /// Get the property name as completion item
    pub fn get_completion_item(&self, class_name: String, version: Version) -> CompletionItem {
        let kind = Some(CompletionItemKind::CLASS);
        CompletionItem {
            label: self.label.clone(),
            //documentation,
            kind,
            data: Some(
                CompletionItemValue::Property {
                    version,
                    class_name,
                }
                .into(),
            ),
            ..Default::default()
        }
    }
}

/// Get [`FDSClasses`] from a file
pub fn get_classes2<P: AsRef<Path>>(path: P) -> FDSClasses {
    let file = fs::read_to_string(path).expect("Should have been able to read the file");

    let mut fds_classes = FDSClasses::default();
    let mut splits = file.split("##");
    splits.next();
    for s in splits {
        if let Ok(ok) = FDSClass::try_new(s) {
            fds_classes.insert(ok.label.clone(), ok);
        }
    }
    fds_classes
}
