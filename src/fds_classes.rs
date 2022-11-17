use anyhow::Result;
use serde_json::Value;
use std::{collections::HashMap, fs};
use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind};

#[derive(Debug)]
pub struct FDSClass {
    pub label: String,
    definition: String,
    reference: String,
    pub properties: HashMap<String, FDSClassProperty>,
}

impl FDSClass {
    fn try_new(content: &str) -> Result<FDSClass> {
        let mut splits = content.split("\n");

        let mut first_line = splits.next().expect("msg").split("\t"); //Empty
        first_line.next();
        first_line.next(); // Number
        let label = first_line.next().unwrap_or_else(|| "ERROR").to_string();
        let definition = first_line
            .next()
            .unwrap_or_else(|| "No Definition")
            .to_string();

        let mut second_line = splits.next().expect("msg").split("\t");
        second_line.next();
        let reference = second_line
            .next()
            .unwrap_or_else(|| "No Reference")
            .to_string();

        let mut properties = HashMap::default();
        for split in splits {
            if !split.is_empty() {
                if let Ok(property) = FDSClassProperty::try_new(split) {
                    properties.insert(property.label.clone(), property);
                }
            }
        }
        //

        Ok(FDSClass {
            label,
            reference,
            definition,
            properties,
        })
    }

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

    pub fn get_completion_item(&self) -> CompletionItem {
        let kind = Some(CompletionItemKind::CLASS);
        CompletionItem {
            label: self.label.clone(),
            //documentation,
            kind,
            data: Some(Value::String(self.label.clone())),
            ..Default::default()
        }
    }
}

#[derive(Debug)]
pub struct FDSClassProperty {
    pub label: String,
    kind: Option<String>,
    reference: Option<String>,
    unit: Option<String>,
    default: Option<String>,
}

impl FDSClassProperty {
    fn try_new(content: &str) -> Result<FDSClassProperty> {
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

        let mut splits = content.split("\t");
        let label = splits.next().expect("Name").to_string();

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

    pub fn get_completion_item(&self, source: String) -> CompletionItem {
        let kind = Some(CompletionItemKind::CLASS);
        CompletionItem {
            label: self.label.clone(),
            //documentation,
            kind,
            data: Some(Value::Array(vec![
                Value::String(source),
                Value::String(self.label.clone()),
            ])),
            ..Default::default()
        }
    }
}

pub fn get_classes(path: &str) -> Vec<FDSClass> {
    let file = fs::read_to_string(path).expect("Should have been able to read the file");

    let mut splits = file.split("##");
    splits.next();
    splits.filter_map(|f| FDSClass::try_new(f).ok()).collect()
}
