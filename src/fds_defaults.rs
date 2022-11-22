use std::fs;

use anyhow::Result;
use serde_json::{Number, Value};
use tower_lsp::lsp_types::{CompletionItem, CompletionTextEdit, TextEdit};

#[derive(Debug)]
pub struct FDSDefault {
    pub classes: Vec<String>,
    pub properties: Vec<String>,

    pub information: Vec<String>,
    pub completion_items: Vec<Vec<String>>,
}

impl FDSDefault {
    pub fn try_new(content: &str) -> Result<Self> {
        let mut lines = content.split('\n').filter(|f| !f.is_empty());
        let Some(line1) = lines.next() else {return Err(anyhow::Error::msg("No first describing line found"))};
        let mut line1 = line1.split(';');
        let (Some(classes), Some(properties)) = (line1.next(), line1.next()) else {return Err(anyhow::Error::msg("Unable to fined definition for defaults."))};

        let classes = classes
            .split(',')
            .filter(|f| !f.is_empty())
            .map(|f| f.to_string())
            .collect::<Vec<_>>();
        let properties = properties
            .split(',')
            .filter(|f| !f.is_empty())
            .map(|f| f.to_string())
            .collect::<Vec<_>>();

        let Some(line2) = lines.next() else{return Err(anyhow::Error::msg("No description row found"))};

        let information = line2
            .split(';')
            .filter(|f| !f.is_empty())
            .map(|f| f.to_string())
            .collect::<Vec<_>>();

        let completion_items = lines
            .map(|f| f.split(';').map(|f| f.to_string()).collect::<Vec<_>>())
            .collect::<Vec<_>>();

        Ok(Self {
            classes,
            properties,
            information,
            completion_items,
        })
    }

    pub fn get_completion_items(&self, index: usize) -> Vec<CompletionItem> {
        self.completion_items
            .iter()
            .enumerate()
            .map(|(i, labels)| {
                let label = format!("\"{}\"", labels[0].clone());
                CompletionItem {
                    label,
                    data: Some(Value::Array(vec![
                        Value::Number(Number::from(2u64)),
                        Value::Number(Number::from(index as u64)),
                        Value::Number(Number::from(i as u64)),
                    ])),
                    ..Default::default()
                }
            })
            .collect::<Vec<_>>()
    }

    pub fn get_element_markdown(&self, name: &String) -> Option<String> {
        if let Some(vec) = self
            .completion_items
            .iter()
            .find(|f| f.first() == Some(name))
        {
            let mut iter_item = vec.iter();
            let mut iter_info = self.information.iter();

            let label = iter_item.next()?.clone();
            let _ = iter_info.next()?;

            let info = iter_info
                .filter_map(|info| {
                    if let Some(value) = iter_item.next() {
                        Some(format!("**{}**: {}  ", info, value))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
                .join("\n");

            Some(format!("`{}`  \n{}", label, info))
        } else {
            None
        }
    }

    //TODO Rename
    pub fn is_item(&self, class: &String, property: &String) -> bool {
        (self.classes.len() == 0 || self.classes.contains(class))
            && self.properties.contains(property)
    }
}

pub fn get_defaults(path: &str) -> Vec<FDSDefault> {
    let file = fs::read_to_string(path).expect("Should have been able to read the file");

    file.split('#')
        .filter(|f| !f.is_empty())
        .filter_map(|f| FDSDefault::try_new(f).ok())
        .collect()
}

#[test]
fn test_fds_default() {
    let d = FDSDefault::try_new(
        r#"CLASS;PROPERTY
First Column;Second Column;Third Column;
FIRST_LABLE;Value 1;Value 2
SECOND_LABLE;Value 1
THIRD_LABLE;Value 1;Value 2;Value 3"#,
    )
    .unwrap();

    println!("{:?}", d);

    let first_lable = d.get_element_markdown(&"FIRST_LABLE".to_string());
    println!("{:?}", first_lable);

    let second_lable = d.get_element_markdown(&"SECOND_LABLE".to_string());
    println!("{:?}", second_lable);

    let third_lable = d.get_element_markdown(&"THIRD_LABLE".to_string());
    println!("{:?}", third_lable);
}
