use crate::Program;
use crate::internal::as2::hir::Class;
use indexmap::IndexMap;
use std::collections::HashMap;

pub struct FieldDefinition {
    pub name: String,
    pub type_name: Option<String>,
    pub is_virtual: bool,
}

pub struct ClassDefinition {
    pub extends: Option<String>,
    pub fields: HashMap<String, FieldDefinition>,
}

impl From<&Class> for ClassDefinition {
    fn from(value: &Class) -> Self {
        let mut fields = HashMap::new();
        for (field_name, field) in value.fields.iter() {
            fields.insert(
                field_name.clone(),
                FieldDefinition {
                    name: field_name.clone(),
                    type_name: field.type_name.clone().map(|v| v.value),
                    is_virtual: false,
                },
            );
        }
        for (field_name, _field) in value.virtual_properties.iter() {
            fields.insert(
                field_name.clone(),
                FieldDefinition {
                    name: field_name.clone(),
                    type_name: None,
                    is_virtual: true,
                },
            );
        }
        ClassDefinition {
            extends: value.extends.clone(),
            fields,
        }
    }
}

pub enum TypeDefinition {
    Class(ClassDefinition),
}

pub struct ProgramDefinition {
    pub types: IndexMap<String, TypeDefinition>,
}

impl From<&Program> for ProgramDefinition {
    fn from(program: &Program) -> Self {
        let mut result = Self {
            types: Default::default(),
        };

        for class_def in &program.classes {
            result.types.insert(
                class_def.name.clone(),
                TypeDefinition::Class(ClassDefinition::from(class_def)),
            );
        }

        result
    }
}

impl ProgramDefinition {
    pub fn get_field<'a>(
        &'a self,
        type_name: &str,
        field_name: &str,
    ) -> Option<&'a FieldDefinition> {
        let mut current_type = Some(type_name);
        while let Some(type_name) = current_type {
            if let Some(type_def) = self.types.get(type_name)
                && let TypeDefinition::Class(class_def) = type_def
            {
                if let Some(field) = class_def.fields.get(field_name) {
                    return Some(field);
                }
                current_type = class_def.extends.as_deref();
                continue;
            }
            current_type = None;
        }
        None
    }
}
