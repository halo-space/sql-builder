//! Field mapper: map Rust field names to column names.

use std::sync::{Arc, Mutex, MutexGuard, OnceLock};

/// Function type for field name mapping.
pub type FieldMapperFunc = Arc<dyn Fn(&str) -> String + Send + Sync + 'static>;

fn identity_impl(s: &str) -> String {
    s.to_string()
}

static IDENTITY_MAPPER: OnceLock<FieldMapperFunc> = OnceLock::new();

/// Identity mapper (equivalent to leaving the mapper unset).
pub fn identity_mapper() -> FieldMapperFunc {
    IDENTITY_MAPPER
        .get_or_init(|| Arc::new(identity_impl))
        .clone()
}

static DEFAULT_FIELD_MAPPER: OnceLock<Mutex<FieldMapperFunc>> = OnceLock::new();
static DEFAULT_FIELD_MAPPER_LOCK: Mutex<()> = Mutex::new(());

fn mapper_cell() -> &'static Mutex<FieldMapperFunc> {
    DEFAULT_FIELD_MAPPER.get_or_init(|| Mutex::new(identity_mapper()))
}

/// Get the current global default FieldMapper.
pub fn default_field_mapper() -> FieldMapperFunc {
    mapper_cell()
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .clone()
}

/// Set the global default FieldMapper and return the previous one.
pub fn set_default_field_mapper(mapper: FieldMapperFunc) -> FieldMapperFunc {
    let mut g = mapper_cell().lock().unwrap_or_else(|e| e.into_inner());
    std::mem::replace(&mut *g, mapper)
}

/// RAII guard for temporarily replacing the global FieldMapper (holds a global lock to avoid test interference).
pub struct DefaultFieldMapperGuard {
    _lock: MutexGuard<'static, ()>,
    old: FieldMapperFunc,
}

impl Drop for DefaultFieldMapperGuard {
    fn drop(&mut self) {
        let _ = set_default_field_mapper(self.old.clone());
    }
}

/// Temporarily set the default FieldMapper for a scope and restore it on drop.
pub fn set_default_field_mapper_scoped(mapper: FieldMapperFunc) -> DefaultFieldMapperGuard {
    let lock = DEFAULT_FIELD_MAPPER_LOCK
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    let old = set_default_field_mapper(mapper);
    DefaultFieldMapperGuard { _lock: lock, old }
}

fn convert_with_separator(s: &str, sep: char) -> String {
    let mut out = String::with_capacity(s.len() + 8);
    let mut prev: Option<char> = None;
    let chars: Vec<char> = s.chars().collect();

    for (i, &c) in chars.iter().enumerate() {
        let next = chars.get(i + 1).copied();
        let is_upper = c.is_ascii_uppercase();

        if is_upper {
            if let Some(p) = prev {
                let prev_is_lower_or_digit = p.is_ascii_lowercase() || p.is_ascii_digit();
                let prev_is_upper = p.is_ascii_uppercase();
                let next_is_lower = next.map(|n| n.is_ascii_lowercase()).unwrap_or(false);

                if prev_is_lower_or_digit || (prev_is_upper && next_is_lower) {
                    out.push(sep);
                }
            }
            out.push(c.to_ascii_lowercase());
        } else {
            out.push(c);
        }

        prev = Some(c);
    }

    out
}

/// SnakeCaseMapper: convert `CamelCase` to `snake_case`.
///
/// Notes: this implementation covers the rules needed for this crate's tests:
/// - upper to lower case
/// - insert `_` at word boundaries (`aB` / `a1B` / `ABc` etc.)
pub fn snake_case_mapper(s: &str) -> String {
    convert_with_separator(s, '_')
}

/// KebabcaseMapper: convert `CamelCase` to `kebab-case`.
pub fn kebab_case_mapper(s: &str) -> String {
    convert_with_separator(s, '-')
}

/// UpperCaseMapper: convert a field name to uppercase.
pub fn upper_case_mapper(s: &str) -> String {
    s.to_ascii_uppercase()
}

/// PrefixMapper: add a fixed prefix to a field name.
pub fn prefix_mapper(prefix: &'static str) -> FieldMapperFunc {
    Arc::new(move |name| format!("{prefix}{name}"))
}

/// SuffixMapper: add a fixed suffix to a field name.
pub fn suffix_mapper(suffix: &'static str) -> FieldMapperFunc {
    Arc::new(move |name| format!("{name}{suffix}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn camel_case_helpers_work() {
        assert_eq!(snake_case_mapper("FieldName"), "field_name");
        assert_eq!(kebab_case_mapper("FieldName"), "field-name");
    }

    #[test]
    fn upper_case_mapper_changes_case() {
        assert_eq!(upper_case_mapper("FieldName"), "FIELDNAME");
    }

    #[test]
    fn prefix_suffix_mappers_apply() {
        let prefix = prefix_mapper("db_");
        let suffix = suffix_mapper("_col");
        assert_eq!(prefix("FieldName"), "db_FieldName");
        assert_eq!(suffix("FieldName"), "FieldName_col");
    }
}
