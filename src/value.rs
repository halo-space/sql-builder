//! SQL parameter value types.

use std::borrow::Cow;

/// SQL parameter value representation.
#[derive(Debug, Clone, PartialEq)]
pub enum SqlValue {
    Null,
    Bool(bool),
    I64(i64),
    U64(u64),
    F64(f64),
    String(Cow<'static, str>),
    Bytes(Vec<u8>),
    DateTime(SqlDateTime),
}

/// SQL datetime value with optional timezone abbreviation.
#[derive(Debug, Clone, PartialEq)]
pub struct SqlDateTime {
    pub dt: time::OffsetDateTime,
    pub tz_abbr: Option<Cow<'static, str>>,
}

impl SqlDateTime {
    pub fn new(dt: time::OffsetDateTime) -> Self {
        Self { dt, tz_abbr: None }
    }

    pub fn with_tz_abbr(mut self, abbr: impl Into<Cow<'static, str>>) -> Self {
        self.tz_abbr = Some(abbr.into());
        self
    }
}

impl SqlValue {
    /// Map `Option<T>` to `SqlValue`: `None => Null`, `Some(v) => v.into()`.
    pub fn from_option<T: Into<SqlValue>>(v: Option<T>) -> Self {
        match v {
            Some(v) => v.into(),
            None => Self::Null,
        }
    }
}

impl From<()> for SqlValue {
    fn from(_: ()) -> Self {
        Self::Null
    }
}

impl From<bool> for SqlValue {
    fn from(v: bool) -> Self {
        Self::Bool(v)
    }
}

impl From<i8> for SqlValue {
    fn from(v: i8) -> Self {
        Self::I64(v as i64)
    }
}

impl From<i16> for SqlValue {
    fn from(v: i16) -> Self {
        Self::I64(v as i64)
    }
}

impl From<i32> for SqlValue {
    fn from(v: i32) -> Self {
        Self::I64(v as i64)
    }
}

impl From<i64> for SqlValue {
    fn from(v: i64) -> Self {
        Self::I64(v)
    }
}

impl From<u8> for SqlValue {
    fn from(v: u8) -> Self {
        Self::U64(v as u64)
    }
}

impl From<u16> for SqlValue {
    fn from(v: u16) -> Self {
        Self::U64(v as u64)
    }
}

impl From<u32> for SqlValue {
    fn from(v: u32) -> Self {
        Self::U64(v as u64)
    }
}

impl From<u64> for SqlValue {
    fn from(v: u64) -> Self {
        Self::U64(v)
    }
}

impl From<f32> for SqlValue {
    fn from(v: f32) -> Self {
        Self::F64(v as f64)
    }
}

impl From<f64> for SqlValue {
    fn from(v: f64) -> Self {
        Self::F64(v)
    }
}

impl From<String> for SqlValue {
    fn from(v: String) -> Self {
        Self::String(Cow::Owned(v))
    }
}

impl From<&'static str> for SqlValue {
    fn from(v: &'static str) -> Self {
        Self::String(Cow::Borrowed(v))
    }
}

impl From<Vec<u8>> for SqlValue {
    fn from(v: Vec<u8>) -> Self {
        Self::Bytes(v)
    }
}

impl From<time::OffsetDateTime> for SqlValue {
    fn from(v: time::OffsetDateTime) -> Self {
        Self::DateTime(SqlDateTime::new(v))
    }
}

#[cfg(test)]
mod tests {
    use super::SqlValue;

    #[test]
    fn from_option_some() {
        assert_eq!(SqlValue::from_option(Some(123_i64)), SqlValue::I64(123));
    }

    #[test]
    fn from_option_none() {
        assert_eq!(SqlValue::from_option::<i64>(None), SqlValue::Null);
    }

    #[test]
    fn from_unit_is_null() {
        let v: SqlValue = ().into();
        assert_eq!(v, SqlValue::Null);
    }

    #[test]
    fn from_string_borrowed() {
        let v: SqlValue = "abc".into();
        assert_eq!(v, SqlValue::String("abc".into()));
    }

    #[test]
    fn from_string_owned() {
        let v: SqlValue = String::from("abc").into();
        assert_eq!(v, SqlValue::String("abc".into()));
    }
}
