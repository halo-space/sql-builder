//! Argument modifiers and helpers for builders.

use crate::flavor::Flavor;
use crate::value::SqlValue;
use crate::valuer::SqlValuer;
use dyn_clone::DynClone;
use std::cell::RefCell;
use std::rc::Rc;

/// Escape: replace `$` with `$$` to avoid being parsed as an expression by `Args::compile`.
pub fn escape(ident: &str) -> String {
    ident.replace('$', "$$")
}

/// EscapeAll: escape a collection of identifiers.
pub fn escape_all(idents: impl IntoIterator<Item = impl AsRef<str>>) -> Vec<String> {
    idents.into_iter().map(|s| escape(s.as_ref())).collect()
}

/// Raw: inline SQL fragment without becoming a placeholder.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Raw {
    pub(crate) expr: String,
}

pub fn raw(expr: impl Into<String>) -> Arg {
    Arg::Raw(Raw { expr: expr.into() })
}

/// List: mark as argument list; expands to `?, ?, ?` (or flavor-specific placeholders).
pub fn list<T: FlattenIntoArgs>(arg: T) -> Arg {
    let mut out = Vec::new();
    arg.flatten_into(&mut out);
    Arg::List {
        args: out,
        is_tuple: false,
    }
}

/// Tuple: mark as tuple; expands to `(?, ?)` (or flavor-specific placeholders).
pub fn tuple<T: FlattenIntoArgs>(values: T) -> Arg {
    let mut out = Vec::new();
    values.flatten_into(&mut out);
    Arg::List {
        args: out,
        is_tuple: true,
    }
}

/// TupleNames: build a column tuple string like `(a, b, c)` without escaping.
pub fn tuple_names(names: impl IntoIterator<Item = impl AsRef<str>>) -> String {
    let mut s = String::from("(");
    let mut first = true;
    for n in names {
        if !first {
            s.push_str(", ");
        }
        first = false;
        s.push_str(n.as_ref());
    }
    s.push(')');
    s
}

/// Flatten: recursively expand values into argument list (trait-based, no reflection).
pub fn flatten<T: FlattenIntoArgs>(v: T) -> Vec<Arg> {
    let mut out = Vec::new();
    v.flatten_into(&mut out);
    out
}

/// Named: named parameter used by `Build/BuildNamed` via `${name}`.
pub fn named(name: impl Into<String>, arg: impl Into<Arg>) -> Arg {
    Arg::Named {
        name: name.into(),
        arg: Box::new(arg.into()),
    }
}

/// SqlNamedArg: helper for `@name` placeholders inside SQL.
#[derive(Debug, Clone, PartialEq)]
pub struct SqlNamedArg {
    pub name: String,
    pub value: Box<Arg>,
}

impl SqlNamedArg {
    pub fn new(name: impl Into<String>, value: impl Into<Arg>) -> Self {
        Self {
            name: name.into(),
            value: Box::new(value.into()),
        }
    }
}

/// Dynamic argument types used by the Builder/Args system.
#[derive(Clone)]
pub enum Arg {
    Value(SqlValue),
    Valuer(Box<dyn SqlValuer>),
    SqlNamed(SqlNamedArg),
    Raw(Raw),
    /// Unified representation for lists/tuples.
    List {
        args: Vec<Arg>,
        is_tuple: bool,
    },
    /// Named(name,arg) — only effective for `${name}` in Build/BuildNamed.
    Named {
        name: String,
        arg: Box<Arg>,
    },
    Builder(Box<dyn Builder>),
}

impl std::fmt::Debug for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(v) => f.debug_tuple("Value").field(v).finish(),
            Self::Valuer(_) => f.write_str("Valuer(..)"),
            Self::SqlNamed(v) => f.debug_tuple("SqlNamed").field(v).finish(),
            Self::Raw(v) => f.debug_tuple("Raw").field(v).finish(),
            Self::List { args, is_tuple } => f
                .debug_struct("List")
                .field("args", args)
                .field("is_tuple", is_tuple)
                .finish(),
            Self::Named { name, arg } => f
                .debug_struct("Named")
                .field("name", name)
                .field("arg", arg)
                .finish(),
            Self::Builder(_) => f.write_str("Builder(..)"),
        }
    }
}

impl PartialEq for Arg {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Value(a), Self::Value(b)) => a == b,
            (Self::Valuer(_), _) | (_, Self::Valuer(_)) => false,
            (Self::SqlNamed(a), Self::SqlNamed(b)) => a == b,
            (Self::Raw(a), Self::Raw(b)) => a == b,
            (
                Self::List {
                    args: a,
                    is_tuple: at,
                },
                Self::List {
                    args: b,
                    is_tuple: bt,
                },
            ) => at == bt && a == b,
            (Self::Named { name: an, arg: aa }, Self::Named { name: bn, arg: ba }) => {
                an == bn && aa == ba
            }
            (Self::Builder(_), _) | (_, Self::Builder(_)) => false,
            _ => false,
        }
    }
}

impl From<Box<dyn Builder>> for Arg {
    fn from(v: Box<dyn Builder>) -> Self {
        Self::Builder(v)
    }
}

impl From<Box<dyn SqlValuer>> for Arg {
    fn from(v: Box<dyn SqlValuer>) -> Self {
        Self::Valuer(v)
    }
}

impl Builder for Box<dyn Builder> {
    fn build_with_flavor(&self, flavor: Flavor, initial_arg: &[Arg]) -> (String, Vec<Arg>) {
        (**self).build_with_flavor(flavor, initial_arg)
    }

    fn flavor(&self) -> Flavor {
        (**self).flavor()
    }
}

/// Builder: trait for objects that can build SQL (nestable).
pub trait Builder: DynClone {
    fn build(&self) -> (String, Vec<Arg>) {
        self.build_with_flavor(self.flavor(), &[])
    }

    fn build_with_flavor(&self, flavor: Flavor, initial_arg: &[Arg]) -> (String, Vec<Arg>);

    fn flavor(&self) -> Flavor;
}

dyn_clone::clone_trait_object!(Builder);

/// RcBuilder: wrap `Rc<RefCell<T>>` as a Builder to enable shared, late-bound builders.
///
/// Typical usage: pass a `SelectBuilder` as a subquery argument while allowing further mutation,
/// so the final build uses the latest state.
#[derive(Debug)]
pub struct RcBuilder<T: Builder> {
    inner: Rc<RefCell<T>>,
}

impl<T: Builder> Clone for RcBuilder<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl<T: Builder> RcBuilder<T> {
    pub fn new(inner: Rc<RefCell<T>>) -> Self {
        Self { inner }
    }

    pub fn inner(&self) -> Rc<RefCell<T>> {
        self.inner.clone()
    }
}

impl<T: Builder> Builder for RcBuilder<T> {
    fn build_with_flavor(&self, flavor: Flavor, initial_arg: &[Arg]) -> (String, Vec<Arg>) {
        self.inner.borrow().build_with_flavor(flavor, initial_arg)
    }

    fn flavor(&self) -> Flavor {
        self.inner.borrow().flavor()
    }
}

pub fn rc_builder<T: Builder>(inner: Rc<RefCell<T>>) -> RcBuilder<T> {
    RcBuilder::new(inner)
}

impl From<SqlValue> for Arg {
    fn from(v: SqlValue) -> Self {
        Self::Value(v)
    }
}

impl From<i64> for Arg {
    fn from(v: i64) -> Self {
        SqlValue::I64(v).into()
    }
}
impl From<i32> for Arg {
    fn from(v: i32) -> Self {
        SqlValue::I64(v as i64).into()
    }
}
impl From<u64> for Arg {
    fn from(v: u64) -> Self {
        SqlValue::U64(v).into()
    }
}
impl From<u16> for Arg {
    fn from(v: u16) -> Self {
        SqlValue::U64(v as u64).into()
    }
}
impl From<bool> for Arg {
    fn from(v: bool) -> Self {
        SqlValue::Bool(v).into()
    }
}
impl From<f64> for Arg {
    fn from(v: f64) -> Self {
        SqlValue::F64(v).into()
    }
}
impl From<&'static str> for Arg {
    fn from(v: &'static str) -> Self {
        SqlValue::from(v).into()
    }
}
impl From<String> for Arg {
    fn from(v: String) -> Self {
        SqlValue::from(v).into()
    }
}
impl From<Vec<u8>> for Arg {
    fn from(v: Vec<u8>) -> Self {
        SqlValue::Bytes(v).into()
    }
}

impl<T> From<Option<T>> for Arg
where
    T: Into<SqlValue>,
{
    fn from(v: Option<T>) -> Self {
        match v {
            Some(x) => x.into().into(),
            None => SqlValue::Null.into(),
        }
    }
}

impl From<time::OffsetDateTime> for Arg {
    fn from(v: time::OffsetDateTime) -> Self {
        SqlValue::from(v).into()
    }
}
impl From<SqlNamedArg> for Arg {
    fn from(v: SqlNamedArg) -> Self {
        Self::SqlNamed(v)
    }
}

/// Trait-based implementation of recursive flattening.
pub trait FlattenIntoArgs {
    fn flatten_into(self, out: &mut Vec<Arg>);
}

impl<T: Into<Arg>> FlattenIntoArgs for T {
    fn flatten_into(self, out: &mut Vec<Arg>) {
        out.push(self.into());
    }
}

impl<T: FlattenIntoArgs> FlattenIntoArgs for Vec<T> {
    fn flatten_into(self, out: &mut Vec<Arg>) {
        for v in self {
            v.flatten_into(out);
        }
    }
}

impl<T: FlattenIntoArgs, const N: usize> FlattenIntoArgs for [T; N] {
    fn flatten_into(self, out: &mut Vec<Arg>) {
        for v in self {
            v.flatten_into(out);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_escape() {
        assert_eq!(escape("foo"), "foo");
        assert_eq!(escape("$foo"), "$$foo");
        assert_eq!(escape("$$$"), "$$$$$$");
    }

    #[test]
    fn test_escape_all() {
        assert_eq!(
            escape_all(["foo", "$foo"]),
            vec!["foo".to_string(), "$$foo".to_string()]
        );
    }

    #[test]
    fn tuple_names_basic() {
        assert_eq!(tuple_names(["a", "b"]), "(a, b)");
    }

    #[test]
    fn flatten_vec_and_array() {
        let a = list(vec![1_i64, 2, 3]);
        match a {
            Arg::List { args, is_tuple } => {
                assert!(!is_tuple);
                assert_eq!(args.len(), 3);
            }
            _ => panic!("expected list"),
        }

        let b = list([1_i64, 2, 3]);
        match b {
            Arg::List { args, is_tuple } => {
                assert!(!is_tuple);
                assert_eq!(args.len(), 3);
            }
            _ => panic!("expected list"),
        }
    }
}
