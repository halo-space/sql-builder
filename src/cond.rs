//! Cond: helpers to build WHERE clause expressions.

use crate::args::Args;
use crate::flavor::Flavor;
use crate::macros::{IntoStrings, collect_into_strings};
use crate::modifiers::{Arg, Builder};
use crate::string_builder::{StringBuilder, filter_empty_strings};
use std::cell::RefCell;
use std::rc::Rc;

const MIN_INDEX_BASE: usize = 256;

pub type ArgsRef = Rc<RefCell<Args>>;

/// Cond provides helper methods for conditional expressions.
#[derive(Debug, Clone)]
pub struct Cond {
    pub(crate) args: ArgsRef,
}

impl Cond {
    /// Create an independent Cond; uses a larger index_base to avoid accidental recursion.
    pub fn new() -> Self {
        let a = Args {
            index_base: MIN_INDEX_BASE,
            ..Args::default()
        };
        Self {
            args: Rc::new(RefCell::new(a)),
        }
    }

    pub(crate) fn with_args(args: ArgsRef) -> Self {
        Self { args }
    }

    /// Var: store a value into Args and return the `$n` placeholder.
    pub fn var(&self, value: impl Into<Arg>) -> String {
        self.args.borrow_mut().add(value)
    }

    fn expr_builder(&self, f: impl Fn(Flavor, &[Arg]) -> (String, Vec<Arg>) + 'static) -> String {
        self.var(Arg::Builder(Box::new(CondDynBuilder::new(f))))
    }

    pub fn equal(&self, field: &str, value: impl Into<Arg>) -> String {
        if field.is_empty() {
            return String::new();
        }
        let field = field.to_string();
        let value: Arg = value.into();
        self.expr_builder(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let v = a.add(value.clone());
            let fmt = format!("{field} = {v}");
            a.compile_with_flavor(&fmt, flavor, initial)
        })
    }
    pub fn e(&self, field: &str, value: impl Into<Arg>) -> String {
        self.equal(field, value)
    }
    pub fn eq(&self, field: &str, value: impl Into<Arg>) -> String {
        self.equal(field, value)
    }

    pub fn not_equal(&self, field: &str, value: impl Into<Arg>) -> String {
        if field.is_empty() {
            return String::new();
        }
        let field = field.to_string();
        let value: Arg = value.into();
        self.expr_builder(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let v = a.add(value.clone());
            let fmt = format!("{field} <> {v}");
            a.compile_with_flavor(&fmt, flavor, initial)
        })
    }
    pub fn ne(&self, field: &str, value: impl Into<Arg>) -> String {
        self.not_equal(field, value)
    }
    pub fn neq(&self, field: &str, value: impl Into<Arg>) -> String {
        self.not_equal(field, value)
    }

    pub fn greater_than(&self, field: &str, value: impl Into<Arg>) -> String {
        if field.is_empty() {
            return String::new();
        }
        let field = field.to_string();
        let value: Arg = value.into();
        self.expr_builder(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let v = a.add(value.clone());
            let fmt = format!("{field} > {v}");
            a.compile_with_flavor(&fmt, flavor, initial)
        })
    }
    pub fn g(&self, field: &str, value: impl Into<Arg>) -> String {
        self.greater_than(field, value)
    }
    pub fn gt(&self, field: &str, value: impl Into<Arg>) -> String {
        self.greater_than(field, value)
    }

    pub fn greater_equal_than(&self, field: &str, value: impl Into<Arg>) -> String {
        if field.is_empty() {
            return String::new();
        }
        let field = field.to_string();
        let value: Arg = value.into();
        self.expr_builder(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let v = a.add(value.clone());
            let fmt = format!("{field} >= {v}");
            a.compile_with_flavor(&fmt, flavor, initial)
        })
    }
    pub fn ge(&self, field: &str, value: impl Into<Arg>) -> String {
        self.greater_equal_than(field, value)
    }
    pub fn gte(&self, field: &str, value: impl Into<Arg>) -> String {
        self.greater_equal_than(field, value)
    }

    pub fn less_than(&self, field: &str, value: impl Into<Arg>) -> String {
        if field.is_empty() {
            return String::new();
        }
        let field = field.to_string();
        let value: Arg = value.into();
        self.expr_builder(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let v = a.add(value.clone());
            let fmt = format!("{field} < {v}");
            a.compile_with_flavor(&fmt, flavor, initial)
        })
    }
    pub fn l(&self, field: &str, value: impl Into<Arg>) -> String {
        self.less_than(field, value)
    }
    pub fn lt(&self, field: &str, value: impl Into<Arg>) -> String {
        self.less_than(field, value)
    }

    pub fn less_equal_than(&self, field: &str, value: impl Into<Arg>) -> String {
        if field.is_empty() {
            return String::new();
        }
        let field = field.to_string();
        let value: Arg = value.into();
        self.expr_builder(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let v = a.add(value.clone());
            let fmt = format!("{field} <= {v}");
            a.compile_with_flavor(&fmt, flavor, initial)
        })
    }
    pub fn le(&self, field: &str, value: impl Into<Arg>) -> String {
        self.less_equal_than(field, value)
    }
    pub fn lte(&self, field: &str, value: impl Into<Arg>) -> String {
        self.less_equal_than(field, value)
    }

    pub fn like(&self, field: &str, value: impl Into<Arg>) -> String {
        if field.is_empty() {
            return String::new();
        }
        let field = field.to_string();
        let value: Arg = value.into();
        self.expr_builder(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let v = a.add(value.clone());
            let fmt = format!("{field} LIKE {v}");
            a.compile_with_flavor(&fmt, flavor, initial)
        })
    }

    pub fn ilike(&self, field: &str, value: impl Into<Arg>) -> String {
        if field.is_empty() {
            return String::new();
        }

        let field = field.to_string();
        let value: Arg = value.into();

        // Choose ILIKE or LOWER(...) LIKE LOWER(...) based on flavor
        let b = CondDynBuilder::new(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let v = a.add(value.clone());
            let fmt = match flavor {
                Flavor::PostgreSQL | Flavor::SQLite => format!("{} ILIKE {v}", field),
                _ => format!("LOWER({}) LIKE LOWER({v})", field),
            };
            a.compile_with_flavor(&fmt, flavor, initial)
        });
        self.var(Arg::Builder(Box::new(b)))
    }

    pub fn not_like(&self, field: &str, value: impl Into<Arg>) -> String {
        if field.is_empty() {
            return String::new();
        }
        let field = field.to_string();
        let value: Arg = value.into();
        self.expr_builder(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let v = a.add(value.clone());
            let fmt = format!("{field} NOT LIKE {v}");
            a.compile_with_flavor(&fmt, flavor, initial)
        })
    }

    pub fn not_ilike(&self, field: &str, value: impl Into<Arg>) -> String {
        if field.is_empty() {
            return String::new();
        }

        let field = field.to_string();
        let value: Arg = value.into();

        let b = CondDynBuilder::new(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let v = a.add(value.clone());
            let fmt = match flavor {
                Flavor::PostgreSQL | Flavor::SQLite => format!("{} NOT ILIKE {v}", field),
                _ => format!("LOWER({}) NOT LIKE LOWER({v})", field),
            };
            a.compile_with_flavor(&fmt, flavor, initial)
        });
        self.var(Arg::Builder(Box::new(b)))
    }

    pub fn is_null(&self, field: &str) -> String {
        if field.is_empty() {
            return String::new();
        }
        let field = field.to_string();
        self.expr_builder(move |_flavor, initial| (format!("{field} IS NULL"), initial.to_vec()))
    }

    pub fn is_not_null(&self, field: &str) -> String {
        if field.is_empty() {
            return String::new();
        }
        let field = field.to_string();
        self.expr_builder(move |_flavor, initial| {
            (format!("{field} IS NOT NULL"), initial.to_vec())
        })
    }

    pub fn between(&self, field: &str, lower: impl Into<Arg>, upper: impl Into<Arg>) -> String {
        if field.is_empty() {
            return String::new();
        }
        let field = field.to_string();
        let lower: Arg = lower.into();
        let upper: Arg = upper.into();
        self.expr_builder(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let l = a.add(lower.clone());
            let u = a.add(upper.clone());
            let fmt = format!("{field} BETWEEN {l} AND {u}");
            a.compile_with_flavor(&fmt, flavor, initial)
        })
    }

    pub fn not_between(&self, field: &str, lower: impl Into<Arg>, upper: impl Into<Arg>) -> String {
        if field.is_empty() {
            return String::new();
        }
        let field = field.to_string();
        let lower: Arg = lower.into();
        let upper: Arg = upper.into();
        self.expr_builder(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let l = a.add(lower.clone());
            let u = a.add(upper.clone());
            let fmt = format!("{field} NOT BETWEEN {l} AND {u}");
            a.compile_with_flavor(&fmt, flavor, initial)
        })
    }

    pub fn in_(&self, field: &str, values: impl IntoIterator<Item = impl Into<Arg>>) -> String {
        if field.is_empty() {
            return String::new();
        }
        let values: Vec<Arg> = values.into_iter().map(|v| v.into()).collect();
        if values.is_empty() {
            return "0 = 1".to_string();
        }
        let field = field.to_string();
        self.expr_builder(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let vals: Vec<String> = values.iter().cloned().map(|v| a.add(v)).collect();
            let fmt = format!("{field} IN ({})", vals.join(", "));
            a.compile_with_flavor(&fmt, flavor, initial)
        })
    }

    pub fn not_in(&self, field: &str, values: impl IntoIterator<Item = impl Into<Arg>>) -> String {
        if field.is_empty() {
            return String::new();
        }
        let values: Vec<Arg> = values.into_iter().map(|v| v.into()).collect();
        if values.is_empty() {
            return "0 = 0".to_string();
        }
        let field = field.to_string();
        self.expr_builder(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let vals: Vec<String> = values.iter().cloned().map(|v| a.add(v)).collect();
            let fmt = format!("{field} NOT IN ({})", vals.join(", "));
            a.compile_with_flavor(&fmt, flavor, initial)
        })
    }

    pub fn or<T>(&self, exprs: T) -> String
    where
        T: IntoStrings,
    {
        let exprs = filter_empty_strings(collect_into_strings(exprs));
        if exprs.is_empty() {
            return String::new();
        }
        let mut buf = StringBuilder::new();
        buf.write_str("(");
        buf.write_strings(&exprs, " OR ");
        buf.write_str(")");
        buf.into_string()
    }

    pub fn and<T>(&self, exprs: T) -> String
    where
        T: IntoStrings,
    {
        let exprs = filter_empty_strings(collect_into_strings(exprs));
        if exprs.is_empty() {
            return String::new();
        }
        let mut buf = StringBuilder::new();
        buf.write_str("(");
        buf.write_strings(&exprs, " AND ");
        buf.write_str(")");
        buf.into_string()
    }

    pub fn not(&self, expr: impl Into<String>) -> String {
        let expr = expr.into();
        if expr.is_empty() {
            return String::new();
        }
        format!("NOT {expr}")
    }

    pub fn exists(&self, subquery: impl Into<Arg>) -> String {
        let subquery: Arg = subquery.into();
        self.expr_builder(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let v = a.add(subquery.clone());
            let fmt = format!("EXISTS ({v})");
            a.compile_with_flavor(&fmt, flavor, initial)
        })
    }

    pub fn not_exists(&self, subquery: impl Into<Arg>) -> String {
        let subquery: Arg = subquery.into();
        self.expr_builder(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let v = a.add(subquery.clone());
            let fmt = format!("NOT EXISTS ({v})");
            a.compile_with_flavor(&fmt, flavor, initial)
        })
    }

    pub fn any(
        &self,
        field: &str,
        op: &str,
        values: impl IntoIterator<Item = impl Into<Arg>>,
    ) -> String {
        if field.is_empty() || op.is_empty() {
            return String::new();
        }
        let values: Vec<Arg> = values.into_iter().map(|v| v.into()).collect();
        if values.is_empty() {
            return "0 = 1".to_string();
        }
        let field = field.to_string();
        let op = op.to_string();
        self.expr_builder(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let vals: Vec<String> = values.iter().cloned().map(|v| a.add(v)).collect();
            let fmt = format!("{field} {op} ANY ({})", vals.join(", "));
            a.compile_with_flavor(&fmt, flavor, initial)
        })
    }

    pub fn all(
        &self,
        field: &str,
        op: &str,
        values: impl IntoIterator<Item = impl Into<Arg>>,
    ) -> String {
        if field.is_empty() || op.is_empty() {
            return String::new();
        }
        let values: Vec<Arg> = values.into_iter().map(|v| v.into()).collect();
        if values.is_empty() {
            return "0 = 1".to_string();
        }
        let field = field.to_string();
        let op = op.to_string();
        self.expr_builder(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let vals: Vec<String> = values.iter().cloned().map(|v| a.add(v)).collect();
            let fmt = format!("{field} {op} ALL ({})", vals.join(", "));
            a.compile_with_flavor(&fmt, flavor, initial)
        })
    }

    pub fn some(
        &self,
        field: &str,
        op: &str,
        values: impl IntoIterator<Item = impl Into<Arg>>,
    ) -> String {
        if field.is_empty() || op.is_empty() {
            return String::new();
        }
        let values: Vec<Arg> = values.into_iter().map(|v| v.into()).collect();
        if values.is_empty() {
            return "0 = 1".to_string();
        }
        let field = field.to_string();
        let op = op.to_string();
        self.expr_builder(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let vals: Vec<String> = values.iter().cloned().map(|v| a.add(v)).collect();
            let fmt = format!("{field} {op} SOME ({})", vals.join(", "));
            a.compile_with_flavor(&fmt, flavor, initial)
        })
    }

    pub fn is_distinct_from(&self, field: &str, value: impl Into<Arg>) -> String {
        if field.is_empty() {
            return String::new();
        }

        let field = field.to_string();
        let value: Arg = value.into();

        let b = CondDynBuilder::new(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let fmt = match flavor {
                Flavor::PostgreSQL | Flavor::SQLite | Flavor::SQLServer => {
                    let v = a.add(value.clone());
                    format!("{field} IS DISTINCT FROM {v}")
                }
                Flavor::MySQL => {
                    let v = a.add(value.clone());
                    format!("NOT {field} <=> {v}")
                }
                _ => {
                    // CASE
                    //     WHEN field IS NULL AND value IS NULL THEN 0
                    //     WHEN field IS NOT NULL AND value IS NOT NULL AND field = value THEN 0
                    //     ELSE 1
                    // END = 1
                    let v1 = a.add(value.clone());
                    let v2 = a.add(value.clone());
                    let v3 = a.add(value.clone());
                    format!(
                        "CASE WHEN {field} IS NULL AND {v1} IS NULL THEN 0 WHEN {field} IS NOT NULL AND {v2} IS NOT NULL AND {field} = {v3} THEN 0 ELSE 1 END = 1"
                    )
                }
            };
            a.compile_with_flavor(&fmt, flavor, initial)
        });
        self.var(Arg::Builder(Box::new(b)))
    }

    pub fn is_not_distinct_from(&self, field: &str, value: impl Into<Arg>) -> String {
        if field.is_empty() {
            return String::new();
        }

        let field = field.to_string();
        let value: Arg = value.into();

        let b = CondDynBuilder::new(move |flavor, initial| {
            let mut a = Args {
                flavor,
                ..Args::default()
            };
            let fmt = match flavor {
                Flavor::PostgreSQL | Flavor::SQLite | Flavor::SQLServer => {
                    let v = a.add(value.clone());
                    format!("{field} IS NOT DISTINCT FROM {v}")
                }
                Flavor::MySQL => {
                    let v = a.add(value.clone());
                    format!("{field} <=> {v}")
                }
                _ => {
                    // CASE
                    //     WHEN field IS NULL AND value IS NULL THEN 1
                    //     WHEN field IS NOT NULL AND value IS NOT NULL AND field = value THEN 1
                    //     ELSE 0
                    // END = 1
                    let v1 = a.add(value.clone());
                    let v2 = a.add(value.clone());
                    let v3 = a.add(value.clone());
                    format!(
                        "CASE WHEN {field} IS NULL AND {v1} IS NULL THEN 1 WHEN {field} IS NOT NULL AND {v2} IS NOT NULL AND {field} = {v3} THEN 1 ELSE 0 END = 1"
                    )
                }
            };
            a.compile_with_flavor(&fmt, flavor, initial)
        });
        self.var(Arg::Builder(Box::new(b)))
    }
}

/// Internal helper for flavor-dependent conditional expressions.
#[derive(Clone)]
struct CondDynBuilder {
    f: Rc<CondBuildFn>,
}

type CondBuildFn = dyn Fn(Flavor, &[Arg]) -> (String, Vec<Arg>);

impl CondDynBuilder {
    fn new(f: impl Fn(Flavor, &[Arg]) -> (String, Vec<Arg>) + 'static) -> Self {
        Self { f: Rc::new(f) }
    }
}

impl Default for Cond {
    fn default() -> Self {
        Self::new()
    }
}

impl Builder for CondDynBuilder {
    fn build_with_flavor(&self, flavor: Flavor, initial_arg: &[Arg]) -> (String, Vec<Arg>) {
        (self.f)(flavor, initial_arg)
    }

    fn flavor(&self) -> Flavor {
        Flavor::default()
    }
}
