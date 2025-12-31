//! WhereClause: reusable WHERE clause builder.

use crate::args::Args;
use crate::flavor::Flavor;
use crate::macros::{IntoStrings, collect_into_strings};
use crate::modifiers::{Arg, Builder};
use crate::string_builder::{StringBuilder, filter_empty_strings};
use std::cell::RefCell;
use std::rc::Rc;

pub type ArgsRef = Rc<RefCell<Args>>;
pub type WhereClauseRef = Rc<RefCell<WhereClause>>;

/// CopyWhereClause: deep clone a WhereClause.
pub fn copy_where_clause(wc: &WhereClauseRef) -> WhereClauseRef {
    Rc::new(RefCell::new(wc.borrow().clone()))
}

#[derive(Debug, Clone)]
struct Clause {
    args: ArgsRef,
    and_exprs: Vec<String>,
}

impl Clause {
    fn build(&self, flavor: Flavor, initial: &[Arg]) -> (String, Vec<Arg>) {
        let exprs = filter_empty_strings(self.and_exprs.clone());
        if exprs.is_empty() {
            return (String::new(), initial.to_vec());
        }
        let mut buf = StringBuilder::new();
        buf.write_strings(&exprs, " AND ");
        self.args
            .borrow()
            .compile_with_flavor(&buf.into_string(), flavor, initial)
    }
}

/// WhereClause: shareable but not thread-safe.
#[derive(Debug, Default, Clone)]
pub struct WhereClause {
    flavor: Flavor,
    clauses: Vec<Clause>,
}

impl WhereClause {
    pub fn new() -> WhereClauseRef {
        Rc::new(RefCell::new(Self::default()))
    }

    pub fn set_flavor(&mut self, flavor: Flavor) -> Flavor {
        let old = self.flavor;
        self.flavor = flavor;
        old
    }

    pub fn flavor(&self) -> Flavor {
        self.flavor
    }

    /// AddWhereExpr: append AND expressions to the where clause (same ArgsRef merges into one clause).
    pub fn add_where_expr<T>(&mut self, args: ArgsRef, exprs: T)
    where
        T: IntoStrings,
    {
        let exprs = collect_into_strings(exprs);
        if exprs.is_empty() || exprs.iter().all(|s| s.is_empty()) {
            return;
        }

        if let Some(last) = self.clauses.last_mut()
            && Rc::ptr_eq(&last.args, &args)
        {
            last.and_exprs.extend(exprs);
            return;
        }

        self.clauses.push(Clause {
            args,
            and_exprs: exprs,
        });
    }

    pub fn add_where_clause(&mut self, other: &WhereClause) {
        self.clauses.extend(other.clauses.clone());
    }
}

/// WhereClause as a Builder: builds `WHERE ...`.
#[derive(Clone)]
pub struct WhereClauseBuilder {
    wc: WhereClauseRef,
}

impl WhereClauseBuilder {
    pub fn new(wc: WhereClauseRef) -> Self {
        Self { wc }
    }
}

impl Builder for WhereClauseBuilder {
    fn build_with_flavor(&self, flavor: Flavor, initial_arg: &[Arg]) -> (String, Vec<Arg>) {
        let wc = self.wc.borrow();
        if wc.clauses.is_empty() {
            return (String::new(), initial_arg.to_vec());
        }

        let mut buf = StringBuilder::new();
        buf.write_str("WHERE ");

        let (sql0, args0) = wc.clauses[0].build(flavor, initial_arg);
        buf.write_str(&sql0);
        let mut args = args0;

        for clause in &wc.clauses[1..] {
            buf.write_str(" AND ");
            let (s, a) = clause.build(flavor, &args);
            buf.write_str(&s);
            args = a;
        }

        (buf.into_string(), args)
    }

    fn flavor(&self) -> Flavor {
        self.wc.borrow().flavor
    }
}
