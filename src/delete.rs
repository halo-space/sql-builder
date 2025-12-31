//! DeleteBuilder: build DELETE statements.

use crate::args::Args;
use crate::cond::{ArgsRef, Cond};
use crate::cte::CTEBuilder;
use crate::flavor::Flavor;
use crate::injection::{Injection, InjectionMarker};
use crate::macros::{IntoStrings, collect_into_strings};
use crate::modifiers::{Arg, Builder};
use crate::string_builder::StringBuilder;
use crate::where_clause::{WhereClause, WhereClauseBuilder, WhereClauseRef};
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

const DELETE_MARKER_INIT: InjectionMarker = 0;
const DELETE_MARKER_AFTER_WITH: InjectionMarker = 1;
const DELETE_MARKER_AFTER_DELETE_FROM: InjectionMarker = 2;
const DELETE_MARKER_AFTER_WHERE: InjectionMarker = 3;
const DELETE_MARKER_AFTER_ORDER_BY: InjectionMarker = 4;
const DELETE_MARKER_AFTER_LIMIT: InjectionMarker = 5;
const DELETE_MARKER_AFTER_RETURNING: InjectionMarker = 6;

#[derive(Debug)]
pub struct DeleteBuilder {
    args: ArgsRef,
    cond: Cond,

    tables: Vec<String>,

    where_clause: Option<WhereClauseRef>,
    where_var: Option<String>,
    cte_var: Option<String>,
    cte: Option<CTEBuilder>,

    order_by_cols: Vec<String>,
    order: Option<&'static str>,
    limit_var: Option<String>,
    returning: Vec<String>,

    injection: Injection,
    marker: InjectionMarker,
}

impl Deref for DeleteBuilder {
    type Target = Cond;
    fn deref(&self) -> &Self::Target {
        &self.cond
    }
}

impl Default for DeleteBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for DeleteBuilder {
    fn clone(&self) -> Self {
        self.clone_builder()
    }
}

impl DeleteBuilder {
    pub fn new() -> Self {
        let args = Rc::new(RefCell::new(Args::default()));
        let cond = Cond::with_args(args.clone());
        Self {
            args,
            cond,
            tables: Vec::new(),
            where_clause: None,
            where_var: None,
            order_by_cols: Vec::new(),
            order: None,
            limit_var: None,
            returning: Vec::new(),
            cte_var: None,
            cte: None,
            injection: Injection::new(),
            marker: DELETE_MARKER_INIT,
        }
    }

    pub fn set_flavor(&mut self, flavor: Flavor) -> Flavor {
        let mut a = self.args.borrow_mut();
        let old = a.flavor;
        a.flavor = flavor;
        old
    }

    pub fn flavor(&self) -> Flavor {
        self.args.borrow().flavor
    }

    fn table_names(&self) -> Vec<String> {
        let mut table_names = Vec::new();
        if !self.tables.is_empty() {
            table_names.extend(self.tables.clone());
        }
        if let Some(cte) = &self.cte {
            table_names.extend(cte.table_names_for_from());
        }
        table_names
    }

    pub fn with(&mut self, cte: &CTEBuilder) -> &mut Self {
        let cte_clone = cte.clone();
        let ph = self.var(Arg::Builder(Box::new(cte.clone())));
        self.cte = Some(cte_clone);
        self.cte_var = Some(ph);
        self.marker = DELETE_MARKER_AFTER_WITH;
        self
    }

    pub fn where_clause(&self) -> Option<WhereClauseRef> {
        self.where_clause.clone()
    }

    pub fn set_where_clause(&mut self, wc: Option<WhereClauseRef>) -> &mut Self {
        match wc {
            None => {
                self.where_clause = None;
                self.where_var = None;
            }
            Some(wc) => {
                if let Some(ph) = &self.where_var {
                    self.args.borrow_mut().replace(
                        ph,
                        Arg::Builder(Box::new(WhereClauseBuilder::new(wc.clone()))),
                    );
                } else {
                    let ph = self.var(Arg::Builder(Box::new(WhereClauseBuilder::new(wc.clone()))));
                    self.where_var = Some(ph);
                }
                self.where_clause = Some(wc);
            }
        }
        self
    }

    pub fn clear_where_clause(&mut self) -> &mut Self {
        self.set_where_clause(None)
    }

    pub fn clone_builder(&self) -> Self {
        let old_args = self.args.borrow().clone();
        let args = Rc::new(RefCell::new(old_args));
        let cond = Cond::with_args(args.clone());

        let mut cloned = Self {
            args,
            cond,
            tables: self.tables.clone(),
            where_clause: self.where_clause.clone(),
            where_var: self.where_var.clone(),
            cte_var: self.cte_var.clone(),
            cte: self.cte.clone(),
            order_by_cols: self.order_by_cols.clone(),
            order: self.order,
            limit_var: self.limit_var.clone(),
            returning: self.returning.clone(),
            injection: self.injection.clone(),
            marker: self.marker,
        };

        if let (Some(wc), Some(ph)) = (&self.where_clause, &self.where_var) {
            let new_wc = Rc::new(RefCell::new(wc.borrow().clone()));
            cloned.where_clause = Some(new_wc.clone());
            cloned
                .args
                .borrow_mut()
                .replace(ph, Arg::Builder(Box::new(WhereClauseBuilder::new(new_wc))));
        }

        if let (Some(cte), Some(ph)) = (&self.cte, &self.cte_var) {
            let cte_for_arg = cte.clone();
            let cte_for_field = cte_for_arg.clone();
            cloned.cte = Some(cte_for_field);
            cloned
                .args
                .borrow_mut()
                .replace(ph, Arg::Builder(Box::new(cte_for_arg)));
        }

        cloned
    }

    pub fn build(&self) -> (String, Vec<Arg>) {
        Builder::build(self)
    }

    fn var(&self, v: impl Into<Arg>) -> String {
        self.args.borrow_mut().add(v)
    }

    pub fn delete_from<T>(&mut self, tables: T) -> &mut Self
    where
        T: IntoStrings,
    {
        self.tables = collect_into_strings(tables);
        self.marker = DELETE_MARKER_AFTER_DELETE_FROM;
        self
    }

    pub fn where_<T>(&mut self, and_expr: T) -> &mut Self
    where
        T: IntoStrings,
    {
        let exprs = collect_into_strings(and_expr);
        if exprs.is_empty() || exprs.iter().all(|s| s.is_empty()) {
            return self;
        }

        if self.where_clause.is_none() {
            let wc = WhereClause::new();
            let ph = self.var(Arg::Builder(Box::new(WhereClauseBuilder::new(wc.clone()))));
            self.where_clause = Some(wc);
            self.where_var = Some(ph);
        }

        self.where_clause
            .as_ref()
            .unwrap()
            .borrow_mut()
            .add_where_expr(self.args.clone(), exprs);
        self.marker = DELETE_MARKER_AFTER_WHERE;
        self
    }

    pub fn add_where_expr<T>(&mut self, args: ArgsRef, exprs: T) -> &mut Self
    where
        T: IntoStrings,
    {
        let exprs = collect_into_strings(exprs);
        if exprs.is_empty() || exprs.iter().all(|s| s.is_empty()) {
            return self;
        }

        if self.where_clause.is_none() {
            let wc = WhereClause::new();
            let ph = self.var(Arg::Builder(Box::new(WhereClauseBuilder::new(wc.clone()))));
            self.where_clause = Some(wc);
            self.where_var = Some(ph);
        }

        self.where_clause
            .as_ref()
            .unwrap()
            .borrow_mut()
            .add_where_expr(args, exprs);
        self.marker = DELETE_MARKER_AFTER_WHERE;
        self
    }

    pub fn add_where_clause(&mut self, other: &WhereClause) -> &mut Self {
        if self.where_clause.is_none() {
            let wc = WhereClause::new();
            let ph = self.var(Arg::Builder(Box::new(WhereClauseBuilder::new(wc.clone()))));
            self.where_clause = Some(wc);
            self.where_var = Some(ph);
        }
        self.where_clause
            .as_ref()
            .unwrap()
            .borrow_mut()
            .add_where_clause(other);
        self
    }

    pub fn add_where_clause_ref(&mut self, other: &WhereClauseRef) -> &mut Self {
        if self.where_clause.is_none() {
            let wc = WhereClause::new();
            let ph = self.var(Arg::Builder(Box::new(WhereClauseBuilder::new(wc.clone()))));
            self.where_clause = Some(wc);
            self.where_var = Some(ph);
        }
        self.where_clause
            .as_ref()
            .unwrap()
            .borrow_mut()
            .add_where_clause(&other.borrow());
        self
    }

    pub fn order_by<T>(&mut self, cols: T) -> &mut Self
    where
        T: IntoStrings,
    {
        self.order_by_cols = collect_into_strings(cols);
        self.marker = DELETE_MARKER_AFTER_ORDER_BY;
        self
    }

    pub fn order_by_asc(&mut self, col: impl Into<String>) -> &mut Self {
        self.order_by_cols.push(format!("{} ASC", col.into()));
        self.marker = DELETE_MARKER_AFTER_ORDER_BY;
        self
    }

    pub fn order_by_desc(&mut self, col: impl Into<String>) -> &mut Self {
        self.order_by_cols.push(format!("{} DESC", col.into()));
        self.marker = DELETE_MARKER_AFTER_ORDER_BY;
        self
    }

    pub fn asc(&mut self) -> &mut Self {
        self.order = Some("ASC");
        self.marker = DELETE_MARKER_AFTER_ORDER_BY;
        self
    }

    pub fn desc(&mut self) -> &mut Self {
        self.order = Some("DESC");
        self.marker = DELETE_MARKER_AFTER_ORDER_BY;
        self
    }

    pub fn limit(&mut self, limit: i64) -> &mut Self {
        if limit < 0 {
            self.limit_var = None;
            return self;
        }
        self.limit_var = Some(self.var(limit));
        self.marker = DELETE_MARKER_AFTER_LIMIT;
        self
    }

    pub fn returning<T>(&mut self, cols: T) -> &mut Self
    where
        T: IntoStrings,
    {
        self.returning = collect_into_strings(cols);
        self.marker = DELETE_MARKER_AFTER_RETURNING;
        self
    }

    pub fn sql(&mut self, sql: impl Into<String>) -> &mut Self {
        self.injection.sql(self.marker, sql);
        self
    }
}

impl Builder for DeleteBuilder {
    fn build_with_flavor(&self, flavor: Flavor, initial_arg: &[Arg]) -> (String, Vec<Arg>) {
        let mut buf = StringBuilder::new();
        write_injection(&mut buf, &self.injection, DELETE_MARKER_INIT);

        if let Some(ph) = &self.cte_var {
            buf.write_leading(ph);
            write_injection(&mut buf, &self.injection, DELETE_MARKER_AFTER_WITH);
        }

        let table_names = self.table_names();
        if !table_names.is_empty() {
            buf.write_leading("DELETE FROM");
            buf.write_str(" ");
            buf.write_str(&table_names.join(", "));
        }
        write_injection(&mut buf, &self.injection, DELETE_MARKER_AFTER_DELETE_FROM);

        if flavor == Flavor::SQLServer && !self.returning.is_empty() {
            buf.write_leading("OUTPUT");
            buf.write_str(" ");
            let prefixed: Vec<String> = self
                .returning
                .iter()
                .map(|c| format!("DELETED.{c}"))
                .collect();
            buf.write_str(&prefixed.join(", "));
            write_injection(&mut buf, &self.injection, DELETE_MARKER_AFTER_RETURNING);
        }

        if let Some(ph) = &self.where_var {
            buf.write_leading(ph);
            write_injection(&mut buf, &self.injection, DELETE_MARKER_AFTER_WHERE);
        }

        if !self.order_by_cols.is_empty() {
            buf.write_leading("ORDER BY");
            buf.write_str(" ");
            buf.write_str(&self.order_by_cols.join(", "));
            if let Some(order) = self.order {
                buf.write_str(" ");
                buf.write_str(order);
            }
            write_injection(&mut buf, &self.injection, DELETE_MARKER_AFTER_ORDER_BY);
        }

        if let Some(lim) = &self.limit_var {
            buf.write_leading("LIMIT");
            buf.write_str(" ");
            buf.write_str(lim);
            write_injection(&mut buf, &self.injection, DELETE_MARKER_AFTER_LIMIT);
        }

        if (flavor == Flavor::PostgreSQL || flavor == Flavor::SQLite) && !self.returning.is_empty()
        {
            buf.write_leading("RETURNING");
            buf.write_str(" ");
            buf.write_str(&self.returning.join(", "));
            write_injection(&mut buf, &self.injection, DELETE_MARKER_AFTER_RETURNING);
        }

        self.args
            .borrow()
            .compile_with_flavor(&buf.into_string(), flavor, initial_arg)
    }

    fn flavor(&self) -> Flavor {
        self.flavor()
    }
}

fn write_injection(buf: &mut StringBuilder, inj: &Injection, marker: InjectionMarker) {
    let sqls = inj.at(marker);
    if sqls.is_empty() {
        return;
    }
    buf.write_leading("");
    buf.write_str(&sqls.join(" "));
}
