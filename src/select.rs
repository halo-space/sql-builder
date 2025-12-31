//! SelectBuilder: build SELECT statements with composable clauses.

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

const SELECT_MARKER_INIT: InjectionMarker = 0;
const SELECT_MARKER_AFTER_WITH: InjectionMarker = 1;
const SELECT_MARKER_AFTER_SELECT: InjectionMarker = 2;
const SELECT_MARKER_AFTER_FROM: InjectionMarker = 3;
const SELECT_MARKER_AFTER_JOIN: InjectionMarker = 4;
const SELECT_MARKER_AFTER_WHERE: InjectionMarker = 5;
const SELECT_MARKER_AFTER_GROUP_BY: InjectionMarker = 6;
const SELECT_MARKER_AFTER_ORDER_BY: InjectionMarker = 7;
const SELECT_MARKER_AFTER_LIMIT: InjectionMarker = 8;
const SELECT_MARKER_AFTER_FOR: InjectionMarker = 9;

/// JoinOption variants for explicit join keywords.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JoinOption {
    FullJoin,
    FullOuterJoin,
    InnerJoin,
    LeftJoin,
    LeftOuterJoin,
    RightJoin,
    RightOuterJoin,
}

impl JoinOption {
    fn as_str(self) -> &'static str {
        match self {
            Self::FullJoin => "FULL",
            Self::FullOuterJoin => "FULL OUTER",
            Self::InnerJoin => "INNER",
            Self::LeftJoin => "LEFT",
            Self::LeftOuterJoin => "LEFT OUTER",
            Self::RightJoin => "RIGHT",
            Self::RightOuterJoin => "RIGHT OUTER",
        }
    }
}

#[derive(Debug)]
pub struct SelectBuilder {
    args: ArgsRef,
    cond: Cond,

    distinct: bool,
    tables: Vec<String>,
    select_cols: Vec<String>,

    join_options: Vec<Option<JoinOption>>,
    join_tables: Vec<String>,
    join_exprs: Vec<Vec<String>>,

    where_clause: Option<WhereClauseRef>,
    where_var: Option<String>,
    cte_var: Option<String>,
    cte: Option<CTEBuilder>,

    having_exprs: Vec<String>,
    group_by_cols: Vec<String>,
    order_by_cols: Vec<String>,
    order: Option<&'static str>,
    limit_var: Option<String>,
    offset_var: Option<String>,
    for_what: Option<&'static str>,

    injection: Injection,
    marker: InjectionMarker,
}

impl Deref for SelectBuilder {
    type Target = Cond;
    fn deref(&self) -> &Self::Target {
        &self.cond
    }
}

impl SelectBuilder {
    pub fn new() -> Self {
        let args = Rc::new(RefCell::new(Args::default()));
        let cond = Cond::with_args(args.clone());
        Self {
            args,
            cond,
            distinct: false,
            tables: Vec::new(),
            select_cols: Vec::new(),
            join_options: Vec::new(),
            join_tables: Vec::new(),
            join_exprs: Vec::new(),
            where_clause: None,
            where_var: None,
            cte_var: None,
            cte: None,
            having_exprs: Vec::new(),
            group_by_cols: Vec::new(),
            order_by_cols: Vec::new(),
            order: None,
            limit_var: None,
            offset_var: None,
            for_what: None,
            injection: Injection::new(),
            marker: SELECT_MARKER_INIT,
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

    pub fn with(&mut self, cte: &CTEBuilder) -> &mut Self {
        let placeholder_builder = cte.clone();
        let ph = self.var(Arg::Builder(Box::new(cte.clone())));
        self.cte = Some(placeholder_builder);
        self.cte_var = Some(ph);
        self.marker = SELECT_MARKER_AFTER_WITH;
        self
    }

    fn table_names(&self) -> Vec<String> {
        let cte_tables = self
            .cte
            .as_ref()
            .map(|cte| cte.table_names_for_from())
            .unwrap_or_default();

        if self.tables.is_empty() {
            return cte_tables;
        }

        if cte_tables.is_empty() {
            return self.tables.clone();
        }

        let mut out = Vec::with_capacity(self.tables.len() + cte_tables.len());
        out.extend(self.tables.clone());
        out.extend(cte_tables);
        out
    }

    /// Returns the current WhereClause (can be shared across builders).
    pub fn where_clause(&self) -> Option<WhereClauseRef> {
        self.where_clause.clone()
    }

    /// Set or share a WhereClause.
    ///
    /// - `None` clears the WHERE clause.
    /// - `Some(wc)` binds the WhereClause to this builder and ensures placeholders point to the right builder.
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

    /// AddWhereExpr: append expressions to the WhereClause with an explicit ArgsRef.
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
        let wc = self.where_clause.as_ref().unwrap().clone();
        wc.borrow_mut().add_where_expr(args, exprs);
        self.marker = SELECT_MARKER_AFTER_WHERE;
        self
    }

    pub fn clone_builder(&self) -> Self {
        let old_args = self.args.borrow().clone();
        let args = Rc::new(RefCell::new(old_args));
        let cond = Cond::with_args(args.clone());

        let mut cloned = Self {
            args,
            cond,
            distinct: self.distinct,
            tables: self.tables.clone(),
            select_cols: self.select_cols.clone(),
            join_options: self.join_options.clone(),
            join_tables: self.join_tables.clone(),
            join_exprs: self.join_exprs.clone(),
            where_clause: self.where_clause.clone(),
            where_var: self.where_var.clone(),
            cte_var: self.cte_var.clone(),
            cte: self.cte.clone(),
            having_exprs: self.having_exprs.clone(),
            group_by_cols: self.group_by_cols.clone(),
            order_by_cols: self.order_by_cols.clone(),
            order: self.order,
            limit_var: self.limit_var.clone(),
            offset_var: self.offset_var.clone(),
            for_what: self.for_what,
            injection: self.injection.clone(),
            marker: self.marker,
        };

        // Deep clone WhereClause and fix Builder placeholder in args to point to the new WhereClause
        if let (Some(wc), Some(ph)) = (&self.where_clause, &self.where_var) {
            let new_wc = Rc::new(RefCell::new(wc.borrow().clone()));
            cloned.where_clause = Some(new_wc.clone());
            cloned
                .args
                .borrow_mut()
                .replace(ph, Arg::Builder(Box::new(WhereClauseBuilder::new(new_wc))));
        }

        if let (Some(cte), Some(ph)) = (&self.cte, &self.cte_var) {
            let new_cte = cte.clone();
            let new_cte_for_field = new_cte.clone();
            cloned.cte = Some(new_cte_for_field);
            cloned
                .args
                .borrow_mut()
                .replace(ph, Arg::Builder(Box::new(new_cte)));
        }

        cloned
    }

    pub fn build(&self) -> (String, Vec<Arg>) {
        Builder::build(self)
    }

    fn var(&self, v: impl Into<Arg>) -> String {
        self.args.borrow_mut().add(v)
    }

    pub fn select<T>(&mut self, cols: T) -> &mut Self
    where
        T: IntoStrings,
    {
        self.select_cols = collect_into_strings(cols);
        self.marker = SELECT_MARKER_AFTER_SELECT;
        self
    }

    pub fn select_more<T>(&mut self, cols: T) -> &mut Self
    where
        T: IntoStrings,
    {
        self.select_cols.extend(collect_into_strings(cols));
        self.marker = SELECT_MARKER_AFTER_SELECT;
        self
    }

    pub fn distinct(&mut self) -> &mut Self {
        self.distinct = true;
        self.marker = SELECT_MARKER_AFTER_SELECT;
        self
    }

    pub fn from<T>(&mut self, tables: T) -> &mut Self
    where
        T: IntoStrings,
    {
        self.tables = collect_into_strings(tables);
        self.marker = SELECT_MARKER_AFTER_FROM;
        self
    }

    pub fn join(&mut self, table: impl Into<String>, on_expr: impl IntoStrings) -> &mut Self {
        self.join_with_option(None, table, on_expr)
    }

    pub fn join_with_option(
        &mut self,
        option: Option<JoinOption>,
        table: impl Into<String>,
        on_expr: impl IntoStrings,
    ) -> &mut Self {
        self.join_options.push(option);
        self.join_tables.push(table.into());
        self.join_exprs.push(collect_into_strings(on_expr));
        self.marker = SELECT_MARKER_AFTER_JOIN;
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

        let wc = self.where_clause.as_ref().unwrap().clone();
        wc.borrow_mut().add_where_expr(self.args.clone(), exprs);
        self.marker = SELECT_MARKER_AFTER_WHERE;
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

    pub fn having<T>(&mut self, and_expr: T) -> &mut Self
    where
        T: IntoStrings,
    {
        self.having_exprs.extend(collect_into_strings(and_expr));
        self.marker = SELECT_MARKER_AFTER_GROUP_BY;
        self
    }

    pub fn group_by<T>(&mut self, cols: T) -> &mut Self
    where
        T: IntoStrings,
    {
        self.group_by_cols.extend(collect_into_strings(cols));
        self.marker = SELECT_MARKER_AFTER_GROUP_BY;
        self
    }

    pub fn order_by<T>(&mut self, cols: T) -> &mut Self
    where
        T: IntoStrings,
    {
        self.order_by_cols.extend(collect_into_strings(cols));
        self.marker = SELECT_MARKER_AFTER_ORDER_BY;
        self
    }

    pub fn order_by_asc(&mut self, col: impl Into<String>) -> &mut Self {
        self.order_by_cols.push(format!("{} ASC", col.into()));
        self.marker = SELECT_MARKER_AFTER_ORDER_BY;
        self
    }

    pub fn order_by_desc(&mut self, col: impl Into<String>) -> &mut Self {
        self.order_by_cols.push(format!("{} DESC", col.into()));
        self.marker = SELECT_MARKER_AFTER_ORDER_BY;
        self
    }

    pub fn asc(&mut self) -> &mut Self {
        self.order = Some("ASC");
        self.marker = SELECT_MARKER_AFTER_ORDER_BY;
        self
    }

    pub fn desc(&mut self) -> &mut Self {
        self.order = Some("DESC");
        self.marker = SELECT_MARKER_AFTER_ORDER_BY;
        self
    }

    pub fn limit(&mut self, limit: i64) -> &mut Self {
        if limit < 0 {
            self.limit_var = None;
            return self;
        }
        self.limit_var = Some(self.var(limit));
        self.marker = SELECT_MARKER_AFTER_LIMIT;
        self
    }

    pub fn offset(&mut self, offset: i64) -> &mut Self {
        if offset < 0 {
            self.offset_var = None;
            return self;
        }
        self.offset_var = Some(self.var(offset));
        self.marker = SELECT_MARKER_AFTER_LIMIT;
        self
    }

    pub fn for_update(&mut self) -> &mut Self {
        self.for_what = Some("UPDATE");
        self.marker = SELECT_MARKER_AFTER_FOR;
        self
    }

    pub fn for_share(&mut self) -> &mut Self {
        self.for_what = Some("SHARE");
        self.marker = SELECT_MARKER_AFTER_FOR;
        self
    }

    pub fn as_(&self, name: &str, alias: &str) -> String {
        format!("{name} AS {alias}")
    }

    pub fn builder_as(&self, builder: impl Builder + 'static, alias: &str) -> String {
        format!(
            "({}) AS {}",
            self.var(Arg::Builder(Box::new(builder))),
            alias
        )
    }

    pub fn sql(&mut self, sql: impl Into<String>) -> &mut Self {
        self.injection.sql(self.marker, sql);
        self
    }
}

impl Clone for SelectBuilder {
    fn clone(&self) -> Self {
        self.clone_builder()
    }
}

impl Default for SelectBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl Builder for SelectBuilder {
    fn build_with_flavor(&self, flavor: Flavor, initial_arg: &[Arg]) -> (String, Vec<Arg>) {
        let mut buf = StringBuilder::new();
        write_injection(&mut buf, &self.injection, SELECT_MARKER_INIT);

        if let Some(ph) = &self.cte_var {
            buf.write_leading(ph);
            write_injection(&mut buf, &self.injection, SELECT_MARKER_AFTER_WITH);
        }

        if !self.select_cols.is_empty() {
            buf.write_leading("SELECT");
            if self.distinct {
                buf.write_str(" DISTINCT");
            }
            buf.write_str(" ");
            buf.write_str(&self.select_cols.join(", "));
        }
        write_injection(&mut buf, &self.injection, SELECT_MARKER_AFTER_SELECT);

        let table_names = self.table_names();
        if !table_names.is_empty() {
            buf.write_leading("FROM");
            buf.write_str(" ");
            buf.write_str(&table_names.join(", "));
        }
        write_injection(&mut buf, &self.injection, SELECT_MARKER_AFTER_FROM);

        for i in 0..self.join_tables.len() {
            if let Some(opt) = self.join_options[i] {
                buf.write_leading(opt.as_str());
            }
            buf.write_leading("JOIN");
            buf.write_str(" ");
            buf.write_str(&self.join_tables[i]);

            let on = self.join_exprs[i]
                .iter()
                .filter(|s| !s.is_empty())
                .cloned()
                .collect::<Vec<_>>();
            if !on.is_empty() {
                buf.write_str(" ON ");
                buf.write_str(&on.join(" AND "));
            }
        }
        if !self.join_tables.is_empty() {
            write_injection(&mut buf, &self.injection, SELECT_MARKER_AFTER_JOIN);
        }

        if let Some(ph) = &self.where_var {
            buf.write_leading(ph);
            write_injection(&mut buf, &self.injection, SELECT_MARKER_AFTER_WHERE);
        }

        if !self.group_by_cols.is_empty() {
            buf.write_leading("GROUP BY");
            buf.write_str(" ");
            buf.write_str(&self.group_by_cols.join(", "));
            let having = self
                .having_exprs
                .iter()
                .filter(|s| !s.is_empty())
                .cloned()
                .collect::<Vec<_>>();
            if !having.is_empty() {
                buf.write_str(" HAVING ");
                buf.write_str(&having.join(" AND "));
            }
            write_injection(&mut buf, &self.injection, SELECT_MARKER_AFTER_GROUP_BY);
        }

        if !self.order_by_cols.is_empty() {
            buf.write_leading("ORDER BY");
            buf.write_str(" ");
            buf.write_str(&self.order_by_cols.join(", "));
            if let Some(order) = self.order {
                buf.write_str(" ");
                buf.write_str(order);
            }
            write_injection(&mut buf, &self.injection, SELECT_MARKER_AFTER_ORDER_BY);
        }

        // LIMIT/OFFSET rules follow each flavor's expected behavior
        match flavor {
            Flavor::MySQL | Flavor::SQLite | Flavor::ClickHouse => {
                if let Some(lim) = &self.limit_var {
                    buf.write_leading("LIMIT");
                    buf.write_str(" ");
                    buf.write_str(lim);
                    if let Some(off) = &self.offset_var {
                        buf.write_leading("OFFSET");
                        buf.write_str(" ");
                        buf.write_str(off);
                    }
                }
            }
            Flavor::CQL => {
                if let Some(lim) = &self.limit_var {
                    buf.write_leading("LIMIT");
                    buf.write_str(" ");
                    buf.write_str(lim);
                }
            }
            Flavor::PostgreSQL => {
                if let Some(lim) = &self.limit_var {
                    buf.write_leading("LIMIT");
                    buf.write_str(" ");
                    buf.write_str(lim);
                }
                if let Some(off) = &self.offset_var {
                    buf.write_leading("OFFSET");
                    buf.write_str(" ");
                    buf.write_str(off);
                }
            }
            Flavor::Presto => {
                if let Some(off) = &self.offset_var {
                    buf.write_leading("OFFSET");
                    buf.write_str(" ");
                    buf.write_str(off);
                }
                if let Some(lim) = &self.limit_var {
                    buf.write_leading("LIMIT");
                    buf.write_str(" ");
                    buf.write_str(lim);
                }
            }
            Flavor::SQLServer | Flavor::Oracle => {
                if self.order_by_cols.is_empty()
                    && (self.limit_var.is_some() || self.offset_var.is_some())
                    && flavor == Flavor::SQLServer
                {
                    buf.write_leading("ORDER BY 1");
                }

                if let Some(off) = &self.offset_var {
                    buf.write_leading("OFFSET");
                    buf.write_str(" ");
                    buf.write_str(off);
                    buf.write_str(" ROWS");
                }

                if let Some(lim) = &self.limit_var {
                    if self.offset_var.is_none() {
                        buf.write_leading("OFFSET 0 ROWS");
                    }
                    buf.write_leading("FETCH NEXT");
                    buf.write_str(" ");
                    buf.write_str(lim);
                    buf.write_str(" ROWS ONLY");
                }
            }
            Flavor::Informix | Flavor::Doris => {
                // Future: handle Informix/Doris specific quirks if needed
                if let Some(lim) = &self.limit_var {
                    buf.write_leading("LIMIT");
                    buf.write_str(" ");
                    buf.write_str(lim);
                    if let Some(off) = &self.offset_var {
                        buf.write_leading("OFFSET");
                        buf.write_str(" ");
                        buf.write_str(off);
                    }
                }
            }
        }

        if self.limit_var.is_some() {
            write_injection(&mut buf, &self.injection, SELECT_MARKER_AFTER_LIMIT);
        }

        if let Some(what) = self.for_what {
            buf.write_leading("FOR");
            buf.write_str(" ");
            buf.write_str(what);
            write_injection(&mut buf, &self.injection, SELECT_MARKER_AFTER_FOR);
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
