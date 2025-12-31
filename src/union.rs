//! UnionBuilder: build UNION / UNION ALL statements.

use crate::args::Args;
use crate::flavor::Flavor;
use crate::injection::{Injection, InjectionMarker};
use crate::macros::{IntoStrings, collect_into_strings};
use crate::modifiers::{Arg, Builder};
use crate::string_builder::StringBuilder;
use std::cell::RefCell;
use std::rc::Rc;

const UNION_DISTINCT: &str = " UNION ";
const UNION_ALL: &str = " UNION ALL ";

const UNION_MARKER_INIT: InjectionMarker = 0;
const UNION_MARKER_AFTER_UNION: InjectionMarker = 1;
const UNION_MARKER_AFTER_ORDER_BY: InjectionMarker = 2;
const UNION_MARKER_AFTER_LIMIT: InjectionMarker = 3;

#[derive(Debug)]
pub struct UnionBuilder {
    opt: &'static str,
    order_by_cols: Vec<String>,
    order: Option<&'static str>,
    limit_var: Option<String>,
    offset_var: Option<String>,

    builder_vars: Vec<String>,
    args: Rc<RefCell<Args>>,

    injection: Injection,
    marker: InjectionMarker,
}

impl Default for UnionBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for UnionBuilder {
    fn clone(&self) -> Self {
        self.clone_builder()
    }
}

impl UnionBuilder {
    pub fn new() -> Self {
        Self {
            opt: UNION_DISTINCT,
            order_by_cols: Vec::new(),
            order: None,
            limit_var: None,
            offset_var: None,
            builder_vars: Vec::new(),
            args: Rc::new(RefCell::new(Args::default())),
            injection: Injection::new(),
            marker: UNION_MARKER_INIT,
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

    pub fn clone_builder(&self) -> Self {
        Self {
            opt: self.opt,
            order_by_cols: self.order_by_cols.clone(),
            order: self.order,
            limit_var: self.limit_var.clone(),
            offset_var: self.offset_var.clone(),
            builder_vars: self.builder_vars.clone(),
            args: Rc::new(RefCell::new(self.args.borrow().clone())),
            injection: self.injection.clone(),
            marker: self.marker,
        }
    }

    fn var(&self, v: impl Into<Arg>) -> String {
        self.args.borrow_mut().add(v)
    }

    pub fn union(
        &mut self,
        builders: impl IntoIterator<Item = impl Builder + 'static>,
    ) -> &mut Self {
        self.union_impl(UNION_DISTINCT, builders)
    }

    pub fn union_all(
        &mut self,
        builders: impl IntoIterator<Item = impl Builder + 'static>,
    ) -> &mut Self {
        self.union_impl(UNION_ALL, builders)
    }

    fn union_impl(
        &mut self,
        opt: &'static str,
        builders: impl IntoIterator<Item = impl Builder + 'static>,
    ) -> &mut Self {
        self.opt = opt;
        self.builder_vars = builders
            .into_iter()
            .map(|b| self.var(Arg::Builder(Box::new(b))))
            .collect();
        self.marker = UNION_MARKER_AFTER_UNION;
        self
    }

    pub fn order_by<T>(&mut self, cols: T) -> &mut Self
    where
        T: IntoStrings,
    {
        self.order_by_cols = collect_into_strings(cols);
        self.marker = UNION_MARKER_AFTER_ORDER_BY;
        self
    }

    pub fn order_by_asc(&mut self, col: impl Into<String>) -> &mut Self {
        self.order_by_cols.push(format!("{} ASC", col.into()));
        self.marker = UNION_MARKER_AFTER_ORDER_BY;
        self
    }

    pub fn order_by_desc(&mut self, col: impl Into<String>) -> &mut Self {
        self.order_by_cols.push(format!("{} DESC", col.into()));
        self.marker = UNION_MARKER_AFTER_ORDER_BY;
        self
    }

    pub fn asc(&mut self) -> &mut Self {
        self.order = Some("ASC");
        self.marker = UNION_MARKER_AFTER_ORDER_BY;
        self
    }

    pub fn desc(&mut self) -> &mut Self {
        self.order = Some("DESC");
        self.marker = UNION_MARKER_AFTER_ORDER_BY;
        self
    }

    pub fn limit(&mut self, limit: i64) -> &mut Self {
        if limit < 0 {
            self.limit_var = None;
            return self;
        }
        self.limit_var = Some(self.var(limit));
        self.marker = UNION_MARKER_AFTER_LIMIT;
        self
    }

    pub fn offset(&mut self, offset: i64) -> &mut Self {
        if offset < 0 {
            self.offset_var = None;
            return self;
        }
        self.offset_var = Some(self.var(offset));
        self.marker = UNION_MARKER_AFTER_LIMIT;
        self
    }

    pub fn sql(&mut self, sql: impl Into<String>) -> &mut Self {
        self.injection.sql(self.marker, sql);
        self
    }
}

impl Builder for UnionBuilder {
    fn build_with_flavor(&self, flavor: Flavor, initial_arg: &[Arg]) -> (String, Vec<Arg>) {
        let mut buf = StringBuilder::new();
        write_injection(&mut buf, &self.injection, UNION_MARKER_INIT);

        let nested_select = (flavor == Flavor::Oracle
            && (self.limit_var.is_some() || self.offset_var.is_some()))
            || (flavor == Flavor::Informix && self.limit_var.is_some());

        if !self.builder_vars.is_empty() {
            let need_paren = flavor != Flavor::SQLite;

            if nested_select {
                buf.write_leading("SELECT * FROM (");
            }

            // first
            if need_paren {
                buf.write_leading("(");
                buf.write_str(&self.builder_vars[0]);
                buf.write_str(")");
            } else {
                buf.write_leading(&self.builder_vars[0]);
            }

            for b in self.builder_vars.iter().skip(1) {
                buf.write_str(self.opt);
                if need_paren {
                    buf.write_str("(");
                }
                buf.write_str(b);
                if need_paren {
                    buf.write_str(")");
                }
            }

            if nested_select {
                buf.write_leading(")");
            }
        }

        write_injection(&mut buf, &self.injection, UNION_MARKER_AFTER_UNION);

        if !self.order_by_cols.is_empty() {
            buf.write_leading("ORDER BY");
            buf.write_str(" ");
            buf.write_str(&self.order_by_cols.join(", "));
            if let Some(order) = self.order {
                buf.write_str(" ");
                buf.write_str(order);
            }
            write_injection(&mut buf, &self.injection, UNION_MARKER_AFTER_ORDER_BY);
        }

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
            Flavor::SQLServer => {
                if self.order_by_cols.is_empty()
                    && (self.limit_var.is_some() || self.offset_var.is_some())
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
            Flavor::Oracle => {
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
            Flavor::Informix => {
                // Informix:
                // - offset 无 limit 时忽略
                // - limit/offset 使用 `SKIP ? FIRST ?`
                if let Some(lim) = &self.limit_var {
                    if let Some(off) = &self.offset_var {
                        buf.write_leading("SKIP");
                        buf.write_str(" ");
                        buf.write_str(off);
                        buf.write_leading("FIRST");
                        buf.write_str(" ");
                        buf.write_str(lim);
                    } else {
                        buf.write_leading("FIRST");
                        buf.write_str(" ");
                        buf.write_str(lim);
                    }
                }
            }
            Flavor::Doris => {
                // Doris:
                // - offset 无 limit 时忽略
                // - limit/offset 使用字面量（不参数化）
                if let Some(lim_ph) = &self.limit_var {
                    if let Some(n) = extract_i64(&self.args.borrow(), lim_ph) {
                        buf.write_leading("LIMIT");
                        buf.write_str(" ");
                        buf.write_str(&n.to_string());
                        if let Some(off_ph) = &self.offset_var
                            && let Some(off) = extract_i64(&self.args.borrow(), off_ph)
                        {
                            buf.write_leading("OFFSET");
                            buf.write_str(" ");
                            buf.write_str(&off.to_string());
                        }
                    } else {
                        // Fallback: keep placeholders.
                        buf.write_leading("LIMIT");
                        buf.write_str(" ");
                        buf.write_str(lim_ph);
                    }
                }
            }
        }

        if self.limit_var.is_some() {
            write_injection(&mut buf, &self.injection, UNION_MARKER_AFTER_LIMIT);
        }

        self.args
            .borrow()
            .compile_with_flavor(&buf.into_string(), flavor, initial_arg)
    }

    fn flavor(&self) -> Flavor {
        self.flavor()
    }
}

fn extract_i64(args: &Args, placeholder: &str) -> Option<i64> {
    let a = args.value(placeholder)?;
    match a {
        Arg::Value(crate::value::SqlValue::I64(v)) => Some(*v),
        Arg::Value(crate::value::SqlValue::U64(v)) => i64::try_from(*v).ok(),
        _ => None,
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
