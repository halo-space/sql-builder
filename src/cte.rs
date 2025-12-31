//! CTEBuilder: build WITH / WITH RECURSIVE clauses.

use crate::args::Args;
use crate::cte_query::CTEQueryBuilder;
use crate::delete::DeleteBuilder;
use crate::flavor::Flavor;
use crate::injection::{Injection, InjectionMarker};
use crate::macros::IntoStrings;
use crate::modifiers::{Arg, Builder};
use crate::select::SelectBuilder;
use crate::string_builder::StringBuilder;
use crate::update::UpdateBuilder;
use std::cell::RefCell;
use std::rc::Rc;

const CTE_MARKER_INIT: InjectionMarker = 0;
const CTE_MARKER_AFTER_WITH: InjectionMarker = 1;

/// `with` creates a CTEBuilder for non-recursive CTEs.
pub fn with(queries: impl IntoIterator<Item = CTEQueryBuilder>) -> CTEBuilder {
    let mut builder = CTEBuilder::new();
    builder.with(queries);
    builder
}

/// `with_recursive` creates a CTEBuilder for recursive CTEs.
pub fn with_recursive(queries: impl IntoIterator<Item = CTEQueryBuilder>) -> CTEBuilder {
    let mut builder = CTEBuilder::new();
    builder.with_recursive(queries);
    builder
}

#[derive(Debug)]
pub struct CTEBuilder {
    recursive: bool,
    queries: Vec<CTEQueryBuilder>,
    query_vars: Vec<String>,

    args: Rc<RefCell<Args>>,
    injection: Injection,
    marker: InjectionMarker,
}

impl Default for CTEBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for CTEBuilder {
    fn clone(&self) -> Self {
        self.clone_builder()
    }
}

impl CTEBuilder {
    pub fn new() -> Self {
        Self {
            recursive: false,
            queries: Vec::new(),
            query_vars: Vec::new(),
            args: Rc::new(RefCell::new(Args::default())),
            injection: Injection::new(),
            marker: CTE_MARKER_INIT,
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
        let args = Rc::new(RefCell::new(self.args.borrow().clone()));
        let cloned = Self {
            recursive: self.recursive,
            queries: self.queries.clone(),
            query_vars: self.query_vars.clone(),
            args,
            injection: self.injection.clone(),
            marker: self.marker,
        };

        // Rebind query_vars builders to keep args consistent after deep clone
        for (ph, q) in cloned.query_vars.iter().zip(cloned.queries.iter()) {
            cloned
                .args
                .borrow_mut()
                .replace(ph, Arg::Builder(Box::new(q.clone_builder())));
        }

        cloned
    }

    fn var(&self, v: impl Into<Arg>) -> String {
        self.args.borrow_mut().add(v)
    }

    pub fn with(&mut self, queries: impl IntoIterator<Item = CTEQueryBuilder>) -> &mut Self {
        self.queries = queries.into_iter().collect();
        self.query_vars = self
            .queries
            .iter()
            .map(|q| self.var(Arg::Builder(Box::new(q.clone_builder()))))
            .collect();
        self.marker = CTE_MARKER_AFTER_WITH;
        self
    }

    pub fn with_recursive(
        &mut self,
        queries: impl IntoIterator<Item = CTEQueryBuilder>,
    ) -> &mut Self {
        self.with(queries);
        self.recursive = true;
        self
    }

    pub fn select<T>(&self, cols: T) -> SelectBuilder
    where
        T: IntoStrings,
    {
        let mut sb = SelectBuilder::new();
        sb.set_flavor(self.flavor());
        sb.with(self);
        sb.select(cols);
        sb
    }

    pub fn delete_from(&self, tables: impl IntoStrings) -> DeleteBuilder {
        let mut db = DeleteBuilder::new();
        db.set_flavor(self.flavor());
        db.with(self);
        db.delete_from(tables);
        db
    }

    pub fn update<T>(&self, tables: T) -> UpdateBuilder
    where
        T: IntoStrings,
    {
        let mut ub = UpdateBuilder::new();
        ub.set_flavor(self.flavor());
        ub.with(self);
        ub.update(tables);
        ub
    }

    pub fn sql(&mut self, sql: impl Into<String>) -> &mut Self {
        self.injection.sql(self.marker, sql);
        self
    }

    pub fn table_names(&self) -> Vec<String> {
        self.queries
            .iter()
            .filter_map(|q| q.table_name().map(|s| s.to_string()))
            .collect()
    }

    #[allow(dead_code)]
    pub(crate) fn table_names_for_from(&self) -> Vec<String> {
        self.queries
            .iter()
            .filter(|q| q.should_add_to_table_list())
            .filter_map(|q| q.table_name().map(|s| s.to_string()))
            .collect()
    }
}

impl Builder for CTEBuilder {
    fn build_with_flavor(&self, flavor: Flavor, initial_arg: &[Arg]) -> (String, Vec<Arg>) {
        let mut buf = StringBuilder::new();
        write_injection(&mut buf, &self.injection, CTE_MARKER_INIT);

        if !self.query_vars.is_empty() {
            buf.write_leading("WITH");
            if self.recursive {
                buf.write_str(" RECURSIVE");
            }
            buf.write_str(" ");
            buf.write_str(&self.query_vars.join(", "));
        }

        write_injection(&mut buf, &self.injection, CTE_MARKER_AFTER_WITH);
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
