//! InsertBuilder: build INSERT statements.

use crate::args::Args;
use crate::flavor::Flavor;
use crate::injection::{Injection, InjectionMarker};
use crate::macros::{IntoStrings, collect_into_strings};
use crate::modifiers::{Arg, Builder, escape, escape_all};
use crate::select::SelectBuilder;
use crate::string_builder::StringBuilder;
use std::cell::RefCell;
use std::rc::Rc;

const INSERT_MARKER_INIT: InjectionMarker = 0;
const INSERT_MARKER_AFTER_INSERT_INTO: InjectionMarker = 1;
const INSERT_MARKER_AFTER_COLS: InjectionMarker = 2;
const INSERT_MARKER_AFTER_VALUES: InjectionMarker = 3;
const INSERT_MARKER_AFTER_SELECT: InjectionMarker = 4;
const INSERT_MARKER_AFTER_RETURNING: InjectionMarker = 5;

#[derive(Debug, Clone)]
pub struct InsertBuilder {
    verb: &'static str,
    table: Option<String>,
    cols: Vec<String>,
    values: Vec<Vec<String>>,
    returning: Vec<String>,

    args: Rc<RefCell<Args>>,

    injection: Injection,
    marker: InjectionMarker,

    // Insert-Select holder
    sb_holder: Option<String>,
}

impl Default for InsertBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl InsertBuilder {
    pub fn new() -> Self {
        Self {
            verb: "INSERT",
            table: None,
            cols: Vec::new(),
            values: Vec::new(),
            returning: Vec::new(),
            args: Rc::new(RefCell::new(Args::default())),
            injection: Injection::new(),
            marker: INSERT_MARKER_INIT,
            sb_holder: None,
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
        let mut cloned = self.clone();

        // 深拷贝 Args（避免 Rc 共享）
        let args = Rc::new(RefCell::new(self.args.borrow().clone()));
        cloned.args = args;

        cloned
    }

    pub fn build(&self) -> (String, Vec<Arg>) {
        Builder::build(self)
    }

    fn var(&self, v: impl Into<Arg>) -> String {
        self.args.borrow_mut().add(v)
    }

    pub fn insert_into(&mut self, table: &str) -> &mut Self {
        self.verb = "INSERT";
        self.table = Some(escape(table));
        self.marker = INSERT_MARKER_AFTER_INSERT_INTO;
        self
    }

    pub fn insert_ignore_into(&mut self, table: &str) -> &mut Self {
        let flavor = self.flavor();
        self.verb = flavor.prepare_insert_ignore();
        self.table = Some(escape(table));
        self.marker = INSERT_MARKER_AFTER_INSERT_INTO;

        // PostgreSQL: ON CONFLICT DO NOTHING must appear after VALUES
        if flavor == Flavor::PostgreSQL {
            self.marker = INSERT_MARKER_AFTER_VALUES;
            self.sql("ON CONFLICT DO NOTHING");
        }
        self
    }

    pub fn replace_into(&mut self, table: &str) -> &mut Self {
        self.verb = "REPLACE";
        self.table = Some(escape(table));
        self.marker = INSERT_MARKER_AFTER_INSERT_INTO;
        self
    }

    pub fn cols<T>(&mut self, cols: T) -> &mut Self
    where
        T: IntoStrings,
    {
        self.cols = escape_all(collect_into_strings(cols));
        self.marker = INSERT_MARKER_AFTER_COLS;
        self
    }

    /// Insert-Select: returns a SelectBuilder to build the SELECT part.
    pub fn select<T>(&mut self, cols: T) -> SelectBuilder
    where
        T: IntoStrings,
    {
        let mut sb = SelectBuilder::new();
        sb.select(cols);
        sb.set_flavor(self.flavor());
        let ph = self.var(Arg::Builder(Box::new(sb.clone_builder())));
        self.sb_holder = Some(ph);
        sb
    }

    pub fn values(&mut self, values: impl IntoIterator<Item = impl Into<Arg>>) -> &mut Self {
        let placeholders: Vec<String> = values.into_iter().map(|v| self.var(v.into())).collect();
        self.values.push(placeholders);
        self.marker = INSERT_MARKER_AFTER_VALUES;
        self
    }

    pub fn returning<T>(&mut self, cols: T) -> &mut Self
    where
        T: IntoStrings,
    {
        self.returning = collect_into_strings(cols);
        self.marker = INSERT_MARKER_AFTER_RETURNING;
        self
    }

    pub fn sql(&mut self, sql: impl Into<String>) -> &mut Self {
        self.injection.sql(self.marker, sql);
        self
    }
}

impl Builder for InsertBuilder {
    fn build_with_flavor(&self, flavor: Flavor, initial_arg: &[Arg]) -> (String, Vec<Arg>) {
        let mut buf = StringBuilder::new();
        write_injection(&mut buf, &self.injection, INSERT_MARKER_INIT);

        // Oracle multi-values: INSERT ALL ... INTO ... VALUES (...) ... SELECT 1 from DUAL
        if flavor == Flavor::Oracle && self.values.len() > 1 {
            buf.write_leading(self.verb);
            buf.write_str(" ALL");

            for row in &self.values {
                if let Some(t) = &self.table {
                    buf.write_str(" INTO ");
                    buf.write_str(t);
                }
                write_injection(&mut buf, &self.injection, INSERT_MARKER_AFTER_INSERT_INTO);

                if !self.cols.is_empty() {
                    buf.write_str(" (");
                    buf.write_str(&self.cols.join(", "));
                    buf.write_str(")");
                    write_injection(&mut buf, &self.injection, INSERT_MARKER_AFTER_COLS);
                }

                buf.write_str(" VALUES (");
                buf.write_str(&row.join(", "));
                buf.write_str(")");
            }

            buf.write_str(" SELECT 1 from DUAL");
            write_injection(&mut buf, &self.injection, INSERT_MARKER_AFTER_VALUES);
            return self
                .args
                .borrow()
                .compile_with_flavor(&buf.into_string(), flavor, initial_arg);
        }

        if let Some(t) = &self.table {
            buf.write_leading(self.verb);
            buf.write_str(" INTO ");
            buf.write_str(t);
        }
        write_injection(&mut buf, &self.injection, INSERT_MARKER_AFTER_INSERT_INTO);

        if !self.cols.is_empty() {
            buf.write_str(" (");
            buf.write_str(&self.cols.join(", "));
            buf.write_str(")");
            write_injection(&mut buf, &self.injection, INSERT_MARKER_AFTER_COLS);
        }

        if flavor == Flavor::SQLServer && !self.returning.is_empty() {
            buf.write_str(" OUTPUT ");
            let prefixed: Vec<String> = self
                .returning
                .iter()
                .map(|c| format!("INSERTED.{c}"))
                .collect();
            buf.write_str(&prefixed.join(", "));
            write_injection(&mut buf, &self.injection, INSERT_MARKER_AFTER_RETURNING);
        }

        if let Some(sb) = &self.sb_holder {
            buf.write_str(" ");
            buf.write_str(sb);
            write_injection(&mut buf, &self.injection, INSERT_MARKER_AFTER_SELECT);
        } else if !self.values.is_empty() {
            buf.write_leading("VALUES");
            buf.write_str(" ");
            let rows: Vec<String> = self
                .values
                .iter()
                .map(|r| format!("({})", r.join(", ")))
                .collect();
            buf.write_str(&rows.join(", "));
        }

        write_injection(&mut buf, &self.injection, INSERT_MARKER_AFTER_VALUES);

        if (flavor == Flavor::PostgreSQL || flavor == Flavor::SQLite) && !self.returning.is_empty()
        {
            buf.write_leading("RETURNING");
            buf.write_str(" ");
            buf.write_str(&self.returning.join(", "));
            write_injection(&mut buf, &self.injection, INSERT_MARKER_AFTER_RETURNING);
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
