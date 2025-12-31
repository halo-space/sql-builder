//! CreateTableBuilder: build CREATE TABLE statements.

use crate::args::Args;
use crate::flavor::Flavor;
use crate::injection::{Injection, InjectionMarker};
use crate::macros::{IntoStrings, collect_into_strings};
use crate::modifiers::{Arg, Builder, escape};
use crate::string_builder::StringBuilder;
use std::cell::RefCell;
use std::rc::Rc;

const CT_MARKER_INIT: InjectionMarker = 0;
const CT_MARKER_AFTER_CREATE: InjectionMarker = 1;
const CT_MARKER_AFTER_DEFINE: InjectionMarker = 2;
const CT_MARKER_AFTER_OPTION: InjectionMarker = 3;

#[derive(Debug, Clone)]
pub struct CreateTableBuilder {
    verb: &'static str,
    if_not_exists: bool,
    table: Option<String>,
    defs: Vec<Vec<String>>,
    options: Vec<Vec<String>>,

    args: Rc<RefCell<Args>>,
    injection: Injection,
    marker: InjectionMarker,
}

impl Default for CreateTableBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl CreateTableBuilder {
    pub fn new() -> Self {
        Self {
            verb: "CREATE TABLE",
            if_not_exists: false,
            table: None,
            defs: Vec::new(),
            options: Vec::new(),
            args: Rc::new(RefCell::new(Args::default())),
            injection: Injection::new(),
            marker: CT_MARKER_INIT,
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

    pub fn create_table(&mut self, table: &str) -> &mut Self {
        self.table = Some(escape(table));
        self.marker = CT_MARKER_AFTER_CREATE;
        self
    }

    pub fn create_temp_table(&mut self, table: &str) -> &mut Self {
        self.verb = "CREATE TEMPORARY TABLE";
        self.table = Some(escape(table));
        self.marker = CT_MARKER_AFTER_CREATE;
        self
    }

    pub fn if_not_exists(&mut self) -> &mut Self {
        self.if_not_exists = true;
        self
    }

    pub fn define<T>(&mut self, def: T) -> &mut Self
    where
        T: IntoStrings,
    {
        self.defs.push(collect_into_strings(def));
        self.marker = CT_MARKER_AFTER_DEFINE;
        self
    }

    pub fn option<T>(&mut self, opt: T) -> &mut Self
    where
        T: IntoStrings,
    {
        self.options.push(collect_into_strings(opt));
        self.marker = CT_MARKER_AFTER_OPTION;
        self
    }

    pub fn sql(&mut self, sql: impl Into<String>) -> &mut Self {
        self.injection.sql(self.marker, sql);
        self
    }

    pub fn num_define(&self) -> usize {
        self.defs.len()
    }

    // CreateTableBuilder currently doesn't need placeholders; add later if needed.
}

impl Builder for CreateTableBuilder {
    fn build_with_flavor(&self, flavor: Flavor, initial_arg: &[Arg]) -> (String, Vec<Arg>) {
        let mut buf = StringBuilder::new();
        write_injection(&mut buf, &self.injection, CT_MARKER_INIT);

        buf.write_leading(self.verb);
        if self.if_not_exists {
            buf.write_leading("IF NOT EXISTS");
        }
        if let Some(t) = &self.table {
            buf.write_leading(t);
        }
        write_injection(&mut buf, &self.injection, CT_MARKER_AFTER_CREATE);

        if !self.defs.is_empty() {
            let defs: Vec<String> = self.defs.iter().map(|d| d.join(" ")).collect();
            buf.write_leading("(");
            buf.write_str(&defs.join(", "));
            buf.write_str(")");
            write_injection(&mut buf, &self.injection, CT_MARKER_AFTER_DEFINE);
        }

        if !self.options.is_empty() {
            let opts: Vec<String> = self.options.iter().map(|o| o.join(" ")).collect();
            buf.write_leading(&opts.join(", "));
            write_injection(&mut buf, &self.injection, CT_MARKER_AFTER_OPTION);
        }

        // flavor currently only affects placeholders; CREATE TABLE typically doesn't produce placeholders
        let _ = flavor;
        self.args
            .borrow()
            .compile_with_flavor(&buf.into_string(), self.flavor(), initial_arg)
    }

    fn flavor(&self) -> Flavor {
        self.flavor()
    }
}

pub fn create_table(table: impl Into<String>) -> CreateTableBuilder {
    let mut builder = CreateTableBuilder::new();
    builder.create_table(&table.into());
    builder
}

pub fn create_temp_table(table: impl Into<String>) -> CreateTableBuilder {
    let mut builder = CreateTableBuilder::new();
    builder.create_temp_table(&table.into());
    builder
}

fn write_injection(buf: &mut StringBuilder, inj: &Injection, marker: InjectionMarker) {
    let sqls = inj.at(marker);
    if sqls.is_empty() {
        return;
    }
    buf.write_leading("");
    buf.write_str(&sqls.join(" "));
}
