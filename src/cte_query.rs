//! CTEQueryBuilder: build a single CTE entry.

use crate::args::Args;
use crate::flavor::Flavor;
use crate::injection::{Injection, InjectionMarker};
use crate::macros::{IntoStrings, collect_into_strings};
use crate::modifiers::{Arg, Builder};
use crate::string_builder::StringBuilder;
use std::cell::RefCell;
use std::rc::Rc;

const CTE_QUERY_MARKER_INIT: InjectionMarker = 0;
const CTE_QUERY_MARKER_AFTER_TABLE: InjectionMarker = 1;
const CTE_QUERY_MARKER_AFTER_AS: InjectionMarker = 2;

#[derive(Default)]
pub struct CTEQueryBuilder {
    name: Option<String>,
    cols: Vec<String>,
    builder_var: Option<String>,
    #[allow(clippy::type_complexity)]
    builder: Option<Box<dyn Builder>>,
    auto_add_to_table_list: bool,

    args: Rc<RefCell<Args>>,
    injection: Injection,
    marker: InjectionMarker,
}

// Box<dyn Builder> cannot auto-derive Debug; keep Debug off to avoid failures.
impl std::fmt::Debug for CTEQueryBuilder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CTEQueryBuilder")
            .field("name", &self.name)
            .field("cols", &self.cols)
            .field("builder_var", &self.builder_var)
            .field("auto_add_to_table_list", &self.auto_add_to_table_list)
            .finish()
    }
}

impl Clone for CTEQueryBuilder {
    fn clone(&self) -> Self {
        self.clone_builder()
    }
}

impl CTEQueryBuilder {
    pub fn new() -> Self {
        Self {
            name: None,
            cols: Vec::new(),
            builder_var: None,
            builder: None,
            auto_add_to_table_list: false,
            args: Rc::new(RefCell::new(Args::default())),
            injection: Injection::new(),
            marker: CTE_QUERY_MARKER_INIT,
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
        let cloned = Self {
            name: self.name.clone(),
            cols: self.cols.clone(),
            builder_var: self.builder_var.clone(),
            builder: self
                .builder
                .as_ref()
                .map(|b| dyn_clone::clone_box(b.as_ref())),
            auto_add_to_table_list: self.auto_add_to_table_list,
            args: Rc::new(RefCell::new(self.args.borrow().clone())),
            injection: self.injection.clone(),
            marker: self.marker,
        };

        if let (Some(ph), Some(b)) = (&self.builder_var, &self.builder) {
            cloned
                .args
                .borrow_mut()
                .replace(ph, Arg::Builder(dyn_clone::clone_box(b.as_ref())));
        }

        cloned
    }

    fn var(&self, v: impl Into<Arg>) -> String {
        self.args.borrow_mut().add(v)
    }

    pub fn table<T>(&mut self, name: impl Into<String>, cols: T) -> &mut Self
    where
        T: IntoStrings,
    {
        self.name = Some(name.into());
        self.cols = collect_into_strings(cols);
        self.marker = CTE_QUERY_MARKER_AFTER_TABLE;
        self
    }

    pub fn as_(&mut self, builder: impl Builder + 'static) -> &mut Self {
        let b: Box<dyn Builder> = Box::new(builder);
        let ph = self.var(Arg::Builder(dyn_clone::clone_box(b.as_ref())));
        self.builder = Some(b);
        self.builder_var = Some(ph);
        self.marker = CTE_QUERY_MARKER_AFTER_AS;
        self
    }

    pub fn add_to_table_list(&mut self) -> &mut Self {
        self.auto_add_to_table_list = true;
        self
    }

    pub fn should_add_to_table_list(&self) -> bool {
        self.auto_add_to_table_list
    }

    pub fn table_name(&self) -> Option<&str> {
        self.name.as_deref()
    }

    pub fn sql(&mut self, sql: impl Into<String>) -> &mut Self {
        self.injection.sql(self.marker, sql);
        self
    }
}

impl Builder for CTEQueryBuilder {
    fn build_with_flavor(&self, flavor: Flavor, initial_arg: &[Arg]) -> (String, Vec<Arg>) {
        let mut buf = StringBuilder::new();
        write_injection(&mut buf, &self.injection, CTE_QUERY_MARKER_INIT);

        if let Some(name) = &self.name {
            buf.write_leading(name);
            if !self.cols.is_empty() {
                buf.write_str(" (");
                buf.write_str(&self.cols.join(", "));
                buf.write_str(")");
            }
            write_injection(&mut buf, &self.injection, CTE_QUERY_MARKER_AFTER_TABLE);
        }

        if let Some(ph) = &self.builder_var {
            buf.write_leading("AS (");
            buf.write_str(ph);
            buf.write_str(")");
            write_injection(&mut buf, &self.injection, CTE_QUERY_MARKER_AFTER_AS);
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
