//! Injection: inject extra SQL fragments at predefined markers.
#![allow(dead_code)]

use std::collections::HashMap;

pub(crate) type InjectionMarker = usize;

#[derive(Debug, Default, Clone)]
pub(crate) struct Injection {
    marker_sqls: HashMap<InjectionMarker, Vec<String>>,
}

impl Injection {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn sql(&mut self, marker: InjectionMarker, sql: impl Into<String>) {
        self.marker_sqls.entry(marker).or_default().push(sql.into());
    }

    pub(crate) fn at(&self, marker: InjectionMarker) -> &[String] {
        self.marker_sqls
            .get(&marker)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }
}
