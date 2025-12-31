//! Struct: lightweight ORM-style helpers for table structs.
//!
//! Without runtime reflection (and avoiding extra proc-macro crates), this uses `macro_rules!`
//! to generate field metadata and getters, providing an experience close to reflective builders.

use crate::delete::DeleteBuilder;
use crate::escape_all;
use crate::field_mapper::{FieldMapperFunc, default_field_mapper};
use crate::flavor::Flavor;
use crate::insert::InsertBuilder;
use crate::select::SelectBuilder;
use crate::select_cols;
use crate::update::UpdateBuilder;
use std::any::Any;
use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldOpt {
    WithQuote,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldMeta {
    /// Rust field name (used to generate value accessors).
    pub rust: &'static str,
    /// Original field name used by the FieldMapper (macro-generated; defaults to `rust` but can override CamelCase).
    pub orig: &'static str,
    /// SQL column name (after db tag/mapper).
    pub db: &'static str,
    /// Optional alias (AS).
    pub as_: Option<&'static str>,
    /// Tags.
    pub tags: &'static [&'static str],
    /// Omitempty tags (include "" for default).
    pub omitempty_tags: &'static [&'static str],
    pub with_quote: bool,
}

impl FieldMeta {
    pub fn name_for_select(&self, flavor: Flavor, alias: &str) -> String {
        let base = if self.with_quote {
            flavor.quote(alias)
        } else {
            alias.to_string()
        };
        if let Some(as_) = self.as_ {
            format!("{base} AS {as_}")
        } else {
            base
        }
    }
}

fn is_ignored(fm: &FieldMeta) -> bool {
    // Honor `db:"-"`: skip this field.
    fm.db == "-"
}

/// Trait implemented by the macro for your structs: exposes metadata, values, and emptiness checks.
pub trait SqlStruct: Sized {
    const FIELDS: &'static [FieldMeta];

    /// Extract field values for INSERT/UPDATE (in FIELDS order).
    fn values(&self) -> Vec<crate::modifiers::Arg>;

    /// Check whether a field is "empty" (used for omitempty).
    fn is_empty_field(&self, rust_field: &'static str) -> bool;

    /// Return writable scan targets for `Struct::addr*`.
    ///
    /// Note: builds all `ScanCell`s at once (internally holds raw pointers) to avoid borrow-checker conflicts.
    fn addr_cells<'a>(
        &'a mut self,
        rust_fields: &[&'static str],
    ) -> Option<Vec<crate::scan::ScanCell<'a>>>;
}

/// Trait to determine "empty" values (implements omitempty semantics).
pub trait IsEmpty {
    fn is_empty_value(&self) -> bool;
}

impl IsEmpty for String {
    fn is_empty_value(&self) -> bool {
        self.is_empty()
    }
}

impl IsEmpty for &str {
    fn is_empty_value(&self) -> bool {
        self.is_empty()
    }
}

impl IsEmpty for bool {
    fn is_empty_value(&self) -> bool {
        !*self
    }
}

macro_rules! empty_num {
    ($($t:ty),+ $(,)?) => {
        $(impl IsEmpty for $t {
            fn is_empty_value(&self) -> bool {
                *self == 0 as $t
            }
        })+
    };
}

empty_num!(i8, i16, i32, i64, isize, u8, u16, u32, u64, usize);

impl IsEmpty for f64 {
    fn is_empty_value(&self) -> bool {
        // Use bits to check zero to avoid -0.0 edge cases.
        self.to_bits() == 0
    }
}

impl<T: IsEmpty> IsEmpty for Option<T> {
    fn is_empty_value(&self) -> bool {
        match self {
            None => true,
            Some(v) => v.is_empty_value(),
        }
    }
}

impl<T> IsEmpty for Vec<T> {
    fn is_empty_value(&self) -> bool {
        self.is_empty()
    }
}

impl IsEmpty for Box<dyn crate::valuer::SqlValuer> {
    fn is_empty_value(&self) -> bool {
        // Mirror pointer semantics: non-null pointer is not empty.
        false
    }
}

pub struct Struct<T: SqlStruct> {
    pub flavor: Flavor,
    mapper: FieldMapperFunc,
    with_tags: Vec<&'static str>,
    without_tags: Vec<&'static str>,
    _phantom: std::marker::PhantomData<T>,
}

impl<T: SqlStruct> Clone for Struct<T> {
    fn clone(&self) -> Self {
        Self {
            flavor: self.flavor,
            mapper: self.mapper.clone(),
            with_tags: self.with_tags.clone(),
            without_tags: self.without_tags.clone(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T: SqlStruct> std::fmt::Debug for Struct<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Mapper cannot derive Debug; print key fields only.
        f.debug_struct("Struct")
            .field("flavor", &self.flavor)
            .field("with_tags", &self.with_tags)
            .field("without_tags", &self.without_tags)
            .finish()
    }
}

impl<T: SqlStruct> Default for Struct<T> {
    fn default() -> Self {
        Self {
            flavor: crate::default_flavor(),
            mapper: default_field_mapper(),
            with_tags: Vec::new(),
            without_tags: Vec::new(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T: SqlStruct> Struct<T> {
    pub fn new() -> Self {
        Self::default()
    }

    /// WithFieldMapper: return a shadow copy with a different mapper.
    ///
    /// - Passing `identity_mapper()` matches the effect of a nil mapper.
    pub fn with_field_mapper(&self, mapper: FieldMapperFunc) -> Self {
        let mut c = self.clone();
        c.mapper = mapper;
        c
    }

    fn has_defined_tag(tag: &str) -> bool {
        if tag.is_empty() {
            return false;
        }
        T::FIELDS
            .iter()
            .any(|f| !is_ignored(f) && f.tags.contains(&tag))
    }

    /// ForFlavor: return a shadow copy with a different flavor.
    pub fn for_flavor(&self, flavor: Flavor) -> Self {
        let mut c = self.clone();
        c.flavor = flavor;
        c
    }

    /// WithTag: return a shadow copy with additional tags.
    pub fn with_tag(&self, tags: impl IntoIterator<Item = &'static str>) -> Self {
        let mut c = self.clone();
        for t in tags {
            if t.is_empty() {
                continue;
            }
            if !c.with_tags.contains(&t) {
                c.with_tags.push(t);
            }
        }
        c.with_tags.sort_unstable();
        c.with_tags.dedup();
        c
    }

    /// WithoutTag: return a shadow copy excluding specific tags.
    pub fn without_tag(&self, tags: impl IntoIterator<Item = &'static str>) -> Self {
        let mut c = self.clone();
        for t in tags {
            if t.is_empty() {
                continue;
            }
            if !c.without_tags.contains(&t) {
                c.without_tags.push(t);
            }
        }
        c.without_tags.sort_unstable();
        c.without_tags.dedup();
        // Filter out excluded with_tags entries.
        c.with_tags.retain(|t| !c.without_tags.contains(t));
        c
    }

    fn should_omit_empty(&self, fm: &FieldMeta) -> bool {
        // Omit-empty rules:
        // - default tag ""
        // - then any with_tags
        let omit = fm.omitempty_tags;
        if omit.is_empty() {
            return false;
        }
        if omit.contains(&"") {
            return true;
        }
        self.with_tags.iter().any(|t| omit.contains(t))
    }

    fn excluded_by_without(&self, fm: &FieldMeta) -> bool {
        self.without_tags.iter().any(|t| fm.tags.contains(t))
    }

    fn alias_of(&self, fm: &FieldMeta) -> String {
        if is_ignored(fm) {
            return String::new();
        }

        if !fm.db.is_empty() {
            return fm.db.to_string();
        }

        let mapped = (self.mapper)(fm.orig);
        if mapped.is_empty() {
            fm.orig.to_string()
        } else {
            mapped
        }
    }

    fn read_key_of(&self, fm: &FieldMeta) -> String {
        // Key preference: AS alias, otherwise mapped alias, otherwise rust name.
        if let Some(as_) = fm.as_ {
            return as_.to_string();
        }
        let a = self.alias_of(fm);
        if a.is_empty() { fm.rust.to_string() } else { a }
    }

    fn write_key_of(&self, fm: &FieldMeta) -> String {
        // For writes: deduplicate by alias.
        let a = self.alias_of(fm);
        if a.is_empty() { fm.rust.to_string() } else { a }
    }

    fn fields_for_read(&self) -> Vec<&'static FieldMeta> {
        self.fields_filtered(true)
    }

    fn fields_for_write(&self) -> Vec<&'static FieldMeta> {
        self.fields_filtered(false)
    }

    fn fields_filtered(&self, for_read: bool) -> Vec<&'static FieldMeta> {
        let mut out = Vec::new();
        let mut seen = HashSet::<String>::new();

        let push_field = |out: &mut Vec<&'static FieldMeta>,
                          seen: &mut HashSet<String>,
                          fm: &'static FieldMeta,
                          for_read: bool| {
            if is_ignored(fm) {
                return;
            }
            if self.excluded_by_without(fm) {
                return;
            }
            let key = if for_read {
                self.read_key_of(fm)
            } else {
                self.write_key_of(fm)
            };
            if seen.insert(key) {
                out.push(fm);
            }
        };

        if self.with_tags.is_empty() {
            for fm in T::FIELDS {
                push_field(&mut out, &mut seen, fm, for_read);
            }
            return out;
        }

        // Filter by with_tags in order (deduplicated).
        for tag in &self.with_tags {
            for fm in T::FIELDS {
                if fm.tags.contains(tag) {
                    push_field(&mut out, &mut seen, fm, for_read);
                }
            }
        }

        out
    }

    fn parse_table_alias(table: &str) -> &str {
        // Match Go behavior: take token after the last space.
        table.rsplit_once(' ').map(|(_, a)| a).unwrap_or(table)
    }

    /// Columns: return unquoted column names for write.
    pub fn columns(&self) -> Vec<String> {
        self.fields_for_write()
            .into_iter()
            .map(|f| self.alias_of(f))
            .collect()
    }

    /// ColumnsForTag: return columns for a specific tag; None if tag not defined.
    pub fn columns_for_tag(&self, tag: &str) -> Option<Vec<String>> {
        if !Self::has_defined_tag(tag) {
            return None;
        }
        // API constraint: requires &'static str; we leak for convenience. Could switch to Cow later.
        let tag: &'static str = Box::leak(tag.to_string().into_boxed_str());
        Some(self.with_tag([tag]).columns())
    }

    /// Values: return values for write in the same order as `columns()`.
    pub fn values(&self, v: &T) -> Vec<crate::modifiers::Arg> {
        let all = v.values();
        let mut out = Vec::new();
        for (fm, arg) in T::FIELDS.iter().zip(all) {
            if is_ignored(fm) || self.excluded_by_without(fm) {
                continue;
            }
            if self.with_tags.is_empty() || self.with_tags.iter().any(|t| fm.tags.contains(t)) {
                out.push(arg);
            }
        }
        // Note: initial order follows declaration, not tag grouping; reorder with fields_for_write to dedupe by tag order.
        let mut map = std::collections::HashMap::<&'static str, crate::modifiers::Arg>::new();
        for (fm, arg) in T::FIELDS.iter().zip(v.values()) {
            map.insert(fm.rust, arg);
        }
        self.fields_for_write()
            .into_iter()
            .filter_map(|fm| map.get(fm.rust).cloned())
            .collect()
    }

    /// ValuesForTag: values restricted to a tag; None if tag not defined.
    pub fn values_for_tag(&self, tag: &str, v: &T) -> Option<Vec<crate::modifiers::Arg>> {
        if !Self::has_defined_tag(tag) {
            return None;
        }
        let tag: &'static str = Box::leak(tag.to_string().into_boxed_str());
        Some(self.with_tag([tag]).values(v))
    }

    /// ForeachRead: iterate readable fields.
    ///
    /// - `dbtag`: db tag (may be empty)
    /// - `is_quoted`: whether the column needs quoting
    /// - `field_meta`: Rust-side metadata instead of reflect.StructField
    pub fn foreach_read(&self, mut trans: impl FnMut(&str, bool, &FieldMeta)) {
        for fm in self.fields_for_read() {
            trans(fm.db, fm.with_quote, fm);
        }
    }

    /// ForeachWrite: iterate writable fields.
    pub fn foreach_write(&self, mut trans: impl FnMut(&str, bool, &FieldMeta)) {
        for fm in self.fields_for_write() {
            trans(fm.db, fm.with_quote, fm);
        }
    }

    /// Addr: return scan targets for readable fields.
    pub fn addr<'a>(&self, st: &'a mut T) -> Vec<crate::scan::ScanCell<'a>> {
        let rust_fields: Vec<&'static str> = self
            .fields_for_read()
            .into_iter()
            .map(|fm| fm.rust)
            .collect();
        st.addr_cells(&rust_fields).unwrap_or_default()
    }

    /// AddrForTag: scan targets filtered by tag; None if tag not defined.
    pub fn addr_for_tag<'a>(
        &self,
        tag: &str,
        st: &'a mut T,
    ) -> Option<Vec<crate::scan::ScanCell<'a>>> {
        if !Self::has_defined_tag(tag) {
            return None;
        }
        let tag: &'static str = Box::leak(tag.to_string().into_boxed_str());
        Some(self.with_tag([tag]).addr(st))
    }

    /// AddrWithCols: scan targets for specific columns; None if any column is missing.
    pub fn addr_with_cols<'a>(
        &self,
        cols: &[&str],
        st: &'a mut T,
    ) -> Option<Vec<crate::scan::ScanCell<'a>>> {
        let fields = self.fields_for_read();
        let mut map = std::collections::HashMap::<String, &'static str>::new();
        for fm in fields {
            let key = self.read_key_of(fm);
            map.insert(key, fm.rust);
        }

        let mut rust_fields = Vec::with_capacity(cols.len());
        for &c in cols {
            rust_fields.push(*map.get(c)?);
        }
        st.addr_cells(&rust_fields)
    }

    pub fn select_from(&self, table: &str) -> SelectBuilder {
        let mut sb = SelectBuilder::new();
        sb.set_flavor(self.flavor);
        sb.from([table.to_string()]);

        let alias = Self::parse_table_alias(table);
        let cols: Vec<String> = self
            .fields_for_read()
            .into_iter()
            .map(|f| {
                let field_alias = self.alias_of(f);
                let mut c = String::new();
                // Follow alias rule: only check if alias contains '.' when adding table prefix.
                if self.flavor != Flavor::CQL && !field_alias.contains('.') {
                    c.push_str(alias);
                    c.push('.');
                }
                c.push_str(&f.name_for_select(self.flavor, &field_alias));
                c
            })
            .collect();

        if cols.is_empty() {
            select_cols!(sb, "*");
        } else {
            sb.select(cols);
        }
        sb
    }

    /// SelectFromForTag: build SELECT for a tag (deprecated).
    pub fn select_from_for_tag(&self, table: &str, tag: &str) -> SelectBuilder {
        // If tag is missing: behaves like SELECT * (with_tag yields empty cols => select "*").
        let tag: &'static str = Box::leak(tag.to_string().into_boxed_str());
        self.with_tag([tag]).select_from(table)
    }

    pub fn update(&self, table: &str, value: &T) -> UpdateBuilder {
        let mut ub = UpdateBuilder::new();
        ub.set_flavor(self.flavor);
        ub.update([table.to_string()]);

        let mut assigns = Vec::new();

        let mut map = std::collections::HashMap::<&'static str, crate::modifiers::Arg>::new();
        for (fm, arg) in T::FIELDS.iter().zip(value.values()) {
            map.insert(fm.rust, arg);
        }

        for fm in self.fields_for_write() {
            if self.should_omit_empty(fm) && value.is_empty_field(fm.rust) {
                continue;
            }
                // If with_quote, keep quoting when writing column names.
            let field_alias = self.alias_of(fm);
            let col = if fm.with_quote {
                self.flavor.quote(&field_alias)
            } else {
                field_alias
            };
            if let Some(v) = map.get(fm.rust).cloned() {
                assigns.push(ub.assign(&col, v));
            }
        }

        ub.set(assigns);
        ub
    }

    /// UpdateForTag: build UPDATE for a tag (deprecated).
    pub fn update_for_tag(&self, table: &str, tag: &str, value: &T) -> UpdateBuilder {
        let tag: &'static str = Box::leak(tag.to_string().into_boxed_str());
        self.with_tag([tag]).update(table, value)
    }

    pub fn delete_from(&self, table: &str) -> DeleteBuilder {
        let mut db = DeleteBuilder::new();
        db.set_flavor(self.flavor);
        db.delete_from([table.to_string()]);
        db
    }

    pub fn insert_into<'a>(
        &self,
        table: &str,
        rows: impl IntoIterator<Item = &'a T>,
    ) -> InsertBuilder
    where
        T: 'a,
    {
        self.insert_internal(table, rows, InsertVerb::Insert)
    }

    /// InsertIntoForTag: build INSERT for a tag (deprecated).
    pub fn insert_into_for_tag<'a>(
        &self,
        table: &str,
        tag: &str,
        rows: impl IntoIterator<Item = &'a T>,
    ) -> InsertBuilder
    where
        T: 'a,
    {
        let tag: &'static str = Box::leak(tag.to_string().into_boxed_str());
        self.with_tag([tag]).insert_into(table, rows)
    }

    pub fn insert_ignore_into_for_tag<'a>(
        &self,
        table: &str,
        tag: &str,
        rows: impl IntoIterator<Item = &'a T>,
    ) -> InsertBuilder
    where
        T: 'a,
    {
        let tag: &'static str = Box::leak(tag.to_string().into_boxed_str());
        self.with_tag([tag]).insert_ignore_into(table, rows)
    }

    pub fn replace_into_for_tag<'a>(
        &self,
        table: &str,
        tag: &str,
        rows: impl IntoIterator<Item = &'a T>,
    ) -> InsertBuilder
    where
        T: 'a,
    {
        let tag: &'static str = Box::leak(tag.to_string().into_boxed_str());
        self.with_tag([tag]).replace_into(table, rows)
    }

    fn filter_rows_any<'a>(values: impl IntoIterator<Item = &'a dyn Any>) -> Vec<&'a T>
    where
        T: 'static,
    {
        values
            .into_iter()
            .filter_map(|v| v.downcast_ref::<T>())
            .collect()
    }

    /// InsertIntoAny: ignore values that are not of type T (like Go's permissive interface slice).
    pub fn insert_into_any<'a>(
        &self,
        table: &str,
        values: impl IntoIterator<Item = &'a dyn Any>,
    ) -> InsertBuilder
    where
        T: 'static,
    {
        let rows = Self::filter_rows_any(values);
        self.insert_into(table, rows)
    }

    pub fn insert_ignore_into_any<'a>(
        &self,
        table: &str,
        values: impl IntoIterator<Item = &'a dyn Any>,
    ) -> InsertBuilder
    where
        T: 'static,
    {
        let rows = Self::filter_rows_any(values);
        self.insert_ignore_into(table, rows)
    }

    pub fn replace_into_any<'a>(
        &self,
        table: &str,
        values: impl IntoIterator<Item = &'a dyn Any>,
    ) -> InsertBuilder
    where
        T: 'static,
    {
        let rows = Self::filter_rows_any(values);
        self.replace_into(table, rows)
    }

    pub fn insert_into_for_tag_any<'a>(
        &self,
        table: &str,
        tag: &str,
        values: impl IntoIterator<Item = &'a dyn Any>,
    ) -> InsertBuilder
    where
        T: 'static,
    {
        let tag: &'static str = Box::leak(tag.to_string().into_boxed_str());
        let rows = Self::filter_rows_any(values);
        self.with_tag([tag]).insert_into(table, rows)
    }

    pub fn insert_ignore_into_for_tag_any<'a>(
        &self,
        table: &str,
        tag: &str,
        values: impl IntoIterator<Item = &'a dyn Any>,
    ) -> InsertBuilder
    where
        T: 'static,
    {
        let tag: &'static str = Box::leak(tag.to_string().into_boxed_str());
        let rows = Self::filter_rows_any(values);
        self.with_tag([tag]).insert_ignore_into(table, rows)
    }

    pub fn replace_into_for_tag_any<'a>(
        &self,
        table: &str,
        tag: &str,
        values: impl IntoIterator<Item = &'a dyn Any>,
    ) -> InsertBuilder
    where
        T: 'static,
    {
        let tag: &'static str = Box::leak(tag.to_string().into_boxed_str());
        let rows = Self::filter_rows_any(values);
        self.with_tag([tag]).replace_into(table, rows)
    }

    pub fn insert_ignore_into<'a>(
        &self,
        table: &str,
        rows: impl IntoIterator<Item = &'a T>,
    ) -> InsertBuilder
    where
        T: 'a,
    {
        self.insert_internal(table, rows, InsertVerb::InsertIgnore)
    }

    pub fn replace_into<'a>(
        &self,
        table: &str,
        rows: impl IntoIterator<Item = &'a T>,
    ) -> InsertBuilder
    where
        T: 'a,
    {
        self.insert_internal(table, rows, InsertVerb::Replace)
    }

    fn insert_internal<'a>(
        &self,
        table: &str,
        rows: impl IntoIterator<Item = &'a T>,
        verb: InsertVerb,
    ) -> InsertBuilder
    where
        T: 'a,
    {
        let mut ib = InsertBuilder::new();
        ib.set_flavor(self.flavor);
        match verb {
            InsertVerb::Insert => {
                ib.insert_into(table);
            }
            InsertVerb::InsertIgnore => {
                ib.insert_ignore_into(table);
            }
            InsertVerb::Replace => {
                ib.replace_into(table);
            }
        }

        let rows: Vec<&T> = rows.into_iter().collect();
        if rows.is_empty() {
            // Empty rows: do not emit cols/values.
            return ib;
        }

        let fields = self.fields_for_write();

        // Decide if a column should be filtered entirely (omitempty and all rows empty).
        let mut nil_cnt = vec![0_usize; fields.len()];
        for (fi, fm) in fields.iter().enumerate() {
            let should_omit = self.should_omit_empty(fm);
            if !should_omit {
                continue;
            }
            for r in &rows {
                if r.is_empty_field(fm.rust) {
                    nil_cnt[fi] += 1;
                }
            }
        }

        let mut kept = Vec::<usize>::new();
        for (i, cnt) in nil_cnt.into_iter().enumerate() {
            if cnt == rows.len() {
                continue;
            }
            kept.push(i);
        }

        let cols: Vec<String> = kept
            .iter()
            .map(|&i| {
                let fm = fields[i];
                let field_alias = self.alias_of(fm);
                if fm.with_quote {
                    self.flavor.quote(&field_alias)
                } else {
                    field_alias
                }
            })
            .collect();
        ib.cols(escape_all(cols));

        for r in rows {
            let mut map = std::collections::HashMap::<&'static str, crate::modifiers::Arg>::new();
            for (fm, arg) in T::FIELDS.iter().zip(r.values()) {
                map.insert(fm.rust, arg);
            }
            let mut row_args = Vec::new();
            for &i in &kept {
                let fm = fields[i];
                row_args.push(
                    map.get(fm.rust)
                        .cloned()
                        .unwrap_or_else(|| crate::SqlValue::Null.into()),
                );
            }
            ib.values(row_args);
        }

        ib
    }
}

#[derive(Debug, Clone, Copy)]
enum InsertVerb {
    Insert,
    InsertIgnore,
    Replace,
}

/// Declare metadata and value accessors for a business struct usable by `Struct<T>`.
///
/// Example:
///
/// ```ignore
/// #[derive(Default)]
/// struct User { id: i64, name: String }
///
/// halo_space::sqlbuilder::sql_struct! {
///   impl User {
///     id:  { db: "id", tags: ["pk"], omitempty: [], quote: false, as: None },
///     name:{ db: "name", tags: [],     omitempty: [""], quote: true,  as: None },
///   }
/// }
/// ```
#[macro_export]
macro_rules! sql_struct {
    (
        impl $ty:ty {
            $(
                $field:ident : { db: $db:literal, $(orig: $orig:literal,)? tags: [ $($tag:literal),* $(,)? ], omitempty: [ $($omit:literal),* $(,)? ], quote: $quote:literal, as: $as:expr }
            ),* $(,)?
        }
    ) => {
        impl $crate::structs::SqlStruct for $ty {
            const FIELDS: &'static [$crate::structs::FieldMeta] = &[
                $(
                    $crate::structs::FieldMeta{
                        rust: stringify!($field),
                        orig: $crate::__sql_struct_orig!(stringify!($field) $(, $orig)?),
                        db: $db,
                        as_: $as,
                        tags: &[ $($tag),* ],
                        omitempty_tags: &[ $($omit),* ],
                        with_quote: $quote,
                    }
                ),*
            ];

            fn values(&self) -> Vec<$crate::modifiers::Arg> {
                vec![
                    $(
                        $crate::modifiers::Arg::from(self.$field.clone())
                    ),*
                ]
            }

            fn is_empty_field(&self, rust_field: &'static str) -> bool {
                match rust_field {
                    $(
                        stringify!($field) => $crate::structs::IsEmpty::is_empty_value(&self.$field),
                    )*
                    _ => false,
                }
            }

            fn addr_cells<'a>(
                &'a mut self,
                rust_fields: &[&'static str],
            ) -> Option<Vec<$crate::scan::ScanCell<'a>>> {
                let mut out = Vec::with_capacity(rust_fields.len());
                for &rf in rust_fields {
                    match rf {
                        $(
                            stringify!($field) => {
                                out.push($crate::scan::ScanCell::from_ptr(std::ptr::addr_of_mut!(self.$field)));
                            }
                        )*
                        _ => return None,
                    }
                }
                Some(out)
            }
        }
    };
}

/// Macro helper: support optional `orig:` parameter.
#[doc(hidden)]
#[macro_export]
macro_rules! __sql_struct_orig {
    ($default:expr) => {
        $default
    };
    ($default:expr, $custom:expr) => {
        $custom
    };
}
