//! Macro helpers providing variadic-style ergonomics for builders.
//! Macros like `select_cols!` / `where_exprs!` accept varargs strings without manual Vec creation.

#[doc(hidden)]
#[macro_export]
macro_rules! __collect_strings {
    () => {
        Vec::<String>::new()
    };
    ($($value:expr),+ $(,)?) => {{
        let mut values = Vec::<String>::new();
        $(
            $crate::extend_into_strings($value, &mut values);
        )*
        values
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! __collect_static_strs {
    () => {
        Vec::<&'static str>::new()
    };
    ($($value:expr),+ $(,)?) => {{
        let mut values = Vec::<&'static str>::new();
        $(
            values.push($value);
        )*
        values
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! __builder_with_strings {
    ($builder:expr, $method:ident $(, $arg:expr)* $(,)?) => {
        $builder.$method($crate::__collect_strings!($($arg),*))
    };
}

pub trait IntoStrings {
    fn extend_into_strings(self, dst: &mut Vec<String>);
}

impl IntoStrings for String {
    fn extend_into_strings(self, dst: &mut Vec<String>) {
        dst.push(self);
    }
}

impl IntoStrings for &str {
    fn extend_into_strings(self, dst: &mut Vec<String>) {
        dst.push(self.to_string());
    }
}

impl<const N: usize, T> IntoStrings for [T; N]
where
    T: Into<String> + Clone,
{
    fn extend_into_strings(self, dst: &mut Vec<String>) {
        for item in &self {
            dst.push(item.clone().into());
        }
    }
}

impl<T> IntoStrings for &[T]
where
    T: Into<String> + Clone,
{
    fn extend_into_strings(self, dst: &mut Vec<String>) {
        for item in self {
            dst.push(item.clone().into());
        }
    }
}

impl<T> IntoStrings for &Vec<T>
where
    T: Into<String> + Clone,
{
    fn extend_into_strings(self, dst: &mut Vec<String>) {
        for item in self {
            dst.push(item.clone().into());
        }
    }
}

impl<T> IntoStrings for Vec<T>
where
    T: Into<String>,
{
    fn extend_into_strings(self, dst: &mut Vec<String>) {
        for item in self {
            dst.push(item.into());
        }
    }
}

#[doc(hidden)]
pub fn extend_into_strings<T>(value: T, dst: &mut Vec<String>)
where
    T: IntoStrings,
{
    value.extend_into_strings(dst);
}

#[doc(hidden)]
pub fn collect_into_strings<T>(value: T) -> Vec<String>
where
    T: IntoStrings,
{
    let mut dst = Vec::new();
    value.extend_into_strings(&mut dst);
    dst
}

#[doc(hidden)]
#[macro_export]
macro_rules! __builder_with_strings_after {
    ($builder:expr, $method:ident, $first:expr $(, $arg:expr)* $(,)?) => {
        $builder.$method($first, $crate::__collect_strings!($($arg),*))
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __builder_with_strings_after_two {
    ($builder:expr, $method:ident, $first:expr, $second:expr $(, $arg:expr)* $(,)?) => {
        $builder.$method($first, $second, $crate::__collect_strings!($($arg),*))
    };
}

/// Variadic helper for `SelectBuilder::select`.
#[macro_export]
macro_rules! select_cols {
    ($builder:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, select $(, $col)*)
    };
}
pub use crate::select_cols;

/// Variadic helper for `SelectBuilder::select_more`.
#[macro_export]
macro_rules! select_more_cols {
    ($builder:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, select_more $(, $col)*)
    };
}
pub use crate::select_more_cols;

/// Variadic helper for `SelectBuilder::from`.
#[macro_export]
macro_rules! from_tables {
    ($builder:expr $(, $table:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, from $(, $table)*)
    };
}
pub use crate::from_tables;

/// Variadic helper for `SelectBuilder::join`.
#[macro_export]
macro_rules! join_on {
    ($builder:expr, $table:expr $(, $expr:expr)* $(,)?) => {
        $crate::__builder_with_strings_after!($builder, join, $table $(, $expr)*)
    };
}
pub use crate::join_on;

/// Variadic helper for `SelectBuilder::join_with_option`.
#[macro_export]
macro_rules! join_with_option {
    ($builder:expr, $option:expr, $table:expr $(, $expr:expr)* $(,)?) => {
        $crate::__builder_with_strings_after_two!($builder, join_with_option, $option, $table $(, $expr)*)
    };
}
pub use crate::join_with_option;

/// Variadic helper for `where_` calls (Select/Update/Delete).
#[macro_export]
macro_rules! where_exprs {
    ($builder:expr $(, $expr:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, where_ $(, $expr)*)
    };
}
pub use crate::where_exprs;

/// Variadic helper for `having`.
#[macro_export]
macro_rules! having_exprs {
    ($builder:expr $(, $expr:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, having $(, $expr)*)
    };
}
pub use crate::having_exprs;

/// Variadic helper for `group_by`.
#[macro_export]
macro_rules! group_by_cols {
    ($builder:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, group_by $(, $col)*)
    };
}
pub use crate::group_by_cols;

/// Variadic helper for `order_by`.
#[macro_export]
macro_rules! order_by_cols {
    ($builder:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, order_by $(, $col)*)
    };
}
pub use crate::order_by_cols;

/// Variadic helper for `InsertBuilder::cols`.
#[macro_export]
macro_rules! insert_cols {
    ($builder:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, cols $(, $col)*)
    };
}
pub use crate::insert_cols;

/// Variadic helper for `InsertBuilder::select`.
#[macro_export]
macro_rules! insert_select_cols {
    ($builder:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, select $(, $col)*)
    };
}
pub use crate::insert_select_cols;

/// Variadic helper for `returning` calls.
#[macro_export]
macro_rules! returning_cols {
    ($builder:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, returning $(, $col)*)
    };
}
pub use crate::returning_cols;

/// Variadic helper for `DeleteBuilder::delete_from`.
#[macro_export]
macro_rules! delete_from_tables {
    ($builder:expr $(, $table:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, delete_from $(, $table)*)
    };
}
pub use crate::delete_from_tables;

/// Variadic helper for `UpdateBuilder::update`.
#[macro_export]
macro_rules! update_tables {
    ($builder:expr $(, $table:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, update $(, $table)*)
    };
}
pub use crate::update_tables;

/// Variadic helper for `UpdateBuilder::set`.
#[macro_export]
macro_rules! update_set {
    ($builder:expr $(, $assignment:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, set $(, $assignment)*)
    };
}
pub use crate::update_set;

/// Variadic helper for `UpdateBuilder::set_more`.
#[macro_export]
macro_rules! update_set_more {
    ($builder:expr $(, $assignment:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, set_more $(, $assignment)*)
    };
}
pub use crate::update_set_more;

/// Variadic helper for `CTEBuilder::select`.
#[macro_export]
macro_rules! cte_select_cols {
    ($builder:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, select $(, $col)*)
    };
}
pub use crate::cte_select_cols;

/// Variadic helper for `CTEBuilder::delete_from`.
#[macro_export]
macro_rules! cte_delete_from {
    ($builder:expr $(, $table:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, delete_from $(, $table)*)
    };
}
pub use crate::cte_delete_from;

/// Variadic helper for `CTEBuilder::update`.
#[macro_export]
macro_rules! cte_update_tables {
    ($builder:expr $(, $table:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, update $(, $table)*)
    };
}
pub use crate::cte_update_tables;

/// Variadic helper for `CTEQueryBuilder::table`.
#[macro_export]
macro_rules! cte_query_table {
    ($builder:expr, $name:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings_after!($builder, table, $name $(, $col)*)
    };
}
pub use crate::cte_query_table;

/// Variadic helper for `CreateTableBuilder::define`.
#[macro_export]
macro_rules! create_table_define {
    ($builder:expr $(, $def:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, define $(, $def)*)
    };
}
pub use crate::create_table_define;

/// Variadic helper for `CreateTableBuilder::option`.
#[macro_export]
macro_rules! create_table_option {
    ($builder:expr $(, $opt:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, option $(, $opt)*)
    };
}
pub use crate::create_table_option;

/// Variadic helper for `Struct::with_tag`.
#[macro_export]
macro_rules! struct_with_tag {
    ($builder:expr $(, $tag:expr)* $(,)?) => {
        $builder.with_tag($crate::__collect_static_strs!($($tag),*))
    };
}
pub use crate::struct_with_tag;

/// Variadic helper for `Struct::without_tag`.
#[macro_export]
macro_rules! struct_without_tag {
    ($builder:expr $(, $tag:expr)* $(,)?) => {
        $builder.without_tag($crate::__collect_static_strs!($($tag),*))
    };
}
pub use crate::struct_without_tag;
