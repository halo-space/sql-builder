//! 宏集合：为 builder 提供 Go 式的可变参数调用封装。
//! 通过 `select_cols!` / `where_exprs!` 等宏，可以使用不定长字符串参数而无需手动创建 `Vec`。

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

/// 为 `SelectBuilder::select` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! select_cols {
    ($builder:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, select $(, $col)*)
    };
}
pub use crate::select_cols;

/// 为 `SelectBuilder::select_more` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! select_more_cols {
    ($builder:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, select_more $(, $col)*)
    };
}
pub use crate::select_more_cols;

/// 为 `SelectBuilder::from` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! from_tables {
    ($builder:expr $(, $table:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, from $(, $table)*)
    };
}
pub use crate::from_tables;

/// 为 `SelectBuilder::join` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! join_on {
    ($builder:expr, $table:expr $(, $expr:expr)* $(,)?) => {
        $crate::__builder_with_strings_after!($builder, join, $table $(, $expr)*)
    };
}
pub use crate::join_on;

/// 为 `SelectBuilder::join_with_option` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! join_with_option {
    ($builder:expr, $option:expr, $table:expr $(, $expr:expr)* $(,)?) => {
        $crate::__builder_with_strings_after_two!($builder, join_with_option, $option, $table $(, $expr)*)
    };
}
pub use crate::join_with_option;

/// 为所有 `where_` 调用提供 Go 风格的可变参数调用（Select/Update/Delete）。
#[macro_export]
macro_rules! where_exprs {
    ($builder:expr $(, $expr:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, where_ $(, $expr)*)
    };
}
pub use crate::where_exprs;

/// 为 `having` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! having_exprs {
    ($builder:expr $(, $expr:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, having $(, $expr)*)
    };
}
pub use crate::having_exprs;

/// 为 `group_by` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! group_by_cols {
    ($builder:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, group_by $(, $col)*)
    };
}
pub use crate::group_by_cols;

/// 为 `order_by` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! order_by_cols {
    ($builder:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, order_by $(, $col)*)
    };
}
pub use crate::order_by_cols;

/// 为 `InsertBuilder::cols` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! insert_cols {
    ($builder:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, cols $(, $col)*)
    };
}
pub use crate::insert_cols;

/// 为 `InsertBuilder::select` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! insert_select_cols {
    ($builder:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, select $(, $col)*)
    };
}
pub use crate::insert_select_cols;

/// 为所有 `returning` 调用提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! returning_cols {
    ($builder:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, returning $(, $col)*)
    };
}
pub use crate::returning_cols;

/// 为 `DeleteBuilder::delete_from` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! delete_from_tables {
    ($builder:expr $(, $table:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, delete_from $(, $table)*)
    };
}
pub use crate::delete_from_tables;

/// 为 `UpdateBuilder::update` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! update_tables {
    ($builder:expr $(, $table:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, update $(, $table)*)
    };
}
pub use crate::update_tables;

/// 为 `UpdateBuilder::set` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! update_set {
    ($builder:expr $(, $assignment:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, set $(, $assignment)*)
    };
}
pub use crate::update_set;

/// 为 `UpdateBuilder::set_more` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! update_set_more {
    ($builder:expr $(, $assignment:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, set_more $(, $assignment)*)
    };
}
pub use crate::update_set_more;

/// 为 `CTEBuilder::select` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! cte_select_cols {
    ($builder:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, select $(, $col)*)
    };
}
pub use crate::cte_select_cols;

/// 为 `CTEBuilder::delete_from` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! cte_delete_from {
    ($builder:expr $(, $table:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, delete_from $(, $table)*)
    };
}
pub use crate::cte_delete_from;

/// 为 `CTEBuilder::update` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! cte_update_tables {
    ($builder:expr $(, $table:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, update $(, $table)*)
    };
}
pub use crate::cte_update_tables;

/// 为 `CTEQueryBuilder::table` 提供 Go 风格的列名参数。
#[macro_export]
macro_rules! cte_query_table {
    ($builder:expr, $name:expr $(, $col:expr)* $(,)?) => {
        $crate::__builder_with_strings_after!($builder, table, $name $(, $col)*)
    };
}
pub use crate::cte_query_table;

/// 为 `CreateTableBuilder::define` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! create_table_define {
    ($builder:expr $(, $def:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, define $(, $def)*)
    };
}
pub use crate::create_table_define;

/// 为 `CreateTableBuilder::option` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! create_table_option {
    ($builder:expr $(, $opt:expr)* $(,)?) => {
        $crate::__builder_with_strings!($builder, option $(, $opt)*)
    };
}
pub use crate::create_table_option;

/// 为 `Struct::with_tag` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! struct_with_tag {
    ($builder:expr $(, $tag:expr)* $(,)?) => {
        $builder.with_tag($crate::__collect_static_strs!($($tag),*))
    };
}
pub use crate::struct_with_tag;

/// 为 `Struct::without_tag` 提供 Go 风格的可变参数调用。
#[macro_export]
macro_rules! struct_without_tag {
    ($builder:expr $(, $tag:expr)* $(,)?) => {
        $builder.without_tag($crate::__collect_static_strs!($($tag),*))
    };
}
pub use crate::struct_without_tag;
