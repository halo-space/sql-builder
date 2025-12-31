//! halo-sqlbuilder: composable SQL builder and argument collector.

pub mod args;
#[cfg(test)]
mod args_tests;
pub mod builder;
#[cfg(test)]
mod builder_tests;
pub mod cond;
#[cfg(test)]
mod cond_tests;
#[cfg(test)]
mod cond_where_tests;
pub mod condition;
pub mod create_table;
#[cfg(test)]
mod create_table_tests;
pub mod cte;
pub mod cte_query;
#[cfg(test)]
mod cte_tests;
pub mod delete;
#[cfg(test)]
mod delete_more_tests;
pub mod dialect;
pub mod expr;
pub mod field_mapper;
pub mod flavor;
#[cfg(test)]
mod flavor_tests;
pub mod injection;
pub mod insert;
#[cfg(test)]
mod insert_tests;
pub mod interpolate;
#[cfg(test)]
mod interpolate_tests;
pub mod macros;
pub use crate::macros::*;
#[cfg(test)]
mod macros_tests;
pub mod modifiers;
#[cfg(test)]
mod modifiers_more_tests;
pub mod scan;
pub mod select;
#[cfg(test)]
mod select_more_tests;
#[cfg(test)]
mod select_tests;
pub mod string_builder;
pub mod structs;
#[cfg(test)]
mod structs_tests;
pub mod union;
#[cfg(test)]
mod union_cte_create_table_tests;
#[cfg(test)]
mod union_more_tests;
pub mod update;
#[cfg(test)]
mod update_delete_tests;
pub mod value;
pub mod valuer;
pub mod where_clause;
#[cfg(test)]
mod where_clause_tests;

pub use crate::args::{Args, CompileError};
pub use crate::builder::{build, build_named, buildf, with_flavor};
pub use crate::cond::Cond;
pub use crate::condition::{
    Chain, ChainOptions, Condition, ConditionValue, JoinCondition, Operator, UpdateField,
    UpdateFieldChain, UpdateFieldOperator, UpdateFieldOptions, UpdateValue, build_delete,
    build_delete_with_flavor, build_select, build_select_with_flavor, build_update,
    build_update_with_flavor, quote_with_flavor, to_field_slice, unquote,
};
pub use crate::create_table::CreateTableBuilder;
pub use crate::cte::{CTEBuilder, with, with_recursive};
pub use crate::cte_query::CTEQueryBuilder;
pub use crate::delete::DeleteBuilder;
pub use crate::dialect::Dialect;
pub use crate::expr::Expr;
pub use crate::field_mapper::{
    FieldMapperFunc, default_field_mapper, identity_mapper, set_default_field_mapper,
    set_default_field_mapper_scoped, snake_case_mapper,
};
pub use crate::flavor::{
    Flavor, InterpolateError, default_flavor, set_default_flavor, set_default_flavor_scoped,
};
pub use crate::insert::InsertBuilder;
pub use crate::modifiers::{
    FlattenIntoArgs, Raw, RcBuilder, SqlNamedArg, escape, escape_all, flatten, list, named, raw,
    rc_builder, tuple, tuple_names,
};
pub use crate::scan::{ScanCell, ScanError, scan_tokens};
pub use crate::select::{JoinOption, SelectBuilder};
pub use crate::structs::{FieldMeta, FieldOpt, SqlStruct, Struct};
pub use crate::union::UnionBuilder;
pub use crate::update::UpdateBuilder;
pub use crate::value::SqlValue;
pub use crate::valuer::{SqlValuer, ValuerError};
pub use crate::where_clause::{WhereClause, WhereClauseBuilder, WhereClauseRef, copy_where_clause};

/// Preferred namespace: `use halo_space::sqlbuilder::{...}`.
pub mod sqlbuilder {
    pub use crate::*;
}
