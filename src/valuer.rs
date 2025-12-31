//! SqlValuer: minimal support for `database/sql/driver.Valuer`-like behavior.
//!
//! In Go, `driver.Valuer` can be invoked during interpolation to obtain the final serializable value.
//! Rust lacks a standard trait, so we provide a crate-local one for users/tests to implement.

use crate::value::SqlValue;

/// Error returned by `SqlValuer`.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[error("builder sql valuer error: {0}")]
pub struct ValuerError(pub String);

/// Trait for computing SQL values at interpolation time.
pub trait SqlValuer: dyn_clone::DynClone + std::fmt::Debug {
    fn value(&self) -> Result<SqlValue, ValuerError>;
}

dyn_clone::clone_trait_object!(SqlValuer);
