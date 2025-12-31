//! SQL Flavor (dialects): control placeholders, quoting, and interpolation behavior.

use std::fmt;
use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::{Mutex, MutexGuard};

/// Flavor enum describing supported SQL dialects.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum Flavor {
    #[default]
    MySQL,
    PostgreSQL,
    SQLite,
    SQLServer,
    CQL,
    ClickHouse,
    Presto,
    Oracle,
    Informix,
    Doris,
}

static DEFAULT_FLAVOR: AtomicU8 = AtomicU8::new(Flavor::MySQL as u8);
static DEFAULT_FLAVOR_LOCK: Mutex<()> = Mutex::new(());

impl Flavor {
    fn from_u8(v: u8) -> Self {
        match v {
            0 => Self::MySQL,
            1 => Self::PostgreSQL,
            2 => Self::SQLite,
            3 => Self::SQLServer,
            4 => Self::CQL,
            5 => Self::ClickHouse,
            6 => Self::Presto,
            7 => Self::Oracle,
            8 => Self::Informix,
            9 => Self::Doris,
            _ => Self::MySQL,
        }
    }

    fn to_u8(self) -> u8 {
        self as u8
    }
}

/// Get the current global default Flavor.
pub fn default_flavor() -> Flavor {
    Flavor::from_u8(DEFAULT_FLAVOR.load(Ordering::Relaxed))
}

/// Set the global default Flavor and return the previous one.
pub fn set_default_flavor(flavor: Flavor) -> Flavor {
    let old = DEFAULT_FLAVOR.swap(flavor.to_u8(), Ordering::Relaxed);
    Flavor::from_u8(old)
}

/// RAII guard for temporarily changing the global Flavor (holds a global lock to avoid test interference).
pub struct DefaultFlavorGuard {
    _lock: MutexGuard<'static, ()>,
    old: Flavor,
}

impl Drop for DefaultFlavorGuard {
    fn drop(&mut self) {
        set_default_flavor(self.old);
    }
}

/// Temporarily set the default Flavor for a scope and restore it on drop.
pub fn set_default_flavor_scoped(flavor: Flavor) -> DefaultFlavorGuard {
    let lock = DEFAULT_FLAVOR_LOCK
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    let old = set_default_flavor(flavor);
    DefaultFlavorGuard { _lock: lock, old }
}

impl fmt::Display for Flavor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::MySQL => "MySQL",
            Self::PostgreSQL => "PostgreSQL",
            Self::SQLite => "SQLite",
            Self::SQLServer => "SQLServer",
            Self::CQL => "CQL",
            Self::ClickHouse => "ClickHouse",
            Self::Presto => "Presto",
            Self::Oracle => "Oracle",
            Self::Informix => "Informix",
            Self::Doris => "Doris",
        };
        f.write_str(s)
    }
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum InterpolateError {
    #[error("builder interpolation for this flavor is not implemented")]
    NotImplemented,
    #[error("builder not enough args when interpolating")]
    MissingArgs,
    #[error("builder unsupported args when interpolating")]
    UnsupportedArgs,
    #[error("{0}")]
    ValuerError(#[from] crate::valuer::ValuerError),
}

impl Flavor {
    /// Quote an identifier using the dialect's rules.
    pub fn quote(self, name: &str) -> String {
        match self {
            Self::MySQL | Self::ClickHouse | Self::Doris => format!("`{name}`"),
            Self::PostgreSQL
            | Self::SQLServer
            | Self::SQLite
            | Self::Presto
            | Self::Oracle
            | Self::Informix => {
                format!("\"{name}\"")
            }
            Self::CQL => format!("'{name}'"),
        }
    }

    /// Dialect-specific INSERT/IGNORE keyword choice.
    pub fn prepare_insert_ignore(self) -> &'static str {
        match self {
            Flavor::MySQL | Flavor::Oracle => "INSERT IGNORE",
            Flavor::PostgreSQL => "INSERT",
            Flavor::SQLite => "INSERT OR IGNORE",
            _ => "INSERT",
        }
    }
}
