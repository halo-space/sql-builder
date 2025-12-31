//! SQL interpolation: replace placeholders in `sql` with literal values from `args`.
//!
//! Safety warning: interpolation is less safe than prepared parameters; use only for drivers without parameter support.

use crate::flavor::{Flavor, InterpolateError};
use crate::modifiers::Arg;
use crate::value::{SqlDateTime, SqlValue};
use time::format_description::FormatItem;

impl Flavor {
    pub fn interpolate(self, sql: &str, args: &[Arg]) -> Result<String, InterpolateError> {
        match self {
            Flavor::MySQL => mysql_like_interpolate(self, sql, args),
            Flavor::SQLite => mysql_like_interpolate(self, sql, args),
            Flavor::CQL => mysql_like_interpolate(self, sql, args),
            Flavor::ClickHouse => mysql_like_interpolate(self, sql, args),
            Flavor::Presto => mysql_like_interpolate(self, sql, args),
            Flavor::Informix => mysql_like_interpolate(self, sql, args),
            Flavor::Doris => mysql_like_interpolate(self, sql, args),
            Flavor::PostgreSQL => postgresql_interpolate(sql, args),
            Flavor::SQLServer => sqlserver_interpolate(sql, args),
            Flavor::Oracle => oracle_interpolate(sql, args),
        }
    }
}

fn mysql_like_interpolate(
    flavor: Flavor,
    query: &str,
    args: &[Arg],
) -> Result<String, InterpolateError> {
    let mut out = String::with_capacity(query.len() + args.len() * 20);
    let mut quote: Option<char> = None;
    let mut escaping = false;
    let mut arg_idx = 0usize;

    for c in query.chars() {
        if escaping {
            out.push(c);
            escaping = false;
            continue;
        }

        match c {
            '\\' if quote.is_some() => {
                out.push(c);
                escaping = true;
            }
            '\'' | '"' | '`' => {
                if quote == Some(c) {
                    quote = None;
                } else if quote.is_none() {
                    quote = Some(c);
                }
                out.push(c);
            }
            '?' if quote.is_none() => {
                if arg_idx >= args.len() {
                    return Err(InterpolateError::MissingArgs);
                }
                encode_value(&mut out, &args[arg_idx], flavor)?;
                arg_idx += 1;
            }
            _ => out.push(c),
        }
    }

    Ok(out)
}

fn postgresql_interpolate(query: &str, args: &[Arg]) -> Result<String, InterpolateError> {
    let mut out = String::with_capacity(query.len() + args.len() * 20);
    let mut quote: Option<char> = None; // '\'' | '"' | '$'(dollar-quote)
    let mut escaping = false;
    let mut dollar_quote: Option<String> = None;

    let bytes = query.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() {
        let c = bytes[i] as char;

        if escaping {
            out.push(c);
            escaping = false;
            i += 1;
            continue;
        }

        match c {
            '\\' if matches!(quote, Some('\'') | Some('"')) => {
                out.push(c);
                escaping = true;
                i += 1;
            }
            '\'' => {
                if quote == Some('\'') {
                    // PostgreSQL: '' 表示一个 '
                    if i + 1 < bytes.len() && bytes[i + 1] as char == '\'' {
                        out.push('\'');
                        out.push('\'');
                        i += 2;
                        continue;
                    }
                    quote = None;
                } else if quote.is_none() {
                    quote = Some('\'');
                }
                out.push('\'');
                i += 1;
            }
            '"' => {
                if quote == Some('"') {
                    quote = None;
                } else if quote.is_none() {
                    quote = Some('"');
                }
                out.push('"');
                i += 1;
            }
            '$' => {
                if quote == Some('$') {
                    // Attempt to match the closing dollar-quote delimiter
                    if let Some(dq) = &dollar_quote
                        && query[i..].starts_with(dq)
                    {
                        out.push_str(dq);
                        i += dq.len();
                        quote = None;
                        dollar_quote = None;
                        continue;
                    }
                    out.push('$');
                    i += 1;
                    continue;
                }

                if quote.is_some() {
                    out.push('$');
                    i += 1;
                    continue;
                }

                // Parse the start of $n or $tag$ dollar-quote
                let mut j = i + 1;
                if j < bytes.len()
                    && (bytes[j] as char).is_ascii_digit()
                    && (bytes[j] as char) != '0'
                {
                    while j < bytes.len() && (bytes[j] as char).is_ascii_digit() {
                        j += 1;
                    }
                    let num_str = &query[i + 1..j];
                    let n: usize = num_str
                        .parse()
                        .map_err(|_| InterpolateError::UnsupportedArgs)?;
                    if n == 0 || n > args.len() {
                        return Err(InterpolateError::MissingArgs);
                    }
                    encode_value(&mut out, &args[n - 1], Flavor::PostgreSQL)?;
                    i = j;
                    continue;
                }

                // dollar quote begin: $tag$
                let mut k = i + 1;
                while k < bytes.len() && (bytes[k] as char).is_ascii_alphabetic() {
                    k += 1;
                }
                if k < bytes.len() && bytes[k] as char == '$' {
                    let dq = &query[i..=k];
                    out.push_str(dq);
                    quote = Some('$');
                    dollar_quote = Some(dq.to_string());
                    i = k + 1;
                    continue;
                }

                out.push('$');
                i += 1;
            }
            _ => {
                out.push(c);
                i += 1;
            }
        }
    }

    Ok(out)
}

fn sqlserver_interpolate(query: &str, args: &[Arg]) -> Result<String, InterpolateError> {
    let mut out = String::with_capacity(query.len() + args.len() * 20);
    let mut quote: Option<char> = None;
    let mut escaping = false;

    let bytes = query.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() {
        let c = bytes[i] as char;

        if escaping {
            out.push(c);
            escaping = false;
            i += 1;
            continue;
        }

        match c {
            '\\' if quote.is_some() => {
                out.push(c);
                escaping = true;
                i += 1;
            }
            '\'' | '"' => {
                if quote == Some(c) {
                    quote = None;
                } else if quote.is_none() {
                    quote = Some(c);
                }
                out.push(c);
                i += 1;
            }
            '@' if quote.is_none() => {
                // Only interpolate @pN/@PN
                if i + 2 < bytes.len()
                    && ((bytes[i + 1] as char) == 'p' || (bytes[i + 1] as char) == 'P')
                {
                    let mut j = i + 2;
                    if j < bytes.len()
                        && (bytes[j] as char).is_ascii_digit()
                        && (bytes[j] as char) != '0'
                    {
                        while j < bytes.len() && (bytes[j] as char).is_ascii_digit() {
                            j += 1;
                        }
                        let num_str = &query[i + 2..j];
                        let n: usize = num_str
                            .parse()
                            .map_err(|_| InterpolateError::UnsupportedArgs)?;
                        if n == 0 || n > args.len() {
                            return Err(InterpolateError::MissingArgs);
                        }
                        encode_value(&mut out, &args[n - 1], Flavor::SQLServer)?;
                        i = j;
                        continue;
                    }
                }
                out.push('@');
                i += 1;
            }
            _ => {
                out.push(c);
                i += 1;
            }
        }
    }

    Ok(out)
}

fn oracle_interpolate(query: &str, args: &[Arg]) -> Result<String, InterpolateError> {
    // Oracle-style interpolate: support :n and :tag: forms, skipping interpolation inside :tag: blocks.
    let mut out = String::with_capacity(query.len() + args.len() * 20);
    let mut quote: Option<char> = None; // '\'' | '"' | ':'(colon-quote)
    let mut escaping = false;
    let mut colon_quote: Option<String> = None;

    let bytes = query.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() {
        let c = bytes[i] as char;

        if escaping {
            out.push(c);
            escaping = false;
            i += 1;
            continue;
        }

        match c {
            '\\' if matches!(quote, Some('\'') | Some('"')) => {
                out.push(c);
                escaping = true;
                i += 1;
            }
            '\'' => {
                if quote == Some('\'') {
                    // Oracle: '' represents a single quote
                    if i + 1 < bytes.len() && bytes[i + 1] as char == '\'' {
                        out.push('\'');
                        out.push('\'');
                        i += 2;
                        continue;
                    }
                    quote = None;
                } else if quote.is_none() {
                    quote = Some('\'');
                }
                out.push('\'');
                i += 1;
            }
            '"' => {
                if quote == Some('"') {
                    quote = None;
                } else if quote.is_none() {
                    quote = Some('"');
                }
                out.push('"');
                i += 1;
            }
            ':' => {
                if quote == Some(':') {
                    if let Some(cq) = &colon_quote
                        && query[i..].starts_with(cq)
                    {
                        out.push_str(cq);
                        i += cq.len();
                        quote = None;
                        colon_quote = None;
                        continue;
                    }
                    out.push(':');
                    i += 1;
                    continue;
                }

                if quote.is_some() {
                    out.push(':');
                    i += 1;
                    continue;
                }

                let mut j = i + 1;
                if j < bytes.len()
                    && (bytes[j] as char).is_ascii_digit()
                    && (bytes[j] as char) != '0'
                {
                    while j < bytes.len() && (bytes[j] as char).is_ascii_digit() {
                        j += 1;
                    }
                    let num_str = &query[i + 1..j];
                    let n: usize = num_str
                        .parse()
                        .map_err(|_| InterpolateError::UnsupportedArgs)?;
                    if n == 0 || n > args.len() {
                        return Err(InterpolateError::MissingArgs);
                    }
                    encode_value(&mut out, &args[n - 1], Flavor::Oracle)?;
                    i = j;
                    continue;
                }

                // colon quote: :tag:
                let mut k = i + 1;
                while k < bytes.len() && (bytes[k] as char).is_ascii_alphabetic() {
                    k += 1;
                }
                if k < bytes.len() && bytes[k] as char == ':' {
                    let cq = &query[i..=k];
                    out.push_str(cq);
                    quote = Some(':');
                    colon_quote = Some(cq.to_string());
                    i = k + 1;
                    continue;
                }

                out.push(':');
                i += 1;
            }
            _ => {
                out.push(c);
                i += 1;
            }
        }
    }

    Ok(out)
}

fn encode_value(out: &mut String, arg: &Arg, flavor: Flavor) -> Result<(), InterpolateError> {
    match arg {
        Arg::Value(v) => encode_sql_value(out, v, flavor),
        Arg::Valuer(v) => {
            let vv = v.value()?;
            encode_sql_value(out, &vv, flavor)
        }
        _ => Err(InterpolateError::UnsupportedArgs),
    }
}

fn encode_sql_value(
    out: &mut String,
    v: &SqlValue,
    flavor: Flavor,
) -> Result<(), InterpolateError> {
    match v {
        SqlValue::Null => out.push_str("NULL"),
        SqlValue::Bool(b) => match flavor {
            Flavor::Oracle => out.push_str(if *b { "1" } else { "0" }),
            _ => out.push_str(if *b { "TRUE" } else { "FALSE" }),
        },
        SqlValue::I64(n) => out.push_str(&n.to_string()),
        SqlValue::U64(n) => out.push_str(&n.to_string()),
        // Rust does not support printf's %g; rely on Display for now (extend if stricter formatting is needed).
        SqlValue::F64(n) => out.push_str(&n.to_string()),
        SqlValue::String(s) => quote_string(out, s.as_ref(), flavor),
        SqlValue::Bytes(b) => encode_bytes(out, b, flavor)?,
        SqlValue::DateTime(dt) => encode_datetime(out, dt, flavor)?,
    }
    Ok(())
}

fn encode_bytes(out: &mut String, data: &[u8], flavor: Flavor) -> Result<(), InterpolateError> {
    if data.is_empty() {
        out.push_str("NULL");
        return Ok(());
    }

    match flavor {
        Flavor::MySQL => {
            out.push_str("_binary");
            quote_string(out, &String::from_utf8_lossy(data), flavor);
        }
        Flavor::PostgreSQL => {
            out.push_str("E'\\\\x");
            push_hex(out, data);
            out.push_str("'::bytea");
        }
        Flavor::SQLite => {
            out.push_str("X'");
            push_hex(out, data);
            out.push('\'');
        }
        Flavor::SQLServer | Flavor::CQL => {
            out.push_str("0x");
            push_hex(out, data);
        }
        Flavor::ClickHouse => {
            out.push_str("unhex('");
            push_hex(out, data);
            out.push_str("')");
        }
        Flavor::Presto => {
            out.push_str("from_hex('");
            push_hex(out, data);
            out.push_str("')");
        }
        Flavor::Oracle => {
            out.push_str("hextoraw('");
            push_hex(out, data);
            out.push_str("')");
        }
        _ => return Err(InterpolateError::UnsupportedArgs),
    }

    Ok(())
}

fn push_hex(out: &mut String, data: &[u8]) {
    const HEX: &[u8; 16] = b"0123456789ABCDEF";
    for &b in data {
        out.push(HEX[((b >> 4) & 0xF) as usize] as char);
        out.push(HEX[(b & 0xF) as usize] as char);
    }
}

fn quote_string(out: &mut String, s: &str, flavor: Flavor) {
    match flavor {
        Flavor::PostgreSQL => out.push('E'),
        Flavor::SQLServer => out.push('N'),
        _ => {}
    }

    out.push('\'');
    for ch in s.chars() {
        match ch {
            '\u{0000}' => out.push_str("\\0"),
            '\u{0008}' => out.push_str("\\b"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{001a}' => out.push_str("\\Z"),
            '\'' => {
                if flavor == Flavor::CQL {
                    out.push_str("''");
                } else {
                    out.push_str("\\'");
                }
            }
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            _ => out.push(ch),
        }
    }
    out.push('\'');
}

fn encode_datetime(
    out: &mut String,
    v: &SqlDateTime,
    flavor: Flavor,
) -> Result<(), InterpolateError> {
    // go: time.Time zero => '0000-00-00'
    // If year==0 treat as zero (time crate disallows year 0; use Unix epoch as the non-zero baseline).
    // Convention: unix_timestamp==0 && nanosecond==0 && tz_abbr None => zero.
    if v.dt.unix_timestamp() == 0 && v.dt.nanosecond() == 0 && v.tz_abbr.is_none() {
        out.push_str("'0000-00-00'");
        return Ok(());
    }

    // Round to microseconds: +500ns
    let dt = v.dt + time::Duration::nanoseconds(500);

    match flavor {
        Flavor::MySQL | Flavor::ClickHouse | Flavor::Informix | Flavor::Doris => {
            // 'YYYY-MM-DD HH:MM:SS.ffffff'
            format_dt(
                out,
                &dt,
                b"'[year]-[month]-[day] [hour]:[minute]:[second].[subsecond digits:6]'",
            );
        }
        Flavor::PostgreSQL => {
            // '... ffffff TZ'
            // Go uses MST abbreviation; here we prefer tz_abbr, fallback to offset.
            format_dt(
                out,
                &dt,
                b"'[year]-[month]-[day] [hour]:[minute]:[second].[subsecond digits:6]'",
            );
            if let Some(abbr) = &v.tz_abbr {
                out.insert(out.len() - 1, ' ');
                out.insert_str(out.len() - 1, abbr.as_ref());
            } else {
                // fallback: +08:00
                let off = dt.offset();
                out.insert(out.len() - 1, ' ');
                out.insert_str(out.len() - 1, &off.to_string());
            }
        }
        Flavor::SQLite | Flavor::Presto => {
            // '... .000'
            format_dt(
                out,
                &dt,
                b"'[year]-[month]-[day] [hour]:[minute]:[second].[subsecond digits:3]'",
            );
        }
        Flavor::SQLServer => {
            // '... ffffff +08:00'
            format_dt(out, &dt, b"'[year]-[month]-[day] [hour]:[minute]:[second].[subsecond digits:6] [offset_hour sign:mandatory]:[offset_minute]'");
        }
        Flavor::CQL => {
            // '... ffffff+0800'
            format_dt(out, &dt, b"'[year]-[month]-[day] [hour]:[minute]:[second].[subsecond digits:6][offset_hour sign:mandatory][offset_minute]'");
        }
        Flavor::Oracle => {
            out.push_str("to_timestamp('");
            let mut tmp = String::new();
            format_dt(
                &mut tmp,
                &dt,
                b"[year]-[month]-[day] [hour]:[minute]:[second].[subsecond digits:6]",
            );
            out.push_str(&tmp);
            out.push_str("', 'YYYY-MM-DD HH24:MI:SS.FF')");
        }
    }

    Ok(())
}

fn format_dt(out: &mut String, dt: &time::OffsetDateTime, fmt: &[u8]) {
    let fmt = std::str::from_utf8(fmt).expect("invalid utf8 format");
    let items: Vec<FormatItem<'_>> =
        time::format_description::parse(fmt).expect("invalid dt format");
    let s = dt.format(&items).expect("format failed");
    out.push_str(&s);
}
