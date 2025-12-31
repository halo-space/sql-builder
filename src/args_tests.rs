#[cfg(test)]
mod tests {
    use crate::args::Args;
    use crate::flavor::{Flavor, set_default_flavor_scoped};
    use crate::modifiers::{Arg, SqlNamedArg, named, raw};
    use crate::value::SqlValue;
    use pretty_assertions::assert_eq;

    fn to_postgresql(sql: &str) -> String {
        // Same as Go's toPostgreSQL: replace '?' with $1..$n in order
        let parts: Vec<&str> = sql.split('?').collect();
        if parts.len() == 1 {
            return sql.to_string();
        }
        let mut out = String::new();
        out.push_str(parts[0]);
        for (i, p) in parts.iter().enumerate().skip(1) {
            out.push('$');
            out.push_str(&(i.to_string()));
            out.push_str(p);
        }
        out
    }

    fn to_sqlserver(sql: &str) -> String {
        let parts: Vec<&str> = sql.split('?').collect();
        if parts.len() == 1 {
            return sql.to_string();
        }
        let mut out = String::new();
        out.push_str(parts[0]);
        for (i, p) in parts.iter().enumerate().skip(1) {
            out.push_str(&format!("@p{i}"));
            out.push_str(p);
        }
        out
    }

    #[test]
    fn args_compile_cases_mysql_like() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);

        let start = Arg::SqlNamed(SqlNamedArg::new("start", 1234567890_i64));
        let end = Arg::SqlNamed(SqlNamedArg::new("end", 1234599999_i64));
        let named1 = named("named1", "foo");
        let named2 = named("named2", "bar");

        let cases: Vec<(&str, Vec<Arg>, &str, Vec<Arg>)> = vec![
            (
                "abc $? def",
                vec![123_i64.into()],
                "abc ? def",
                vec![123_i64.into()],
            ),
            (
                "abc $0 def",
                vec![456_i64.into()],
                "abc ? def",
                vec![456_i64.into()],
            ),
            (
                "abc $1 def",
                vec![123_i64.into()],
                "abc /* INVALID ARG $1 */ def",
                vec![],
            ),
            (
                "abc ${unknown} def ",
                vec![123_i64.into()],
                "abc  def ",
                vec![],
            ),
            ("abc $$ def", vec![123_i64.into()], "abc $ def", vec![]),
            ("abcdef$", vec![123_i64.into()], "abcdef$", vec![]),
            (
                "abc $? $? $0 $? def",
                vec![123_i64.into(), 456_i64.into(), 789_i64.into()],
                "abc ? ? ? ? def",
                vec![
                    123_i64.into(),
                    456_i64.into(),
                    123_i64.into(),
                    456_i64.into(),
                ],
            ),
            (
                "abc $? $? $0 $? def",
                vec![123_i64.into(), raw("raw"), 789_i64.into()],
                "abc ? raw ? raw def",
                vec![123_i64.into(), 123_i64.into()],
            ),
            (
                "abc $-1 $a def",
                vec![123_i64.into()],
                "abc $-1 $a def",
                vec![],
            ),
            (
                "abc ${named1} def ${named2} ${named1}",
                vec![named2.clone(), named1.clone(), named2.clone()],
                "abc ? def ? ?",
                vec!["foo".into(), "bar".into(), "foo".into()],
            ),
            (
                "$? $? $?",
                vec![end.clone(), start.clone(), end.clone()],
                "@end @start @end",
                vec![
                    Arg::SqlNamed(SqlNamedArg::new("end", 1234599999_i64)),
                    Arg::SqlNamed(SqlNamedArg::new("start", 1234567890_i64)),
                ],
            ),
        ];

        for (fmt, args_in, expected_sql, expected_args) in cases {
            let mut a = Args::default();
            for v in args_in {
                a.add(v);
            }
            let (sql, args) = a.compile(fmt, &[]);
            assert_eq!(sql, expected_sql);
            assert_eq!(args, expected_args);
        }
    }

    #[test]
    fn args_compile_cases_other_flavors() {
        let cases: Vec<(&str, Vec<Arg>, &str)> = vec![
            ("abc $? def", vec![123_i64.into()], "abc ? def"),
            ("abc $0 def", vec![456_i64.into()], "abc ? def"),
            (
                "abc $? $? $0 $? def",
                vec![123_i64.into(), 456_i64.into(), 789_i64.into()],
                "abc ? ? ? ? def",
            ),
        ];

        for &(flavor, conv) in &[
            (Flavor::PostgreSQL, to_postgresql as fn(&str) -> String),
            (Flavor::SQLServer, to_sqlserver as fn(&str) -> String),
            (Flavor::CQL, |s: &str| s.to_string()),
        ] {
            let _g = set_default_flavor_scoped(flavor);
            for (fmt, args_in, expected_mysql_sql) in &cases {
                let mut a = Args::default();
                for v in args_in.iter().cloned() {
                    a.add(v);
                }
                let (sql, _args) = a.compile(fmt, &[]);
                assert_eq!(sql, conv(expected_mysql_sql));
            }
        }
    }

    #[test]
    fn args_add_returns_dollar_index() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let mut a = Args::default();
        for i in 0..10 {
            assert_eq!(a.add(i as i64), format!("${i}"));
        }
    }

    #[test]
    fn args_value_parses_prefix() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let mut a = Args::default();
        let v1 = 123_i64;
        let p = a.add(v1);
        assert_eq!(a.value(&p), Some(&Arg::Value(SqlValue::I64(v1))));
        assert_eq!(a.value("invalid"), None);
        assert_eq!(
            a.value(&(p + "something else")),
            Some(&Arg::Value(SqlValue::I64(v1)))
        );
    }
}
