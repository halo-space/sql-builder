#[cfg(test)]
mod tests {
    use crate::Flavor;
    use crate::builder::{build, build_named, buildf, with_flavor};
    use crate::modifiers::{Arg, Builder, SqlNamedArg, list, named, raw};
    use crate::select::SelectBuilder;
    use crate::value::SqlValue;
    use crate::{
        default_flavor, from_tables, insert_cols, select_cols, set_default_flavor_scoped,
        where_exprs,
    };
    use pretty_assertions::assert_eq;
    use std::collections::HashMap;

    #[test]
    fn buildf_basic() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let b = buildf(
            "EXPLAIN SELECT * FROM banned WHERE state IN (%v, %v)",
            [1_i64, 2_i64],
        );
        let (sql, args) = b.build();
        assert_eq!(sql, "EXPLAIN SELECT * FROM banned WHERE state IN (?, ?)");
        assert_eq!(args.len(), 2_usize);
    }

    #[test]
    fn build_named_basic() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let mut m = HashMap::new();
        m.insert(
            "time".to_string(),
            SqlNamedArg::new("start", 1234567890_i64).into(),
        );
        m.insert("status".to_string(), list([1_i64, 2, 5]));
        m.insert("name".to_string(), "Huan%".into());
        m.insert("table".to_string(), raw("user"));

        let b = build_named(
            "SELECT * FROM ${table} WHERE status IN (${status}) AND name LIKE ${name} AND created_at > ${time} AND modified_at < ${time} + 86400",
            m,
        );
        let (sql, _args) = b.build();
        assert_eq!(
            sql,
            "SELECT * FROM user WHERE status IN (?, ?, ?) AND name LIKE ? AND created_at > @start AND modified_at < @start + 86400"
        );
    }

    #[test]
    fn build_named_example_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let mut m = HashMap::new();
        m.insert(
            "time".to_string(),
            SqlNamedArg::new("start", 1234567890_i64).into(),
        );
        m.insert("status".to_string(), list([1_i64, 2, 5]));
        m.insert("name".to_string(), "Huan%".into());
        m.insert("table".to_string(), raw("user"));

        let b = build_named(
            "SELECT * FROM ${table} WHERE status IN (${status}) AND name LIKE ${name} AND created_at > ${time} AND modified_at < ${time} + 86400",
            m,
        );
        let (sql, args) = b.build();
        assert_eq!(
            sql,
            "SELECT * FROM user WHERE status IN (?, ?, ?) AND name LIKE ? AND created_at > @start AND modified_at < @start + 86400"
        );

        let mut values = Vec::new();
        let mut named_start = None;
        for arg in args {
            match arg {
                Arg::Value(v) => values.push(v),
                Arg::SqlNamed(named) => named_start = Some(named),
                other => panic!("unexpected arg {other:?}"),
            }
        }
        assert_eq!(
            values,
            vec![
                SqlValue::I64(1),
                SqlValue::I64(2),
                SqlValue::I64(5),
                SqlValue::String("Huan%".into())
            ]
        );
        let named_start = named_start.expect("named start arg");
        assert_eq!(named_start.name, "start");
        match *named_start.value {
            Arg::Value(SqlValue::I64(v)) => assert_eq!(v, 1234567890),
            other => panic!("unexpected named value {other:?}"),
        }
    }

    #[test]
    fn buildf_example_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let mut sb = SelectBuilder::new();
        select_cols!(sb, "id");
        from_tables!(sb, "user");
        let builder_arg = crate::modifiers::Arg::Builder(Box::new(sb));
        let b = buildf(
            "EXPLAIN %v LEFT JOIN SELECT * FROM banned WHERE state IN (%v, %v)",
            [builder_arg, 1_i64.into(), 2_i64.into()],
        );
        let (sql, args) = b.build();
        assert_eq!(
            sql,
            "EXPLAIN SELECT id FROM user LEFT JOIN SELECT * FROM banned WHERE state IN (?, ?)"
        );
        let values: Vec<SqlValue> = args
            .into_iter()
            .map(|arg| match arg {
                Arg::Value(v) => v,
                other => panic!("unexpected arg {other:?}"),
            })
            .collect();
        assert_eq!(values, vec![SqlValue::I64(1), SqlValue::I64(2)]);
    }

    #[test]
    fn build_example_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let mut sb = SelectBuilder::new();
        select_cols!(sb, "id");
        from_tables!(sb, "user");
        let cond = sb.in_("status", [1_i64, 2_i64]);
        where_exprs!(sb, cond);
        let b = build(
            "EXPLAIN $? LEFT JOIN SELECT * FROM $? WHERE created_at > $? AND state IN (${states}) AND modified_at BETWEEN $2 AND $?",
            [
                crate::modifiers::Arg::Builder(Box::new(sb)),
                raw("banned"),
                1514458225_i64.into(),
                1514544625_i64.into(),
                named("states", list([3_i64, 4, 5])),
            ],
        );
        let (sql, args) = b.build();
        assert_eq!(
            sql,
            "EXPLAIN SELECT id FROM user WHERE status IN (?, ?) LEFT JOIN SELECT * FROM banned WHERE created_at > ? AND state IN (?, ?, ?) AND modified_at BETWEEN ? AND ?"
        );
        assert_eq!(
            Flavor::MySQL.interpolate(&sql, &args).unwrap(),
            "EXPLAIN SELECT id FROM user WHERE status IN (1, 2) LEFT JOIN SELECT * FROM banned WHERE created_at > 1514458225 AND state IN (3, 4, 5) AND modified_at BETWEEN 1514458225 AND 1514544625"
        );
        let values: Vec<SqlValue> = args
            .into_iter()
            .map(|arg| match arg {
                Arg::Value(v) => v,
                other => panic!("unexpected arg {other:?}"),
            })
            .collect();
        assert_eq!(
            values,
            vec![
                SqlValue::I64(1),
                SqlValue::I64(2),
                SqlValue::I64(1514458225),
                SqlValue::I64(3),
                SqlValue::I64(4),
                SqlValue::I64(5),
                SqlValue::I64(1514458225),
                SqlValue::I64(1514544625),
            ]
        );
    }

    #[test]
    fn with_flavor_overrides_default() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let b = with_flavor(
            buildf("SELECT * FROM foo WHERE id = %v", [1234_i64]),
            Flavor::PostgreSQL,
        );
        let (sql, args) = b.build();
        assert_eq!(sql, "SELECT * FROM foo WHERE id = $1");
        assert_eq!(args.len(), 1);

        let (sql2, _args2) = b.build_with_flavor(Flavor::MySQL, &[]);
        assert_eq!(sql2, "SELECT * FROM foo WHERE id = ?");
        let (sql3, _args3) = with_flavor(
            buildf("SELECT * FROM foo WHERE id = %v", [1234_i64]),
            Flavor::Informix,
        )
        .build();
        assert_eq!(sql3, "SELECT * FROM foo WHERE id = ?");
    }

    #[test]
    fn builder_get_flavor() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let b1 = build("SELECT * FROM foo WHERE id = $0", [1234_i64]);
        assert_eq!(b1.flavor(), default_flavor());

        let b2 = buildf("SELECT * FROM foo WHERE id = %v", [1234_i64]);
        assert_eq!(b2.flavor(), default_flavor());

        let mut m = HashMap::new();
        m.insert("table".to_string(), "foo".into());
        let b3 = build_named("SELECT * FROM ${table} WHERE id = 1234", m);
        assert_eq!(b3.flavor(), default_flavor());

        let b4 = with_flavor(
            build("SELECT * FROM foo WHERE id = $0", [1234_i64]),
            Flavor::PostgreSQL,
        );
        assert_eq!(b4.flavor(), Flavor::PostgreSQL);
    }

    #[test]
    fn default_flavor_is_global() {
        let _g = set_default_flavor_scoped(Flavor::PostgreSQL);
        let b = buildf("SELECT * FROM foo WHERE id = %v", [1234_i64]);
        let (sql, _args) = b.build();
        assert_eq!(sql, "SELECT * FROM foo WHERE id = $1");
    }

    #[test]
    fn build_with_postgresql_builders_respects_outer_default_flavor() {
        // Nested builders: inner flavor should not override outer default flavor.
        {
            let _g = set_default_flavor_scoped(Flavor::MySQL);

            let mut sb1 = crate::select::SelectBuilder::new();
            sb1.set_flavor(Flavor::PostgreSQL);
            select_cols!(sb1, "col1", "col2");
            from_tables!(sb1, "t1");
            let w11 = sb1.e("id", 1234_i64);
            let w12 = sb1.g("level", 2_i64);
            where_exprs!(sb1, w11, w12);

            let mut sb2 = crate::select::SelectBuilder::new();
            sb2.set_flavor(Flavor::PostgreSQL);
            select_cols!(sb2, "col3", "col4");
            from_tables!(sb2, "t2");
            let w21 = sb2.e("id", 4567_i64);
            let w22 = sb2.le("level", 5_i64);
            where_exprs!(sb2, w21, w22);

            let (sql, args) = build(
                "SELECT $1 AS col5 LEFT JOIN $0 LEFT JOIN $2",
                vec![
                    crate::modifiers::Arg::Builder(Box::new(sb1)),
                    7890_i64.into(),
                    crate::modifiers::Arg::Builder(Box::new(sb2)),
                ],
            )
            .build();
            assert_eq!(
                sql,
                "SELECT ? AS col5 LEFT JOIN SELECT col1, col2 FROM t1 WHERE id = ? AND level > ? LEFT JOIN SELECT col3, col4 FROM t2 WHERE id = ? AND level <= ?"
            );
            assert_eq!(args.len(), 5);
        }

        {
            // Note: previous guard must drop before locking again, otherwise same-thread double lock deadlocks.
            let _g = set_default_flavor_scoped(Flavor::PostgreSQL);

            let mut sb1 = crate::select::SelectBuilder::new();
            sb1.set_flavor(Flavor::PostgreSQL);
            select_cols!(sb1, "col1", "col2");
            from_tables!(sb1, "t1");
            let w11 = sb1.e("id", 1234_i64);
            let w12 = sb1.g("level", 2_i64);
            where_exprs!(sb1, w11, w12);

            let mut sb2 = crate::select::SelectBuilder::new();
            sb2.set_flavor(Flavor::PostgreSQL);
            select_cols!(sb2, "col3", "col4");
            from_tables!(sb2, "t2");
            let w21 = sb2.e("id", 4567_i64);
            let w22 = sb2.le("level", 5_i64);
            where_exprs!(sb2, w21, w22);

            let (sql, args) = build(
                "SELECT $1 AS col5 LEFT JOIN $0 LEFT JOIN $2",
                vec![
                    crate::modifiers::Arg::Builder(Box::new(sb1)),
                    7890_i64.into(),
                    crate::modifiers::Arg::Builder(Box::new(sb2)),
                ],
            )
            .build();
            assert_eq!(
                sql,
                "SELECT $1 AS col5 LEFT JOIN SELECT col1, col2 FROM t1 WHERE id = $2 AND level > $3 LEFT JOIN SELECT col3, col4 FROM t2 WHERE id = $4 AND level <= $5"
            );
            assert_eq!(args.len(), 5);
        }
    }

    #[test]
    fn build_with_cql_nested_insert_builders() {
        // Mirror Go `TestBuildWithCQL`.
        let _g = set_default_flavor_scoped(Flavor::CQL);

        let mut ib1 = crate::insert::InsertBuilder::new();
        ib1.set_flavor(Flavor::CQL);
        ib1.insert_into("t1");
        insert_cols!(ib1, "col1", "col2").values([1_i64, 2_i64]);

        let mut ib2 = crate::insert::InsertBuilder::new();
        ib2.set_flavor(Flavor::CQL);
        ib2.insert_into("t2");
        insert_cols!(ib2, "col3", "col4").values([3_i64, 4_i64]);

        let (sql, args) = build(
            "BEGIN BATCH USING TIMESTAMP $0 $1; $2; APPLY BATCH;",
            [
                1481124356754405_i64.into(),
                crate::modifiers::Arg::Builder(Box::new(ib1)),
                crate::modifiers::Arg::Builder(Box::new(ib2)),
            ],
        )
        .build();

        assert_eq!(
            sql,
            "BEGIN BATCH USING TIMESTAMP ? INSERT INTO t1 (col1, col2) VALUES (?, ?); INSERT INTO t2 (col3, col4) VALUES (?, ?); APPLY BATCH;"
        );
        assert_eq!(args.len(), 5);
    }

    #[test]
    fn build_literal_dollar_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let b = build("price is $$ $0", [123_i64]);
        let (sql, args) = b.build();

        assert_eq!(sql, "price is $ ?");
        let values: Vec<SqlValue> = args
            .iter()
            .map(|arg| match arg {
                Arg::Value(v) => v.clone(),
                other => panic!("unexpected arg {other:?}"),
            })
            .collect();
        assert_eq!(values, vec![SqlValue::I64(123)]);
    }

    #[test]
    fn build_named_reuse_same_arg() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let mut map = HashMap::new();
        map.insert("value".to_string(), SqlNamedArg::new("foo", 999_i64).into());

        let (sql, args) = build_named("${value} ${value}", map).build();
        assert_eq!(sql, "@foo @foo");
        assert_eq!(args.len(), 1);

        match &args[0] {
            Arg::SqlNamed(named) => {
                assert_eq!(named.name, "foo");
                match *named.value.clone() {
                    Arg::Value(SqlValue::I64(v)) => assert_eq!(v, 999),
                    other => panic!("unexpected named value {other:?}"),
                }
            }
            other => panic!("unexpected arg {other:?}"),
        }
    }

    #[test]
    fn select_builder_named_args_like_readme() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let now = 1_514_458_225_i64;
        let start = SqlNamedArg::new("start", now - 86400);
        let end = SqlNamedArg::new("end", now + 86400);

        let mut sb = SelectBuilder::new();
        select_cols!(sb, "name");
        from_tables!(sb, "user");
        let between = sb.between("created_at", start.clone(), end.clone());
        let ge = sb.greater_equal_than("modified_at", start.clone());
        where_exprs!(sb, between, ge);

        let (sql, args) = sb.build();
        assert_eq!(
            sql,
            "SELECT name FROM user WHERE created_at BETWEEN @start AND @end AND modified_at >= @start"
        );

        assert_eq!(args.len(), 2);
        match &args[0] {
            Arg::SqlNamed(named) => assert_eq!(named.name, "start"),
            other => panic!("unexpected arg {other:?}"),
        }
        match &args[1] {
            Arg::SqlNamed(named) => assert_eq!(named.name, "end"),
            other => panic!("unexpected arg {other:?}"),
        }
    }
}
