#[cfg(test)]
mod tests {
    use crate::args::Args;
    use crate::cond::Cond;
    use crate::flavor::Flavor;
    use crate::modifiers::Builder;
    use crate::{from_tables, select_cols, where_exprs};
    use pretty_assertions::assert_eq;
    use std::cell::RefCell;
    use std::rc::Rc;

    struct TestPair {
        expected: &'static str,
        actual: String,
    }

    fn new_test_pair(expected: &'static str, f: impl FnOnce(&Cond) -> String) -> TestPair {
        // Match Go cond_test.go newCond(): Args{} with index_base=0.
        let args = Rc::new(RefCell::new(Args::default()));
        let cond = Cond::with_args(args.clone());
        let fmt = f(&cond);
        let (sql, _) = args
            .borrow()
            .compile_with_flavor(&fmt, Flavor::PostgreSQL, &[]);
        TestPair {
            expected,
            actual: sql,
        }
    }

    #[test]
    fn cond_basic_like_go() {
        let cases = vec![
            new_test_pair("$a = $1", |c| c.equal("$a", 123)),
            new_test_pair("$b = $1", |c| c.e("$b", 123)),
            new_test_pair("$c = $1", |c| c.eq("$c", 123)),
            new_test_pair("$a <> $1", |c| c.not_equal("$a", 123)),
            new_test_pair("$b <> $1", |c| c.ne("$b", 123)),
            new_test_pair("$c <> $1", |c| c.neq("$c", 123)),
            new_test_pair("$a > $1", |c| c.greater_than("$a", 123)),
            new_test_pair("$b > $1", |c| c.g("$b", 123)),
            new_test_pair("$c > $1", |c| c.gt("$c", 123)),
            new_test_pair("$a >= $1", |c| c.greater_equal_than("$a", 123)),
            new_test_pair("$b >= $1", |c| c.ge("$b", 123)),
            new_test_pair("$c >= $1", |c| c.gte("$c", 123)),
            new_test_pair("$a < $1", |c| c.less_than("$a", 123)),
            new_test_pair("$b < $1", |c| c.l("$b", 123)),
            new_test_pair("$c < $1", |c| c.lt("$c", 123)),
            new_test_pair("$a <= $1", |c| c.less_equal_than("$a", 123)),
            new_test_pair("$b <= $1", |c| c.le("$b", 123)),
            new_test_pair("$c <= $1", |c| c.lte("$c", 123)),
            new_test_pair("$a IN ($1, $2, $3)", |c| c.in_("$a", [1, 2, 3])),
            new_test_pair("0 = 1", |c| c.in_("$a", Vec::<i64>::new())),
            new_test_pair("$a NOT IN ($1, $2, $3)", |c| c.not_in("$a", [1, 2, 3])),
            new_test_pair("0 = 0", |c| c.not_in("$a", Vec::<i64>::new())),
            new_test_pair("$a LIKE $1", |c| c.like("$a", "%Huan%")),
            new_test_pair("$a ILIKE $1", |c| c.ilike("$a", "%Huan%")),
            new_test_pair("$a NOT LIKE $1", |c| c.not_like("$a", "%Huan%")),
            new_test_pair("$a NOT ILIKE $1", |c| c.not_ilike("$a", "%Huan%")),
            new_test_pair("$a IS NULL", |c| c.is_null("$a")),
            new_test_pair("$a IS NOT NULL", |c| c.is_not_null("$a")),
            new_test_pair("$a BETWEEN $1 AND $2", |c| c.between("$a", 123, 456)),
            new_test_pair("$a NOT BETWEEN $1 AND $2", |c| {
                c.not_between("$a", 123, 456)
            }),
            new_test_pair("NOT 1 = 1", |c| c.not("1 = 1")),
            new_test_pair("EXISTS ($1)", |c| c.exists(1)),
            new_test_pair("NOT EXISTS ($1)", |c| c.not_exists(1)),
            new_test_pair("$a > ANY ($1, $2)", |c| c.any("$a", ">", [1, 2])),
            new_test_pair("0 = 1", |c| c.any("$a", ">", Vec::<i64>::new())),
            new_test_pair("$a < ALL ($1)", |c| c.all("$a", "<", [1])),
            new_test_pair("0 = 1", |c| c.all("$a", "<", Vec::<i64>::new())),
            new_test_pair("$a > SOME ($1, $2, $3)", |c| c.some("$a", ">", [1, 2, 3])),
            new_test_pair("0 = 1", |c| c.some("$a", ">", Vec::<i64>::new())),
            new_test_pair("$a IS DISTINCT FROM $1", |c| c.is_distinct_from("$a", 1)),
            new_test_pair("$a IS NOT DISTINCT FROM $1", |c| {
                c.is_not_distinct_from("$a", 1)
            }),
            new_test_pair("$1", |c| c.var(123)),
        ];

        for c in cases {
            assert_eq!(c.actual, c.expected);
        }
    }

    #[test]
    fn cond_or_and_empty_rules_like_go() {
        let args = Rc::new(RefCell::new(Args::default()));
        let cond = Cond::with_args(args);
        assert_eq!(cond.or([""]), "");
        assert_eq!(cond.or(["", "", ""]), "");
        assert_eq!(cond.and([""]), "");
        assert_eq!(cond.and(["", "", ""]), "");

        assert_eq!(cond.or(["", "1 = 1", "2 = 2"]), "(1 = 1 OR 2 = 2)");
        assert_eq!(cond.and(["", "1 = 1", "2 = 2"]), "(1 = 1 AND 2 = 2)");
    }

    #[test]
    fn cond_empty_field_like_go() {
        let cond = Cond::new(); // NewCond: empty field yields "".
        let cases = vec![
            cond.equal("", 123),
            cond.not_equal("", 123),
            cond.greater_than("", 123),
            cond.greater_equal_than("", 123),
            cond.less_than("", 123),
            cond.less_equal_than("", 123),
            cond.in_("", [1, 2, 3]),
            cond.not_in("", [1, 2, 3]),
            cond.like("", "%Huan%"),
            cond.ilike("", "%Huan%"),
            cond.not_like("", "%Huan%"),
            cond.not_ilike("", "%Huan%"),
            cond.is_null(""),
            cond.is_not_null(""),
            cond.between("", 123, 456),
            cond.not_between("", 123, 456),
            cond.not(""),
            cond.any("", "", [1, 2]),
            cond.any("", ">", [1, 2]),
            cond.any("$a", "", [1, 2]),
            cond.all("", "", [1]),
            cond.all("", ">", [1]),
            cond.all("$a", "", [1]),
            cond.some("", "", [1, 2, 3]),
            cond.some("", ">", [1, 2, 3]),
            cond.some("$a", "", [1, 2, 3]),
            cond.is_distinct_from("", 1),
            cond.is_not_distinct_from("", 1),
        ];
        for actual in cases {
            assert_eq!(actual, "");
        }
    }

    #[test]
    fn cond_with_flavor_like_go() {
        let args = Rc::new(RefCell::new(Args::default()));
        let cond = Cond::with_args(args.clone());
        let fmt = [
            cond.ilike("f1", 1),
            cond.not_ilike("f2", 2),
            cond.is_distinct_from("f3", 3),
            cond.is_not_distinct_from("f4", 4),
        ]
        .join("\n");

        let expected_pg =
            "f1 ILIKE $1\nf2 NOT ILIKE $2\nf3 IS DISTINCT FROM $3\nf4 IS NOT DISTINCT FROM $4";
        let expected_mysql =
            "LOWER(f1) LIKE LOWER(?)\nLOWER(f2) NOT LIKE LOWER(?)\nNOT f3 <=> ?\nf4 <=> ?";
        let expected_sqlite =
            "f1 ILIKE ?\nf2 NOT ILIKE ?\nf3 IS DISTINCT FROM ?\nf4 IS NOT DISTINCT FROM ?";
        let expected_presto = "LOWER(f1) LIKE LOWER(?)\nLOWER(f2) NOT LIKE LOWER(?)\nCASE WHEN f3 IS NULL AND ? IS NULL THEN 0 WHEN f3 IS NOT NULL AND ? IS NOT NULL AND f3 = ? THEN 0 ELSE 1 END = 1\nCASE WHEN f4 IS NULL AND ? IS NULL THEN 1 WHEN f4 IS NOT NULL AND ? IS NOT NULL AND f4 = ? THEN 1 ELSE 0 END = 1";

        let (actual_pg, _) = args
            .borrow()
            .compile_with_flavor(&fmt, Flavor::PostgreSQL, &[]);
        assert_eq!(actual_pg, expected_pg);

        let (actual_mysql, _) = args.borrow().compile_with_flavor(&fmt, Flavor::MySQL, &[]);
        assert_eq!(actual_mysql, expected_mysql);

        let (actual_sqlite, _) = args.borrow().compile_with_flavor(&fmt, Flavor::SQLite, &[]);
        assert_eq!(actual_sqlite, expected_sqlite);

        let (actual_presto, _) = args.borrow().compile_with_flavor(&fmt, Flavor::Presto, &[]);
        assert_eq!(actual_presto, expected_presto);
    }

    #[test]
    fn cond_expr_like_go() {
        let args = Rc::new(RefCell::new(Args::default()));
        let cond = Cond::with_args(args.clone());

        let sb1 = crate::builder::build("SELECT 1 = 1", Vec::<crate::modifiers::Arg>::new());
        let sb2 = crate::builder::build("SELECT FALSE", Vec::<crate::modifiers::Arg>::new());

        let fmts = vec![
            cond.and(Vec::<String>::new()),
            cond.or(Vec::<String>::new()),
            cond.and([cond.var(sb1.clone()), cond.var(sb2.clone())]),
            cond.or([cond.var(sb1.clone()), cond.var(sb2.clone())]),
            cond.not(cond.or([
                cond.var(sb1.clone()),
                cond.and([cond.var(sb1), cond.var(sb2)]),
            ])),
        ];

        let expect = vec![
            "",
            "",
            "(SELECT 1 = 1 AND SELECT FALSE)",
            "(SELECT 1 = 1 OR SELECT FALSE)",
            "NOT (SELECT 1 = 1 OR (SELECT 1 = 1 AND SELECT FALSE))",
        ];

        for (fmt, expected) in fmts.into_iter().zip(expect) {
            let (actual, values) = args.borrow().compile(&fmt, &[]);
            assert_eq!(values.len(), 0);
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn cond_misuse_like_go() {
        let cond = Cond::new(); // index_base=256
        let mut sb = crate::select::SelectBuilder::new();
        select_cols!(sb, "*");
        from_tables!(sb, "t1");
        where_exprs!(sb, cond.equal("a", 123));
        let (sql, args) = sb.build();
        assert_eq!(sql, "SELECT * FROM t1 WHERE /* INVALID ARG $256 */");
        assert_eq!(args.len(), 0);
    }
}
