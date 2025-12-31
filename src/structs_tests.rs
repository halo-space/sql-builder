#[cfg(test)]
mod tests {
    use crate::Struct;
    use crate::field_mapper::{
        identity_mapper, kebab_case_mapper, prefix_mapper, set_default_field_mapper,
        set_default_field_mapper_scoped, snake_case_mapper, suffix_mapper, upper_case_mapper,
    };
    use crate::flavor::{Flavor, set_default_flavor_scoped};
    use crate::modifiers::Builder;
    use crate::scan_tokens;
    use pretty_assertions::assert_eq;

    #[derive(Clone, Default)]
    struct StructWithQuote {
        a: String,
        c: f64,
    }

    crate::sql_struct! {
        impl StructWithQuote {
            a: { db: "aa",  tags: [], omitempty: [], quote: true,  as: None },
            c: { db: "ccc", tags: [], omitempty: [], quote: false, as: None },
        }
    }

    #[test]
    fn struct_with_quote_select_update_insert_mysql_pg_cql() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        // SELECT
        let st = Struct::<StructWithQuote>::new().for_flavor(Flavor::MySQL);
        let (sql, _) = st.select_from("foo").build();
        assert_eq!(sql, "SELECT foo.`aa`, foo.ccc FROM foo");

        let st = Struct::<StructWithQuote>::new().for_flavor(Flavor::PostgreSQL);
        let (sql, _) = st.select_from("foo").build();
        assert_eq!(sql, r#"SELECT foo."aa", foo.ccc FROM foo"#);

        let st = Struct::<StructWithQuote>::new().for_flavor(Flavor::CQL);
        let (sql, _) = st.select_from("foo").build();
        assert_eq!(sql, "SELECT 'aa', ccc FROM foo");

        // UPDATE
        let v = StructWithQuote {
            a: "aaa".to_string(),
            c: 0.0,
        };
        let st = Struct::<StructWithQuote>::new().for_flavor(Flavor::MySQL);
        let (sql, _) = st.update("foo", &v).build();
        assert_eq!(sql, "UPDATE foo SET `aa` = ?, ccc = ?");

        let st = Struct::<StructWithQuote>::new().for_flavor(Flavor::PostgreSQL);
        let (sql, _) = st.update("foo", &v).build();
        assert_eq!(sql, r#"UPDATE foo SET "aa" = $1, ccc = $2"#);

        let st = Struct::<StructWithQuote>::new().for_flavor(Flavor::CQL);
        let (sql, _) = st.update("foo", &v).build();
        assert_eq!(sql, "UPDATE foo SET 'aa' = ?, ccc = ?");

        // INSERT
        let st = Struct::<StructWithQuote>::new().for_flavor(Flavor::MySQL);
        let (sql, _) = st.insert_into("foo", [&v]).build();
        assert_eq!(sql, "INSERT INTO foo (`aa`, ccc) VALUES (?, ?)");

        let st = Struct::<StructWithQuote>::new().for_flavor(Flavor::PostgreSQL);
        let (sql, _) = st.insert_into("foo", [&v]).build();
        assert_eq!(sql, r#"INSERT INTO foo ("aa", ccc) VALUES ($1, $2)"#);

        let st = Struct::<StructWithQuote>::new().for_flavor(Flavor::CQL);
        let (sql, _) = st.insert_into("foo", [&v]).build();
        assert_eq!(sql, "INSERT INTO foo ('aa', ccc) VALUES (?, ?)");
    }

    #[derive(Clone, Default)]
    struct StructOmitEmpty {
        a: i64,
        b: Option<String>,
        c: u16,
        d: Option<f64>,
        e: bool,
    }

    crate::sql_struct! {
        impl StructOmitEmpty {
            a: { db: "aa", tags: [], omitempty: [""], quote: true,  as: None },
            b: { db: "bb", tags: [], omitempty: [""], quote: false, as: None },
            c: { db: "cc", tags: [], omitempty: [""], quote: false, as: None },
            d: { db: "D",  tags: [], omitempty: [""], quote: false, as: None },
            e: { db: "ee", tags: [], omitempty: [],   quote: false, as: None },
        }
    }

    #[test]
    fn struct_omit_empty_default_tag() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let st = Struct::<StructOmitEmpty>::new().for_flavor(Flavor::MySQL);
        let (sql, _) = st.update("foo", &StructOmitEmpty::default()).build();
        assert_eq!(sql, "UPDATE foo SET ee = ?");
    }

    #[derive(Clone, Default)]
    struct Tags {
        a: i64,
        b: i64,
        c: i64,
        d: i64,
        e: i64,
        f: i64,
        g: i64,
        h: i64,
    }

    crate::sql_struct! {
        impl Tags {
            a: { db: "a", tags: ["tag1"], omitempty: [], quote: false, as: None },
            b: { db: "b", tags: ["tag2"], omitempty: [], quote: false, as: None },
            c: { db: "c", tags: ["tag3"], omitempty: [], quote: false, as: None },
            d: { db: "d", tags: ["tag1","tag2"], omitempty: [], quote: false, as: None },
            e: { db: "e", tags: ["tag2","tag3"], omitempty: [], quote: false, as: None },
            f: { db: "f", tags: ["tag1","tag3"], omitempty: [], quote: false, as: None },
            g: { db: "g", tags: ["tag1","tag2","tag3"], omitempty: [], quote: false, as: None },
            h: { db: "h", tags: [], omitempty: [], quote: false, as: None },
        }
    }

    #[test]
    fn struct_with_and_without_tags_columns_order() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let st = Struct::<Tags>::new();

        assert_eq!(st.columns(), vec!["a", "b", "c", "d", "e", "f", "g", "h"]);
        assert_eq!(
            st.with_tag([]).columns(),
            vec!["a", "b", "c", "d", "e", "f", "g", "h"]
        );
        assert_eq!(
            st.without_tag([]).columns(),
            vec!["a", "b", "c", "d", "e", "f", "g", "h"]
        );
        assert_eq!(
            st.with_tag([""]).columns(),
            vec!["a", "b", "c", "d", "e", "f", "g", "h"]
        );
        assert_eq!(
            st.without_tag([""]).columns(),
            vec!["a", "b", "c", "d", "e", "f", "g", "h"]
        );

        assert_eq!(st.with_tag(["tag1"]).columns(), vec!["a", "d", "f", "g"]);
        assert_eq!(st.with_tag(["tag2"]).columns(), vec!["b", "d", "e", "g"]);
        assert_eq!(st.with_tag(["tag3"]).columns(), vec!["c", "e", "f", "g"]);

        assert_eq!(
            st.with_tag(["tag1", "tag2"]).columns(),
            vec!["a", "d", "f", "g", "b", "e"]
        );
        assert_eq!(
            st.with_tag(["tag1", "tag3"]).columns(),
            vec!["a", "d", "f", "g", "c", "e"]
        );
        assert_eq!(
            st.with_tag(["tag2", "tag3"]).columns(),
            vec!["b", "d", "e", "g", "c", "f"]
        );
        assert_eq!(
            st.with_tag(["tag2", "tag3", "tag2", "", "tag3"]).columns(),
            vec!["b", "d", "e", "g", "c", "f"]
        );

        assert_eq!(st.without_tag(["tag3"]).columns(), vec!["a", "b", "d", "h"]);
        assert_eq!(st.without_tag(["tag3", "tag2"]).columns(), vec!["a", "h"]);
        assert_eq!(
            st.without_tag(["tag3", "tag2", "tag3", "", "tag2"])
                .columns(),
            vec!["a", "h"]
        );

        assert_eq!(
            st.with_tag(["tag1", "tag2"])
                .without_tag(["tag3"])
                .columns(),
            vec!["a", "d", "b"]
        );
        assert_eq!(
            st.without_tag(["tag3"])
                .with_tag(["tag1", "tag2"])
                .columns(),
            vec!["a", "d", "b"]
        );
        assert_eq!(
            st.with_tag(["tag1", "tag2", "tag3"])
                .without_tag(["tag3"])
                .columns(),
            vec!["a", "d", "b"]
        );
        assert_eq!(
            st.without_tag(["tag3", "tag1"])
                .with_tag(["tag1", "tag2", "tag3"])
                .columns(),
            vec!["b"]
        );

        assert_eq!(
            st.with_tag(["tag2"]).with_tag(["tag1"]).columns(),
            vec!["a", "d", "f", "g", "b", "e"]
        );
        assert_eq!(
            st.without_tag(["tag3"])
                .with_tag(["tag1"])
                .with_tag(["tag3", "", "tag2"])
                .columns(),
            vec!["a", "d", "b"]
        );
        assert_eq!(
            st.without_tag(["tag3"])
                .with_tag(["tag1"])
                .with_tag(["tag3", "tag2"])
                .without_tag(["tag1", "", "tag3"])
                .columns(),
            vec!["b"]
        );
    }

    #[derive(Clone, Default)]
    struct OmitForInsert {
        a: i64,
        c: u16,
        e: bool,
    }

    crate::sql_struct! {
        impl OmitForInsert {
            a: { db: "aa", tags: ["patch2"], omitempty: ["", "patch2"], quote: true,  as: None },
            c: { db: "cc", tags: ["patch2"], omitempty: ["", "patch2"], quote: false, as: None },
            e: { db: "ee", tags: ["patch"],  omitempty: [],          quote: false, as: None },
        }
    }

    #[test]
    fn struct_insert_filters_all_empty_columns_when_omitempty() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let st = Struct::<OmitForInsert>::new().with_tag(["patch2"]);

        let v1 = OmitForInsert {
            a: 123,
            c: 0,
            e: false,
        };
        let v2 = OmitForInsert {
            a: 123,
            c: 2,
            e: true,
        };

        let (sql, args) = st.insert_into("foo", [&v1, &v2]).build();
        // Ensure `cc` is not omitted (second row non-empty) and `ee` not in tag=patch2 set.
        assert_eq!(sql, "INSERT INTO foo (`aa`, cc) VALUES (?, ?), (?, ?)");
        assert_eq!(args.len(), 4);
    }

    #[derive(Clone, Default)]
    struct OmitEmptyForTag {
        a: i64,
        b: Option<String>,
        c: u16,
        d: Option<f64>,
        e: bool,
    }

    crate::sql_struct! {
        impl OmitEmptyForTag {
            // A/B: omitempty default ("") => always active
            a: { db: "aa", tags: ["patch"], omitempty: [""], quote: true,  as: None },
            b: { db: "bb", tags: ["patch"], omitempty: [""], quote: false, as: None },
            // C: omitempty() == default tag
            c: { db: "cc", tags: ["patch"], omitempty: [""], quote: false, as: None },
            // D: omitempty(patch) only active with WithTag(patch)
            d: { db: "D",  tags: ["patch"], omitempty: ["patch"], quote: false, as: None },
            e: { db: "ee", tags: ["patch"], omitempty: [],        quote: false, as: None },
        }
    }

    #[test]
    fn struct_omit_empty_for_tag_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let st = Struct::<OmitEmptyForTag>::new();

        // Without WithTag: only default-tag fields (A/B/C) are omitted; D stays.
        let (sql1, _) = st.update("foo", &OmitEmptyForTag::default()).build();
        assert_eq!(sql1, "UPDATE foo SET D = ?, ee = ?");

        // WithTag("patch"): A/B/C/D all omit empty values.
        let v = OmitEmptyForTag {
            a: 123,
            b: Some("bbbb".to_string()),
            c: 234,
            d: None,
            e: true,
        };
        let (sql2, args2) = st.with_tag(["patch"]).update("foo", &v).build();
        assert_eq!(sql2, "UPDATE foo SET `aa` = ?, bb = ?, cc = ?, ee = ?");
        assert_eq!(args2.len(), 4);
    }

    #[derive(Clone, Default)]
    struct OmitEmptyForMultipleTags {
        a: i64,
        b: Option<String>,
        c: u16,
        d: Option<f64>,
        e: bool,
    }

    crate::sql_struct! {
        impl OmitEmptyForMultipleTags {
            // Equivalent to: omitempty + omitempty(patch, patch2)
            a: { db: "aa", tags: ["patch","patch2"], omitempty: ["", "patch", "patch2"], quote: true,  as: None },
            b: { db: "bb", tags: ["patch"],         omitempty: [""],                 quote: false, as: None },
            // Equivalent to: omitempty + omitempty(patch2)
            c: { db: "cc", tags: ["patch2"],        omitempty: ["", "patch2"],       quote: false, as: None },
            d: { db: "D",  tags: ["patch","patch2"],omitempty: ["patch","patch2"],    quote: false, as: None },
            e: { db: "ee", tags: ["patch"],         omitempty: [],                   quote: false, as: None },
        }
    }

    #[test]
    fn struct_omit_empty_for_multiple_tags_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let st = Struct::<OmitEmptyForMultipleTags>::new();

        let (sql1, _) = st
            .update("foo", &OmitEmptyForMultipleTags::default())
            .build();
        assert_eq!(sql1, "UPDATE foo SET D = ?, ee = ?");

        let v = OmitEmptyForMultipleTags {
            a: 123,
            b: Some("bbbb".to_string()),
            c: 0,
            d: None,
            e: true,
        };
        let (sql2, args2) = st.with_tag(["patch2"]).update("foo", &v).build();
        assert_eq!(sql2, "UPDATE foo SET `aa` = ?");
        assert_eq!(args2.len(), 1);

        // Insert: cc is 0 in v1 but 2 in v2 => cc column must not be filtered out.
        let v1 = OmitEmptyForMultipleTags {
            a: 123,
            b: Some("bbbb".to_string()),
            c: 0,
            d: None,
            e: false,
        };
        let v2 = OmitEmptyForMultipleTags {
            a: 123,
            b: Some("bbbb".to_string()),
            c: 2,
            d: None,
            e: true,
        };
        let (sql3, args3) = st
            .with_tag(["patch2"])
            .insert_into("foo", [&v1, &v2])
            .build();
        assert_eq!(sql3, "INSERT INTO foo (`aa`, cc) VALUES (?, ?), (?, ?)");
        assert_eq!(args3.len(), 4);
    }

    #[derive(Clone, Default)]
    struct WithPointers {
        a: i64,
        b: Option<String>,
        c: Option<f64>,
    }

    crate::sql_struct! {
        impl WithPointers {
            // a/c: omitempty
            a: { db: "aa", tags: [], omitempty: [""], quote: false, as: None },
            // b: not omitempty (write NULL even when None).
            b: { db: "bb", tags: [], omitempty: [],   quote: false, as: None },
            c: { db: "cc", tags: [], omitempty: [""], quote: false, as: None },
        }
    }

    #[test]
    fn struct_with_pointers_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let st = Struct::<WithPointers>::new();

        let (sql1, _) = st.update("foo", &WithPointers::default()).build();
        assert_eq!(sql1, "UPDATE foo SET bb = ?");

        let v = WithPointers {
            a: 123,
            b: None,
            c: Some(123.45),
        };
        let (sql2, args2) = st.update("foo", &v).build();
        assert_eq!(sql2, "UPDATE foo SET aa = ?, bb = ?, cc = ?");
        assert_eq!(args2.len(), 3);
    }

    #[derive(Clone, Default)]
    struct StructWithMapper {
        field_name1: String,                  // no db -> use mapper
        field_name_set_by_tag: i64,           // db set_by_tag
        field_name_shadowed: i64,             // db field_name1 (shadowed when mapper is snake_case)
        embedded_field2: i64,                 // no db -> use mapper
        embedded_and_embedded_field1: String, // no db -> use mapper
    }

    crate::sql_struct! {
        impl StructWithMapper {
            // Note: db:"" means no db tag; uses mapper and keeps definition order for shadow behavior.
            field_name1: { db: "", orig: "FieldName1", tags: [], omitempty: [], quote: true,  as: None },
            field_name_set_by_tag: { db: "set_by_tag", tags: [], omitempty: [], quote: false, as: None },
            field_name_shadowed: { db: "field_name1", tags: [], omitempty: [], quote: false, as: None },
            embedded_field2: { db: "", orig: "EmbeddedField2", tags: [], omitempty: [], quote: false, as: None },
            embedded_and_embedded_field1: { db: "", orig: "EmbeddedAndEmbeddedField1", tags: [], omitempty: [], quote: false, as: None },
        }
    }

    #[test]
    fn struct_field_mapper_like_go() {
        let _g1 = set_default_flavor_scoped(Flavor::MySQL);
        let _g2 = set_default_field_mapper_scoped(std::sync::Arc::new(snake_case_mapper));

        let s = Struct::<StructWithMapper>::new();
        let (sql, _) = s.select_from("t").build();
        assert_eq!(
            sql,
            "SELECT t.`field_name1`, t.set_by_tag, t.embedded_field2, t.embedded_and_embedded_field1 FROM t"
        );

        let s_without = s.with_field_mapper(identity_mapper());
        let (sql2, _) = s_without.select_from("t").build();
        assert_eq!(
            sql2,
            "SELECT t.`FieldName1`, t.set_by_tag, t.field_name1, t.EmbeddedField2, t.EmbeddedAndEmbeddedField1 FROM t"
        );
    }

    #[derive(Clone, Default)]
    struct StructFieldTagExample {
        field1: String,
        field2: i64,
        field3: i64,
        field4: i64,
        field5: String,
        ignored: i32,
        quoted: String,
        empty: u64,
        tagged: String,
        field_with_table_alias: String,
    }

    crate::sql_struct! {
        impl StructFieldTagExample {
            field1: { db: "", orig: "Field1", tags: [], omitempty: [], quote: false, as: None },
            field2: { db: "field2", tags: [], omitempty: [], quote: false, as: None },
            field3: { db: "field3", orig: "Field3", tags: ["foo","bar"], omitempty: [], quote: false, as: None },
            field4: { db: "field4", orig: "Field4", tags: ["foo"], omitempty: [], quote: false, as: None },
            field5: { db: "field5", orig: "Field5", tags: [], omitempty: [], quote: false, as: Some("f5_alias") },
            ignored: { db: "-", orig: "Ignored", tags: [], omitempty: [], quote: false, as: None },
            quoted: { db: "quoted", orig: "Quoted", tags: [], omitempty: [], quote: true, as: None },
            empty: { db: "empty", orig: "Empty", tags: [], omitempty: [""], quote: false, as: None },
            tagged: { db: "tagged", orig: "Tagged", tags: ["tag1","tag2","tag3"], omitempty: ["tag1","tag3"], quote: false, as: None },
            field_with_table_alias: { db: "m.field", orig: "FieldWithTableAlias", tags: [], omitempty: [], quote: false, as: None },
        }
    }

    #[test]
    fn struct_field_tag_readme_example() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let s = Struct::<StructFieldTagExample>::new();
        let cols = s.columns();

        assert!(cols.contains(&"Field1".to_string()));
        assert!(cols.contains(&"field2".to_string()));
        assert!(cols.contains(&"field3".to_string()));
        assert!(cols.contains(&"field4".to_string()));
        assert!(cols.contains(&"quoted".to_string()));
        assert!(cols.contains(&"field5".to_string()));
        assert!(!cols.contains(&"ignored".to_string()));

        let cols_tagged = s.with_tag(["foo"]).columns();
        assert_eq!(cols_tagged, vec!["field3", "field4"]);

        let (sql, _) = s.select_from("t").build();
        assert!(sql.contains("t.field5 AS f5_alias"));
        assert!(sql.contains("m.field"));
        assert!(!sql.contains("t.m.field"));
    }

    #[derive(Clone, Default)]
    struct StructMapperVariants {
        field_one: String,
        field_two: String,
    }

    crate::sql_struct! {
        impl StructMapperVariants {
            field_one: { db: "", orig: "FieldOne", tags: [], omitempty: [], quote: false, as: None },
            field_two: { db: "", orig: "FieldTwo", tags: [], omitempty: [], quote: false, as: None },
        }
    }

    #[test]
    fn struct_field_mapper_additional_strategies_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let base = Struct::<StructMapperVariants>::new();

        let (sql_prefix, _) = base
            .with_field_mapper(prefix_mapper("db_"))
            .select_from("t")
            .build();
        assert_eq!(sql_prefix, "SELECT t.db_FieldOne, t.db_FieldTwo FROM t");

        let (sql_suffix, _) = base
            .with_field_mapper(suffix_mapper("_col"))
            .select_from("t")
            .build();
        assert_eq!(sql_suffix, "SELECT t.FieldOne_col, t.FieldTwo_col FROM t");

        let (sql_upper, _) = base
            .with_field_mapper(std::sync::Arc::new(upper_case_mapper))
            .select_from("t")
            .build();
        assert_eq!(sql_upper, "SELECT t.FIELDONE, t.FIELDTWO FROM t");

        let (sql_kebab, _) = base
            .with_field_mapper(std::sync::Arc::new(kebab_case_mapper))
            .select_from("t")
            .build();
        assert_eq!(sql_kebab, "SELECT t.field-one, t.field-two FROM t");
    }

    #[derive(Clone, Default)]
    struct StructOrders {
        id: i64,
        user_id: i64,
        product_name: String,
        status: i64,
        user_addr_line1: String,
        user_addr_line2: String,
        created_at: i64,
    }

    crate::sql_struct! {
        impl StructOrders {
            id: { db: "", tags: [], omitempty: [], quote: false, as: None },
            user_id: { db: "", tags: [], omitempty: [], quote: false, as: None },
            product_name: { db: "", tags: [], omitempty: [], quote: false, as: None },
            status: { db: "", tags: [], omitempty: [], quote: false, as: None },
            user_addr_line1: { db: "", tags: [], omitempty: [], quote: false, as: None },
            user_addr_line2: { db: "", tags: [], omitempty: [], quote: false, as: None },
            created_at: { db: "", tags: [], omitempty: [], quote: false, as: None },
        }
    }

    fn some_other_mapper(_: &str) -> String {
        String::new()
    }

    #[test]
    fn struct_field_mapper_default_snapshot_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let _g_mapper = set_default_field_mapper_scoped(std::sync::Arc::new(snake_case_mapper));

        let s = Struct::<StructOrders>::new();
        let (sql1, _) = s.select_from("orders").limit(10).build();
        assert_eq!(
            sql1,
            "SELECT orders.id, orders.user_id, orders.product_name, orders.status, orders.user_addr_line1, orders.user_addr_line2, orders.created_at FROM orders LIMIT ?"
        );

        set_default_field_mapper(std::sync::Arc::new(some_other_mapper));
        let (sql2, _) = s.select_from("orders").limit(10).build();
        assert_eq!(sql1, sql2);
    }

    #[derive(Clone, Default)]
    struct StructWithAs {
        t1: String,
        t2: String,
        t3: String,
        t4: String,
    }

    crate::sql_struct! {
        impl StructWithAs {
            t1: { db: "t1", tags: ["tag"], omitempty: [], quote: false, as: Some("f1") },
            t2: { db: "t2", tags: [],      omitempty: [], quote: false, as: None },
            t3: { db: "t2", tags: [],      omitempty: [], quote: false, as: Some("f3") },
            t4: { db: "t4", tags: ["tag"], omitempty: [], quote: false, as: Some("f3") },
        }
    }

    #[test]
    fn struct_field_as_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let s = Struct::<StructWithAs>::new();

        let (sql1, _) = s.select_from("t").build();
        assert_eq!(sql1, "SELECT t.t1 AS f1, t.t2, t.t2 AS f3 FROM t");

        let (sql2, _) = s.with_tag(["tag"]).select_from("t").build();
        assert_eq!(sql2, "SELECT t.t1 AS f1, t.t4 AS f3 FROM t");

        let v = StructWithAs {
            t1: "t1".to_string(),
            t2: "t2".to_string(),
            t3: "t3".to_string(),
            t4: "t4".to_string(),
        };
        let (sql3, _) = s.update("t", &v).build();
        assert_eq!(sql3, "UPDATE t SET t1 = ?, t2 = ?, t4 = ?");
    }

    #[derive(Debug, Clone)]
    struct ImplValuer(i64);

    impl crate::valuer::SqlValuer for ImplValuer {
        fn value(&self) -> Result<crate::SqlValue, crate::valuer::ValuerError> {
            Ok(crate::SqlValue::I64(self.0 * 2))
        }
    }

    #[derive(Clone)]
    struct ContainsValuer {
        f1: String,
        f2: Box<dyn crate::valuer::SqlValuer>,
    }

    crate::sql_struct! {
        impl ContainsValuer {
            f1: { db: "F1", orig: "F1", tags: [], omitempty: [], quote: false, as: None },
            f2: { db: "F2", orig: "F2", tags: [], omitempty: [], quote: false, as: None },
        }
    }

    #[test]
    fn struct_fields_impl_valuer_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let st = Struct::<ContainsValuer>::new().for_flavor(Flavor::MySQL);

        let f1 = "foo".to_string();
        let f2: Box<dyn crate::valuer::SqlValuer> = Box::new(ImplValuer(100));

        let v = ContainsValuer { f1: f1.clone(), f2 };
        let (sql, args) = st.update("t", &v).build();
        assert_eq!(sql, "UPDATE t SET F1 = ?, F2 = ?");

        let result = Flavor::MySQL.interpolate(&sql, &args).unwrap();
        assert_eq!(result, "UPDATE t SET F1 = 'foo', F2 = 200");
    }

    #[derive(Clone, Default)]
    struct UserForTag {
        id: i64,
        name: String,
        status: i64,
        created_at: i64,
    }

    crate::sql_struct! {
        impl UserForTag {
            id: { db: "id", orig: "ID", tags: ["important"], omitempty: [], quote: false, as: None },
            name: { db: "", orig: "Name", tags: ["important"], omitempty: [], quote: false, as: None },
            status: { db: "status", orig: "Status", tags: ["important"], omitempty: [], quote: false, as: None },
            created_at: { db: "created_at", orig: "CreatedAt", tags: [], omitempty: [], quote: false, as: None },
        }
    }

    #[test]
    fn struct_columns_and_values_for_tag_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let s = Struct::<UserForTag>::new();

        assert_eq!(s.columns(), vec!["id", "Name", "status", "created_at"]);
        assert_eq!(
            s.columns_for_tag("important").unwrap(),
            vec!["id", "Name", "status"]
        );
        assert_eq!(s.columns_for_tag("invalid"), None);

        let u = UserForTag {
            id: 123,
            name: "huandu".to_string(),
            status: 2,
            created_at: 1234567890,
        };
        let v = s.values_for_tag("important", &u).unwrap();
        assert_eq!(v.len(), 3);
        assert_eq!(v[0], 123_i64.into());
        assert_eq!(v[1], "huandu".to_string().into());
        assert_eq!(v[2], 2_i64.into());
        assert_eq!(s.values_for_tag("invalid", &u), None);
    }

    #[derive(Clone, Default)]
    struct ForeachDemo {
        id: i64,
        name: String,
        ignored: i64,
    }

    crate::sql_struct! {
        impl ForeachDemo {
            id: { db: "id", orig: "ID", tags: [], omitempty: [], quote: false, as: None },
            // Empty db: Foreach* exposes empty dbtag.
            name: { db: "", orig: "Name", tags: [], omitempty: [], quote: true, as: None },
            // db "-"：忽略
            ignored: { db: "-", orig: "Ignored", tags: [], omitempty: [], quote: false, as: None },
        }
    }

    #[test]
    fn struct_foreach_read_and_write_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let s = Struct::<ForeachDemo>::new();

        let mut read = Vec::<(String, bool, String)>::new();
        s.foreach_read(|dbtag, quoted, fm| {
            read.push((dbtag.to_string(), quoted, fm.rust.to_string()));
        });
        // Default without tags: ForRead order matches definition (ignores db:"-").
        assert_eq!(
            read,
            vec![
                ("id".to_string(), false, "id".to_string()),
                ("".to_string(), true, "name".to_string()),
            ]
        );

        let mut write = Vec::<(String, bool, String)>::new();
        s.foreach_write(|dbtag, quoted, fm| {
            write.push((dbtag.to_string(), quoted, fm.rust.to_string()));
        });
        // ForWrite equals ForRead in this simple case.
        assert_eq!(
            write,
            vec![
                ("id".to_string(), false, "id".to_string()),
                ("".to_string(), true, "name".to_string()),
            ]
        );
    }

    #[derive(Clone, Default)]
    struct StructUserForTest {
        id: i64,
        name: String,
        status: i64,
        created_at: i64,
        ignored: i64,
    }

    crate::sql_struct! {
        impl StructUserForTest {
            id: { db: "id", orig: "ID", tags: ["important"], omitempty: [], quote: false, as: None },
            name: { db: "", orig: "Name", tags: ["important"], omitempty: [], quote: false, as: None },
            status: { db: "status", orig: "Status", tags: ["important"], omitempty: [], quote: false, as: None },
            created_at: { db: "created_at", orig: "CreatedAt", tags: [], omitempty: [], quote: false, as: None },
            ignored: { db: "-", orig: "unexported", tags: [], omitempty: [], quote: false, as: None },
        }
    }

    #[test]
    fn struct_select_from_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let s = Struct::<StructUserForTest>::new();
        let (sql, args) = s.select_from("user").build();
        assert_eq!(
            sql,
            "SELECT user.id, user.Name, user.status, user.created_at FROM user"
        );
        assert!(args.is_empty());
    }

    #[test]
    fn struct_select_from_for_tag_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let s = Struct::<StructUserForTest>::new();
        let (sql, args) = s.select_from_for_tag("user", "important").build();
        assert_eq!(sql, "SELECT user.id, user.Name, user.status FROM user");
        assert!(args.is_empty());
    }

    #[test]
    fn struct_update_and_update_for_tag_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let s = Struct::<StructUserForTest>::new();
        let u = StructUserForTest {
            id: 123,
            name: "Huan Du".to_string(),
            status: 2,
            created_at: 1234567890,
            ignored: 0,
        };

        let (sql, args) = s.update("user", &u).build();
        assert_eq!(
            sql,
            "UPDATE user SET id = ?, Name = ?, status = ?, created_at = ?"
        );
        assert_eq!(args.len(), 4);

        let (sql2, args2) = s.update_for_tag("user", "important", &u).build();
        assert_eq!(sql2, "UPDATE user SET id = ?, Name = ?, status = ?");
        assert_eq!(args2.len(), 3);
    }

    #[test]
    fn struct_insert_and_insert_for_tag_like_go_including_ignore_wrong_type() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let s = Struct::<StructUserForTest>::new();
        let u1 = StructUserForTest {
            id: 123,
            name: "Huan Du".to_string(),
            status: 2,
            created_at: 1234567890,
            ignored: 0,
        };
        let u2 = StructUserForTest {
            id: 456,
            name: "Du Huan".to_string(),
            status: 2,
            created_at: 1234567890,
            ignored: 0,
        };
        let fake = 789_i64;

        // 单行
        for (ib, verb) in [
            (s.insert_into("user", [&u1]), "INSERT "),
            (s.insert_ignore_into("user", [&u1]), "INSERT IGNORE "),
            (s.replace_into("user", [&u1]), "REPLACE "),
        ] {
            let (sql, args) = ib.build();
            assert_eq!(
                sql,
                format!("{verb}INTO user (id, Name, status, created_at) VALUES (?, ?, ?, ?)")
            );
            assert_eq!(args.len(), 4);
        }

        // 多行 + 含无关类型（应被忽略）
        let items: Vec<&dyn std::any::Any> = vec![&u1, &u2, &fake];
        for (ib, verb) in [
            (s.insert_into_any("user", items.clone()), "INSERT "),
            (
                s.insert_ignore_into_any("user", items.clone()),
                "INSERT IGNORE ",
            ),
            (s.replace_into_any("user", items.clone()), "REPLACE "),
        ] {
            let (sql, args) = ib.build();
            assert_eq!(
                sql,
                format!(
                    "{verb}INTO user (id, Name, status, created_at) VALUES (?, ?, ?, ?), (?, ?, ?, ?)"
                )
            );
            assert_eq!(args.len(), 8);
        }

        // for tag
        for (ib, verb) in [
            (s.insert_into_for_tag("user", "important", [&u1]), "INSERT "),
            (
                s.insert_ignore_into_for_tag("user", "important", [&u1]),
                "INSERT IGNORE ",
            ),
            (
                s.replace_into_for_tag("user", "important", [&u1]),
                "REPLACE ",
            ),
        ] {
            let (sql, args) = ib.build();
            assert_eq!(
                sql,
                format!("{verb}INTO user (id, Name, status) VALUES (?, ?, ?)")
            );
            assert_eq!(args.len(), 3);
        }

        let items: Vec<&dyn std::any::Any> = vec![&u1, &u2, &fake];
        for (ib, verb) in [
            (
                s.insert_into_for_tag_any("user", "important", items.clone()),
                "INSERT ",
            ),
            (
                s.insert_ignore_into_for_tag_any("user", "important", items.clone()),
                "INSERT IGNORE ",
            ),
            (
                s.replace_into_for_tag_any("user", "important", items.clone()),
                "REPLACE ",
            ),
        ] {
            let (sql, args) = ib.build();
            assert_eq!(
                sql,
                format!("{verb}INTO user (id, Name, status) VALUES (?, ?, ?), (?, ?, ?)")
            );
            assert_eq!(args.len(), 6);
        }
    }

    #[test]
    fn struct_delete_from_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let s = Struct::<StructUserForTest>::new();
        let (sql, args) = s.delete_from("user").build();
        assert_eq!(sql, "DELETE FROM user");
        assert!(args.is_empty());
    }

    #[test]
    fn struct_values_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let s = Struct::<StructUserForTest>::new();
        let u = StructUserForTest {
            id: 123,
            name: "huandu".to_string(),
            status: 2,
            created_at: 1234567890,
            ignored: 0,
        };
        let vs = s.values(&u);
        assert_eq!(vs.len(), 4);
        assert_eq!(vs[0], 123_i64.into());
        assert_eq!(vs[1], "huandu".to_string().into());
        assert_eq!(vs[2], 2_i64.into());
        assert_eq!(vs[3], 1234567890_i64.into());
    }

    #[test]
    fn struct_addr_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let s = Struct::<StructUserForTest>::new();

        let mut user = StructUserForTest::default();
        let expected = StructUserForTest {
            id: 123,
            name: "huandu".to_string(),
            status: 2,
            created_at: 1234567890,
            ignored: 0,
        };
        let input = format!(
            "{} {} {} {}",
            expected.id, expected.name, expected.status, expected.created_at
        );

        let addrs = s.addr(&mut user);
        scan_tokens(&input, addrs).unwrap();
        assert_eq!(user.id, expected.id);
        assert_eq!(user.name, expected.name);
        assert_eq!(user.status, expected.status);
        assert_eq!(user.created_at, expected.created_at);
    }

    #[test]
    fn struct_addr_for_tag_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let s = Struct::<StructUserForTest>::new();

        let mut user = StructUserForTest {
            created_at: 9876543210,
            ..Default::default()
        };

        let expected = StructUserForTest {
            id: 123,
            name: "huandu".to_string(),
            status: 2,
            created_at: 1234567890,
            ignored: 0,
        };
        let input = format!(
            "{} {} {} {}",
            expected.id, expected.name, expected.status, expected.created_at
        );

        let addrs = s.addr_for_tag("important", &mut user).unwrap();
        scan_tokens(&input, addrs).unwrap();

        assert_eq!(user.id, expected.id);
        assert_eq!(user.name, expected.name);
        assert_eq!(user.status, expected.status);
        // created_at is not in important tag; keep original value.
        assert_eq!(user.created_at, 9876543210);
        assert!(s.addr_for_tag("invalid", &mut user).is_none());
    }

    #[test]
    fn struct_addr_with_cols_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let s = Struct::<StructUserForTest>::new();

        let mut user = StructUserForTest::default();
        let expected = StructUserForTest {
            id: 123,
            name: "huandu".to_string(),
            status: 2,
            created_at: 1234567890,
            ignored: 0,
        };
        let input = format!(
            "{} {} {} {}",
            expected.name, expected.id, expected.created_at, expected.status
        );

        let cols = ["Name", "id", "created_at", "status"];
        let addrs = s.addr_with_cols(&cols, &mut user).unwrap();
        scan_tokens(&input, addrs).unwrap();

        assert_eq!(user.id, expected.id);
        assert_eq!(user.name, expected.name);
        assert_eq!(user.status, expected.status);
        assert_eq!(user.created_at, expected.created_at);

        assert!(
            s.addr_with_cols(&["invalid", "non-exist"], &mut user)
                .is_none()
        );
    }

    #[derive(Clone, Default)]
    struct ExampleOrmUser {
        id: i64,
        name: String,
        status: i64,
    }

    crate::sql_struct! {
        impl ExampleOrmUser {
            id: { db: "id", tags: [], omitempty: [], quote: false, as: None },
            name: { db: "name", tags: [], omitempty: [], quote: false, as: None },
            status: { db: "status", tags: [], omitempty: [], quote: false, as: None },
        }
    }

    #[test]
    fn struct_use_struct_as_orm_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let st = Struct::<ExampleOrmUser>::new();
        let mut sb = st.select_from("user");
        let expr = sb.equal("id", 1234_i64);
        sb.where_([expr]);

        let (sql, args) = sb.build();
        assert_eq!(
            sql,
            "SELECT user.id, user.name, user.status FROM user WHERE id = ?"
        );
        assert_eq!(args, vec![1234_i64.into()]);
    }

    #[derive(Clone, Default)]
    struct ExampleMember {
        id: String,
        user_id: String,
        member_name: String,
        created_at: String,
        name: String,
        email: String,
    }

    crate::sql_struct! {
        impl ExampleMember {
            id: { db: "id", tags: [], omitempty: [], quote: false, as: None },
            user_id: { db: "user_id", tags: [], omitempty: [], quote: false, as: None },
            member_name: { db: "name", tags: [], omitempty: [], quote: false, as: None },
            created_at: { db: "created_at", tags: [], omitempty: [], quote: false, as: None },
            name: { db: "u.name", tags: [], omitempty: [], quote: false, as: None },
            email: { db: "u.email", tags: [], omitempty: [], quote: false, as: None },
        }
    }

    #[test]
    fn struct_build_join_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let st = Struct::<ExampleMember>::new();
        let mut sb = st.select_from("member");
        sb.join("user u", ["member.user_id = u.id"]);

        let (sql, _) = sb.build();
        assert_eq!(
            sql,
            "SELECT member.id, member.user_id, member.name, member.created_at, u.name, u.email FROM member JOIN user u ON member.user_id = u.id"
        );
    }

    #[test]
    fn struct_foreach_read_write_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let st = Struct::<StructWithQuote>::new();

        let mut read = Vec::new();
        st.foreach_read(|dbtag, with_quote, fm| {
            read.push((dbtag.to_string(), with_quote, fm.rust));
        });
        assert_eq!(
            read,
            vec![
                ("aa".to_string(), true, "a"),
                ("ccc".to_string(), false, "c"),
            ]
        );

        let mut write = Vec::new();
        st.foreach_write(|dbtag, with_quote, fm| {
            write.push((dbtag.to_string(), with_quote, fm.rust));
        });
        assert_eq!(
            write,
            vec![
                ("aa".to_string(), true, "a"),
                ("ccc".to_string(), false, "c"),
            ]
        );
    }

    type State = i64;

    const ORDER_STATE_CREATED: State = 0;
    const ORDER_STATE_PAID: State = 1;

    #[derive(Clone, Default)]
    struct OrderExample {
        id: i64,
        state: State,
        sku_id: i64,
        user_id: i64,
        price: i64,
        discount: i64,
        desc: String,
        created_at: i64,
        modified_at: i64,
    }

    crate::sql_struct! {
        impl OrderExample {
            id: { db: "id", tags: ["pk"], omitempty: [], quote: false, as: None },
            state: { db: "state", tags: ["paid"], omitempty: [], quote: false, as: None },
            sku_id: { db: "sku_id", tags: [], omitempty: [], quote: false, as: None },
            user_id: { db: "user_id", tags: [], omitempty: [], quote: false, as: None },
            price: { db: "price", tags: ["update"], omitempty: [], quote: false, as: None },
            discount: { db: "discount", tags: ["update"], omitempty: [], quote: false, as: None },
            desc: { db: "`desc`", tags: ["new","update"], omitempty: [], quote: false, as: None },
            created_at: { db: "created_at", tags: [], omitempty: [], quote: false, as: None },
            modified_at: { db: "modified_at", tags: ["update","paid"], omitempty: [], quote: false, as: None },
        }
    }

    #[test]
    fn struct_with_tag_update_paid_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let mut order = OrderExample {
            id: 1234,
            state: ORDER_STATE_CREATED,
            sku_id: 5678,
            user_id: 7527,
            price: 1000,
            discount: 0,
            desc: "Best goods".to_string(),
            created_at: 1,
            modified_at: 2,
        };
        let st = Struct::<OrderExample>::new();

        let (insert_sql, _) = st.insert_into("order", [&order]).build();
        assert_eq!(
            insert_sql,
            "INSERT INTO order (id, state, sku_id, user_id, price, discount, `desc`, created_at, modified_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        );

        let mut select_builder = st.with_tag(["update"]).select_from("order");
        let expr = select_builder.equal("id", 1234);
        let (select_sql, _) = select_builder.where_([expr]).build();
        assert_eq!(
            select_sql,
            "SELECT order.price, order.discount, order.`desc`, order.modified_at FROM order WHERE id = ?"
        );

        order.discount += 100;
        order.modified_at += 1;
        let mut ub = st.with_tag(["update"]).update("order", &order);
        let expr = ub.equal("id", order.id);
        let (update_sql, _) = ub.where_([expr]).build();
        assert_eq!(
            update_sql,
            "UPDATE order SET price = ?, discount = ?, `desc` = ?, modified_at = ? WHERE id = ?"
        );

        let mut paid_builder = st.with_tag(["paid"]).select_from("order");
        let expr = paid_builder.equal("id", order.id);
        let (select_paid, _) = paid_builder.where_([expr]).build();
        assert_eq!(
            select_paid,
            "SELECT order.state, order.modified_at FROM order WHERE id = ?"
        );

        order.state = ORDER_STATE_PAID;
        order.modified_at += 1;
        let mut ub_paid = st.with_tag(["paid"]).update("order", &order);
        let expr = ub_paid.equal("id", order.id);
        let (paid_sql, _) = ub_paid.where_([expr]).build();
        assert_eq!(
            paid_sql,
            "UPDATE order SET state = ?, modified_at = ? WHERE id = ?"
        );
    }

    #[derive(Clone, Default)]
    struct UserWithoutPk {
        id: i64,
        first_name: String,
        last_name: String,
        modified_at_time: i64,
    }

    crate::sql_struct! {
        impl UserWithoutPk {
            id: { db: "id", tags: ["pk"], omitempty: [], quote: false, as: None },
            first_name: { db: "first_name", tags: [], omitempty: [], quote: false, as: None },
            last_name: { db: "last_name", tags: [], omitempty: [], quote: false, as: None },
            modified_at_time: { db: "modified_at_time", tags: [], omitempty: [], quote: false, as: None },
        }
    }

    #[test]
    fn struct_without_tag_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let st = Struct::<UserWithoutPk>::new();
        let user = UserWithoutPk {
            id: 1234,
            first_name: "Huan".to_string(),
            last_name: "Du".to_string(),
            modified_at_time: 999,
        };

        let mut ub = st.without_tag(["pk"]).update("user", &user);
        let expr = ub.equal("id", user.id);
        let (sql, _) = ub.where_([expr]).build();
        assert_eq!(
            sql,
            "UPDATE user SET first_name = ?, last_name = ?, modified_at_time = ? WHERE id = ?"
        );

        let ib = st.without_tag(["pk"]).insert_into("user", [&user]);
        let (sql_insert, _) = ib.build();
        assert_eq!(
            sql_insert,
            "INSERT INTO user (first_name, last_name, modified_at_time) VALUES (?, ?, ?)"
        );

        let mut db = st.delete_from("user");
        let expr = db.equal("id", user.id);
        let (sql_delete, _) = db.where_([expr]).build();
        assert_eq!(sql_delete, "DELETE FROM user WHERE id = ?");
    }

    #[test]
    fn struct_for_postgresql_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let st = Struct::<UserWithoutPk>::new().for_flavor(Flavor::PostgreSQL);
        let mut sb = st.select_from("user");
        let expr = sb.equal("id", 1234);
        let (sql, args) = sb.where_([expr]).build();
        assert_eq!(
            sql,
            "SELECT user.id, user.first_name, user.last_name, user.modified_at_time FROM user WHERE id = $1"
        );
        assert_eq!(args, vec![1234_i64.into()]);
    }

    #[test]
    fn struct_for_cql_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let st = Struct::<UserWithoutPk>::new().for_flavor(Flavor::CQL);
        let mut sb = st.select_from("user");
        let expr = sb.equal("id", 1234);
        let (sql, args) = sb.where_([expr]).build();
        assert_eq!(
            sql,
            "SELECT id, first_name, last_name, modified_at_time FROM user WHERE id = ?"
        );
        assert_eq!(args, vec![1234_i64.into()]);
    }
}
