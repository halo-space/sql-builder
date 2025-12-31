# halo-sqlbuilder 

- Provides:
- `Args` + `Flavor`: placeholder strategies `?`, `$1`, `@p1`, `:1` with dialect control.
- Builders: `SelectBuilder`, `InsertBuilder`, `UpdateBuilder`, `DeleteBuilder`, `UnionBuilder`, `CTEBuilder`, `CTEQueryBuilder`, `CreateTableBuilder`, covering query/insert/update/delete/aggregate/CTE/Union with cloning.
- `Build`/`Buildf`/`BuildNamed`: `${name}`, `$0`, `$?`, `$$`, `Raw`, `List`, `Tuple`, nested builders, named arg reuse, literal `$`.
- `Struct` + `field_mapper`: `macro_rules!` generated `FieldMeta`; supports `db`/`fieldtag`/`fieldopt`/`fieldas`, `with_tag`/`without_tag`, custom mappers (snake_case/kebab_case/prefix/suffix); works with `SqlValuer`.
- `Scan` + `ScanCell`: Addr-style scanning helpers.
- `interpolate`: literal SQL interpolation for drivers without parameter support; flavor-aware escaping for strings/numbers/datetime/bool.
- `SqlValuer`: deferred value computation with custom sources.
- 138 examples/tests (doc-tests included) covering builder, Struct, CTE, Union, field mapper, named params, etc.

## Install & Import

- Install: `cargo add halo-sqlbuilder`
- Use: `use halo_space::sqlbuilder::{...};`

## 中文使用说明

- 安装：`cargo add halo-sqlbuilder`
- 导入：`use halo_space::sqlbuilder::{...};`
- 典型用法（SELECT）：
  ```rust
  use halo_space::sqlbuilder::{select::SelectBuilder, select_cols, from_tables, where_exprs};

  let mut sb = SelectBuilder::new();
  select_cols!(sb, "id", "name");
  from_tables!(sb, "users");
  where_exprs!(sb, "status = 'active'");
  let (sql, args) = sb.build();
  assert_eq!(sql, "SELECT id, name FROM users WHERE status = 'active'");
  assert!(args.is_empty());
  ```

## Usage

### Build SELECT

```rust
use halo_space::sqlbuilder::{from_tables, select_cols, where_exprs, select::SelectBuilder};

let mut sb = SelectBuilder::new();
select_cols!(sb, "id");
from_tables!(sb, "user");
where_exprs!(sb, sb.in_("status", [1_i64, 2, 3]));

let (sql, args) = sb.build();
assert_eq!(sql, "SELECT id FROM user WHERE status IN (?, ?, ?)");
assert_eq!(args.len(), 3);
```

### Builder API directly

```rust
use halo_space::sqlbuilder::select::SelectBuilder;

let mut sb = SelectBuilder::new();
sb.select("id") // single column
    .select_more(["name", "email"]) // array input
    .select_more(vec!["score"]); // Vec input
sb.from(["users", "users_detail"]);
sb.order_by(["name", "score"])
    .where_(["score >= 100", "status = 'active'"]);

let (sql, args) = sb.build();
assert!(sql.contains("SELECT id, name, email, score"));
assert!(sql.contains("FROM users, users_detail"));
```

`select` / `select_more` / `from` / `where_` accept anything implementing `IntoStrings` (`&str`, `String`, arrays, `Vec`), or you can call the macros directly.

`SelectBuilder`/`UpdateBuilder`/`DeleteBuilder`/`InsertBuilder` expose `build()` which delegates to `build_with_flavor`, so you rarely need to import `modifiers::Builder` explicitly.

### Condition / Chain queries

```rust
use halo_space::sqlbuilder::condition::{
    build_select_with_flavor, Chain, ChainOptions, Condition, ConditionValue, Operator,
};
use halo_space::sqlbuilder::select::SelectBuilder;
use halo_space::sqlbuilder::Flavor;

// Build OR conditions
let conditions = vec![
    Condition::new("name", Operator::Equal, "jzero"),
    Condition {
        or: true,
        or_operators: vec![Operator::Between, Operator::Between],
        or_fields: vec!["age".into(), "height".into()],
        or_values: vec![
            ConditionValue::from([24_i64, 48]),
            ConditionValue::from([170_i64, 175]),
        ],
        skip: false,
        skip_fn: None,
        or_values_fn: None,
        field: String::new(),
        operator: Operator::Between,
        value: ConditionValue::default(),
        value_fn: None,
        join: None,
        where_clause: None,
    },
];

let mut sb = SelectBuilder::new();
sb.select(vec!["name", "age", "height"]).from(vec!["user"]);
let (sql, args) = build_select_with_flavor(Flavor::MySQL, sb, conditions);
assert_eq!(
    sql,
    "SELECT name, age, height FROM user WHERE `name` = ? AND (`age` BETWEEN ? AND ? OR `height` BETWEEN ? AND ?)"
);
assert_eq!(args, vec!["jzero".into(), 24_i64.into(), 48_i64.into(), 170_i64.into(), 175_i64.into()]);

// Chain supports join / pagination / group by / order
let chain = Chain::new()
    .equal_opts("status", "active", ChainOptions::default().skip(false))
    .join(
        halo_space::sqlbuilder::JoinOption::InnerJoin,
        "user_ext",
        ["user.id = user_ext.uid"],
    )
    .group_by("status")
    .page(2, 10)
    .order_by_desc("created_at");

let mut sb2 = SelectBuilder::new();
sb2.select(vec!["user.id", "user.name"]).from(vec!["user"]);
let (sql2, _args2) = build_select_with_flavor(Flavor::MySQL, sb2, chain.build());
assert!(sql2.contains("INNER JOIN user_ext ON user.id = user_ext.uid"));
assert!(sql2.contains("LIMIT"));

// Post-modifiers similar to Go's WithValueFunc/WithSkip/WithSkipFunc
// 既可传闭包，也可把已有函数名当回调传进去
fn compute_name() -> &'static str {
    "jzero"
}

let chain2 = Chain::new()
    .equal("name", "placeholder")
    .value_fn(|| compute_name().into()) // pass a function; .value_fn(|| "jzero".into()) also works
    .skip(false)                        // set skip
    .skip_fn(|| false);                 // skip_fn has higher priority
let mut sb3 = SelectBuilder::new();
sb3.select(vec!["id", "name"]).from(vec!["user"]);
let (sql3, args3) = build_select_with_flavor(Flavor::MySQL, sb3, chain2.build());
assert_eq!(sql3, "SELECT id, name FROM user WHERE `name` = ?");
assert_eq!(args3, vec!["jzero".into()]);
```

### 变长参数宏

Macros (`select_cols!`, `from_tables!`, `where_exprs!`, `returning_cols!`, etc.) can be imported from the root: `use halo_space::sqlbuilder::{select_cols, from_tables, where_exprs};` They expand multiple strings/columns into `Vec<String>` so you don't build slices manually.

```rust
use halo_space::sqlbuilder::{from_tables, order_by_cols, select_cols, where_exprs, select::SelectBuilder};

let mut sb = SelectBuilder::new();
select_cols!(sb, "id", "name");
from_tables!(sb, "users");
where_exprs!(sb, "status = 'active'", "type <> 'guest'");
order_by_cols!(sb, "name");

let (sql, _) = sb.build();
assert!(sql.contains("WHERE"));
```

Other macros: `insert_cols!` / `insert_select_cols!` / `delete_from_tables!` / `update_set!` / `create_table_define!` / `struct_with_tag!` for common string varargs.

### INSERT / RETURNING

```rust
use halo_space::sqlbuilder::{insert::InsertBuilder, insert_cols, returning_cols};

let mut ib = InsertBuilder::new();
ib.insert_into("users");
insert_cols!(ib, "name", "age").values(["alice", 18_i64]);
returning_cols!(ib, "id");

let (sql, args) = ib.build();
assert_eq!(sql, "INSERT INTO users (name, age) VALUES (?, ?) RETURNING id");
assert_eq!(args.len(), 2);
```

### UPDATE / WHERE / ORDER BY

```rust
use halo_space::sqlbuilder::{update::UpdateBuilder, update_set, where_exprs, update_tables};

let mut ub = UpdateBuilder::new();
update_tables!(ub, "users");
update_set!(ub, "score = score + 1");
where_exprs!(ub, "status = 'active'");
ub.order_by_desc("score");

let (sql, _) = ub.build();
assert!(sql.contains("UPDATE users SET score = score + 1 WHERE status = 'active' ORDER BY score DESC"));
```

### Condition / Chain update

```rust
use halo_space::sqlbuilder::condition::{
    build_update_with_flavor, Chain, ConditionValue, Operator, UpdateFieldChain, UpdateFieldOptions,
};
use halo_space::sqlbuilder::update::UpdateBuilder;
use halo_space::sqlbuilder::Flavor;

let updates = UpdateFieldChain::new()
    .assign("name", "alice", UpdateFieldOptions::default())
    .incr("version", UpdateFieldOptions::default())
    .add("score", 5_i64, UpdateFieldOptions::default());

let chain = Chain::new().equal("id", 1_i64);

let mut ub = UpdateBuilder::new();
ub.update(vec!["users"]);
let (sql, _args) = build_update_with_flavor(Flavor::MySQL, ub, updates.build(), chain.build());
assert!(sql.starts_with("UPDATE users SET"));
assert!(sql.contains("WHERE `id` = ?"));
```

### DELETE / LIMIT

```rust
use halo_space::sqlbuilder::{delete::DeleteBuilder, delete_from_tables, where_exprs};

let mut db = DeleteBuilder::new();
delete_from_tables!(db, "sessions");
where_exprs!(db, "expired_at < NOW()");
db.limit(100);

let (sql, _) = db.build();
assert!(sql.contains("DELETE FROM sessions WHERE expired_at < NOW() LIMIT ?"));
```

### 嵌套 Builder / Buildf

```rust
use halo_space::sqlbuilder::{builder::buildf, from_tables, select_cols, select::SelectBuilder};

let mut sb = SelectBuilder::new();
select_cols!(sb, "id");
from_tables!(sb, "user");

let explain = buildf(
    "EXPLAIN %v LEFT JOIN SELECT * FROM banned WHERE state IN (%v, %v)",
    [sb.into(), 1_i64, 2_i64],
);
let (sql, _) = explain.build();
assert!(sql.contains("EXPLAIN SELECT id FROM user"));
```

### Named parameters

```rust
use halo_space::sqlbuilder::{
    builder::build_named,
    modifiers::{SqlNamedArg, raw, list},
};

let mut named = std::collections::HashMap::new();
named.insert("table".to_string(), raw("user"));
named.insert("status".to_string(), list([1_i64, 2, 3]));
named.insert("time".to_string(), SqlNamedArg::new("start", 1_514_458_225_i64).into());

let (sql, args) = build_named(
    "SELECT * FROM ${table} WHERE status IN (${status}) AND created_at > ${time}",
    named,
)
.build();
assert!(sql.contains("@start"));
```

### Struct ORM + field mapper

```rust
use halo_space::sqlbuilder::{field_mapper::snake_case_mapper, Struct};

// Enable snake_case mapping
let _guard = halo_space::sqlbuilder::field_mapper::set_default_field_mapper_scoped(
    std::sync::Arc::new(snake_case_mapper),
);

#[derive(Default, Clone)]
struct User {
    id: i64,
    user_name: String,
}

// 使用 sql_struct! 生成字段元数据与取值逻辑
halo_space::sqlbuilder::sql_struct! {
    impl User {
        id:        { db: "id",  tags: [], omitempty: [], quote: false, as: None },
        user_name: { db: "",    tags: [], omitempty: [], quote: false, as: None },
    }
}

let s = Struct::<User>::new();
let (sql, _) = s.select_from("user").build();
assert!(sql.contains("user.user_name"));
```

### CTE and Union

```rust
use halo_space::sqlbuilder::{
    cte::with,
    cte_query::CTEQueryBuilder,
    from_tables, select_cols, where_exprs,
    select::SelectBuilder,
};

let mut users_cte = CTEQueryBuilder::new();
let mut query = SelectBuilder::new();
select_cols!(query, "id");
from_tables!(query, "users");
where_exprs!(query, "name IS NOT NULL");
users_cte.table("users", ["id"]).as_(query);

let cte = with([users_cte]);
let mut sb = cte.select(Vec::<String>::new());
select_cols!(sb, "users.id");
from_tables!(sb, "users");
let (sql, _) = sb.build();
assert!(sql.contains("WITH users"));
```

### UNION / UNION ALL

```rust
use halo_space::sqlbuilder::{union::UnionBuilder, select::SelectBuilder, select_cols, from_tables};

let mut sb1 = SelectBuilder::new();
select_cols!(sb1, "id");
from_tables!(sb1, "t1");

let mut sb2 = SelectBuilder::new();
select_cols!(sb2, "id");
from_tables!(sb2, "t2");

let mut ub = UnionBuilder::new();
ub.union_all([sb1, sb2]).order_by(["id"]).limit(10);
let (sql, _) = ub.build();
assert!(sql.contains("UNION ALL"));
```

### CREATE TABLE

```rust
use halo_space::sqlbuilder::{
    create_table::CreateTableBuilder, create_table_define, create_table_option,
};

let mut ct = CreateTableBuilder::new();
ct.create_table("users").if_not_exists();
create_table_define!(ct, "id INT", "name TEXT");
create_table_option!(ct, "ENGINE=InnoDB");

let (sql, _) = ct.build();
assert!(sql.contains("CREATE TABLE"));
```

### Flavor switching

```rust
use halo_space::sqlbuilder::{Flavor, select::SelectBuilder, select_cols, from_tables};

let mut sb = SelectBuilder::new();
select_cols!(sb, "id");
from_tables!(sb, "user");

// Uses global Flavor by default; can switch temporarily
let (pg_sql, _) = sb.build_with_flavor(Flavor::PostgreSQL, &[]);
let (mysql_sql, _) = sb.build_with_flavor(Flavor::MySQL, &[]);
assert!(pg_sql.contains("$1") || pg_sql.contains("$2")); // PostgreSQL placeholders
assert!(mysql_sql.contains("?")); // MySQL placeholders
```

### Args/占位符与命名参数

```rust
use halo_space::sqlbuilder::{builder::build_named, modifiers::{SqlNamedArg, list, raw}};

let mut named = std::collections::HashMap::new();
named.insert("table".to_string(), raw("user"));
named.insert("ids".to_string(), list([1_i64, 2, 3]));
named.insert("now".to_string(), SqlNamedArg::new("t", 1_700_000_000_i64).into());

let (sql, args) = build_named(
    "SELECT * FROM ${table} WHERE id IN (${ids}) AND created_at > ${now}",
    named,
).build();
assert!(sql.contains("@t")); // named placeholder
assert_eq!(args.len(), 0);   // named params not in args
```

### Build/Buildf 快速包装

```rust
use halo_space::sqlbuilder::builder::{build, buildf};

let b1 = build("SELECT 1", ());
assert_eq!(b1.build().0, "SELECT 1");

let b2 = buildf("SELECT * FROM banned WHERE state IN (%v, %v)", [1, 2]);
assert_eq!(b2.build().1.len(), 2);
```

### Struct/field mapper/with_tag

```rust
use halo_space::sqlbuilder::{field_mapper::snake_case_mapper, Struct};
let _guard = halo_space::sqlbuilder::field_mapper::set_default_field_mapper_scoped(
    std::sync::Arc::new(snake_case_mapper),
);

halo_space::sqlbuilder::sql_struct! {
    impl User {
        id: { db: "id", tags: [], omitempty: [], quote: false, as: None },
        user_name: { db: "", tags: [], omitempty: [], quote: false, as: None }
    }
}

let s = Struct::<User>::new().with_tag(["json"]); // only fields tagged json
let sb = s.select_from("user");
let (sql, _) = sb.build();
assert!(sql.contains("user.user_name"));
```

### Scan/Addr usage

```rust
use halo_space::sqlbuilder::scan::{ScanCell, scan_tokens};

let tokens = vec!["id", "name", "42", "alice"];
let mut cells = [ScanCell::default(); 2];
let mut id: i64 = 0;
let mut name = String::new();
cells[0].addr(&mut id);
cells[1].addr(&mut name);

scan_tokens(&tokens, &mut cells).unwrap();
assert_eq!(id, 42);
assert_eq!(name, "alice");
```

### interpolate (non-parameterized)

```rust
use halo_space::sqlbuilder::interpolate::interpolate_with_flavor;
use halo_space::sqlbuilder::Flavor;

let (sql, _args) = interpolate_with_flavor(
    "SELECT * FROM user WHERE name = ? AND score >= ?",
    ["alice", 90],
    Flavor::MySQL,
).unwrap();
assert!(sql.contains("'alice'"));
assert!(sql.contains("90"));
```

### SqlValuer deferred values

```rust
use halo_space::sqlbuilder::{valuer::SqlValuer, value::SqlValue};

struct Now;
impl SqlValuer for Now {
    fn to_sql_value(&self) -> Result<SqlValue, halo_space::sqlbuilder::valuer::ValuerError> {
        Ok(SqlValue::I64(1_700_000_000)) // example: return current timestamp
    }
}

let now = Now;
let v: SqlValue = now.to_sql_value().unwrap();
assert_eq!(v, SqlValue::I64(1_700_000_000));
```

## 维护与测试

```bash
cargo fmt
cargo clippy --all-targets --all-features -- -D warnings
cargo test
```

## 许可证

MIT

## 致谢

- [huandu/go-sqlbuilder](https://github.com/huandu/go-sqlbuilder): design inspiration.
- [jzero](https://github.com/jzero-io/jzero): chain and template ideas used in examples.


