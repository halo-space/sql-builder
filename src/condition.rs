//! 条件链与条件构建器（对齐 jzero `core/stores/condition` 的链式与条件能力）。
use crate::DeleteBuilder;
use crate::cond::Cond;
use crate::flavor::{Flavor, default_flavor};
use crate::modifiers::{Arg, Builder};
use crate::select::{JoinOption, SelectBuilder};
use crate::update::UpdateBuilder;
use crate::value::SqlValue;
use crate::where_clause::{WhereClause, WhereClauseRef};
use std::collections::HashMap;
use std::sync::Arc;

/// 条件运算符。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Equal,
    NotEqual,
    IsNull,
    IsNotNull,
    GreaterThan,
    LessThan,
    GreaterEqualThan,
    LessEqualThan,
    In,
    NotIn,
    Like,
    NotLike,
    Limit,
    Offset,
    Between,
    NotBetween,
    OrderBy,
    OrderByDesc,
    OrderByAsc,
    GroupBy,
    Join,
}

/// 条件值，支持单值或列表值。
#[derive(Debug, Clone)]
pub enum ConditionValue {
    Single(Arg),
    List(Vec<Arg>),
}

impl ConditionValue {
    pub fn to_vec(&self) -> Vec<Arg> {
        match self {
            Self::Single(v) => vec![v.clone()],
            Self::List(v) => v.clone(),
        }
    }

    pub fn first(&self) -> Option<Arg> {
        match self {
            Self::Single(v) => Some(v.clone()),
            Self::List(v) => v.first().cloned(),
        }
    }

    pub fn pair(&self) -> Option<(Arg, Arg)> {
        match self {
            Self::Single(_) => None,
            Self::List(v) if v.len() >= 2 => Some((v[0].clone(), v[1].clone())),
            _ => None,
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Self::List(v) if v.is_empty())
    }
}

impl Default for ConditionValue {
    fn default() -> Self {
        Self::Single(SqlValue::Null.into())
    }
}

impl<T: Into<Arg>> From<T> for ConditionValue {
    fn from(v: T) -> Self {
        Self::Single(v.into())
    }
}

impl<T: Into<Arg>> From<Vec<T>> for ConditionValue {
    fn from(v: Vec<T>) -> Self {
        Self::List(v.into_iter().map(Into::into).collect())
    }
}

impl<T: Into<Arg>, const N: usize> From<[T; N]> for ConditionValue {
    fn from(v: [T; N]) -> Self {
        Self::List(v.into_iter().map(Into::into).collect())
    }
}

impl<T: Into<Arg>> From<HashMap<String, T>> for ConditionValue {
    fn from(v: HashMap<String, T>) -> Self {
        Self::List(v.into_values().map(Into::into).collect())
    }
}

/// 可选项：控制 skip/value 函数。
#[derive(Clone, Default)]
pub struct ChainOptions {
    pub skip: bool,
    pub skip_fn: Option<Arc<dyn Fn() -> bool + Send + Sync>>,
    pub value_fn: Option<Arc<dyn Fn() -> ConditionValue + Send + Sync>>,
    pub or_values_fn: Option<Arc<dyn Fn() -> Vec<ConditionValue> + Send + Sync>>,
}

impl ChainOptions {
    pub fn skip(mut self, skip: bool) -> Self {
        self.skip = skip;
        self
    }

    pub fn skip_fn(mut self, f: impl Fn() -> bool + Send + Sync + 'static) -> Self {
        self.skip_fn = Some(Arc::new(f));
        self
    }

    pub fn value_fn(mut self, f: impl Fn() -> ConditionValue + Send + Sync + 'static) -> Self {
        self.value_fn = Some(Arc::new(f));
        self
    }

    pub fn or_values_fn(
        mut self,
        f: impl Fn() -> Vec<ConditionValue> + Send + Sync + 'static,
    ) -> Self {
        self.or_values_fn = Some(Arc::new(f));
        self
    }
}

impl std::fmt::Debug for ChainOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ChainOptions")
            .field("skip", &self.skip)
            .field("has_skip_fn", &self.skip_fn.is_some())
            .field("has_value_fn", &self.value_fn.is_some())
            .field("has_or_values_fn", &self.or_values_fn.is_some())
            .finish()
    }
}

/// Join 条件。
#[derive(Debug, Clone)]
pub struct JoinCondition {
    pub option: Option<JoinOption>,
    pub table: String,
    pub on_expr: Vec<String>,
}

/// 组合条件。
#[derive(Clone)]
pub struct Condition {
    pub skip: bool,
    pub skip_fn: Option<Arc<dyn Fn() -> bool + Send + Sync>>,
    pub or: bool,
    pub or_operators: Vec<Operator>,
    pub or_fields: Vec<String>,
    pub or_values: Vec<ConditionValue>,
    pub or_values_fn: Option<Arc<dyn Fn() -> Vec<ConditionValue> + Send + Sync>>,
    pub field: String,
    pub operator: Operator,
    pub value: ConditionValue,
    pub value_fn: Option<Arc<dyn Fn() -> ConditionValue + Send + Sync>>,
    pub join: Option<JoinCondition>,
    pub where_clause: Option<WhereClauseRef>,
}

impl Condition {
    pub fn new(
        field: impl Into<String>,
        operator: Operator,
        value: impl Into<ConditionValue>,
    ) -> Self {
        Self {
            skip: false,
            skip_fn: None,
            or: false,
            or_operators: Vec::new(),
            or_fields: Vec::new(),
            or_values: Vec::new(),
            or_values_fn: None,
            field: field.into(),
            operator,
            value: value.into(),
            value_fn: None,
            join: None,
            where_clause: None,
        }
    }
}

impl std::fmt::Debug for Condition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Condition")
            .field("skip", &self.skip)
            .field("or", &self.or)
            .field("field", &self.field)
            .field("operator", &self.operator)
            .field("value", &self.value)
            .field("or_fields", &self.or_fields)
            .field("or_operators", &self.or_operators)
            .field("join", &self.join)
            .field("has_skip_fn", &self.skip_fn.is_some())
            .field("has_value_fn", &self.value_fn.is_some())
            .field("has_or_values_fn", &self.or_values_fn.is_some())
            .field("has_where_clause", &self.where_clause.is_some())
            .finish()
    }
}

/// 条件链。
#[derive(Debug, Default, Clone)]
pub struct Chain {
    conditions: Vec<Condition>,
}

impl Chain {
    pub fn new() -> Self {
        Self::default()
    }

    /// 修改当前链尾部的条件（若不存在条件则忽略），用于模拟 Go 版可变参 Option 的“后置修饰”体验。
    fn map_last(mut self, f: impl FnOnce(&mut Condition)) -> Self {
        if let Some(last) = self.conditions.last_mut() {
            f(last);
        }
        self
    }

    pub fn add_condition(mut self, condition: Condition) -> Self {
        self.conditions.push(condition);
        self
    }

    fn add_chain(
        mut self,
        field: impl Into<String>,
        operator: Operator,
        value: impl Into<ConditionValue>,
        opts: ChainOptions,
    ) -> Self {
        self.conditions.push(Condition {
            skip: opts.skip,
            skip_fn: opts.skip_fn,
            or: false,
            or_operators: Vec::new(),
            or_fields: Vec::new(),
            or_values: Vec::new(),
            or_values_fn: None,
            field: field.into(),
            operator,
            value: value.into(),
            value_fn: opts.value_fn,
            join: None,
            where_clause: None,
        });
        self
    }

    pub fn equal(self, field: impl Into<String>, value: impl Into<ConditionValue>) -> Self {
        self.add_chain(field, Operator::Equal, value, ChainOptions::default())
    }

    /// 为最近一次添加的条件设置 value_fn（优先级高于显式 value），贴近 Go 版 WithValueFunc。
    pub fn value_fn(self, f: impl Fn() -> ConditionValue + Send + Sync + 'static) -> Self {
        self.map_last(|c| c.value_fn = Some(Arc::new(f)))
    }

    /// 为最近一次添加的条件设置 skip，贴近 Go 版 WithSkip。
    pub fn skip(self, skip: bool) -> Self {
        self.map_last(|c| c.skip = skip)
    }

    /// 为最近一次添加的条件设置 skip_fn（优先级高于 skip），贴近 Go 版 WithSkipFunc。
    pub fn skip_fn(self, f: impl Fn() -> bool + Send + Sync + 'static) -> Self {
        self.map_last(|c| c.skip_fn = Some(Arc::new(f)))
    }

    pub fn equal_opts(
        self,
        field: impl Into<String>,
        value: impl Into<ConditionValue>,
        opts: ChainOptions,
    ) -> Self {
        self.add_chain(field, Operator::Equal, value, opts)
    }

    pub fn not_equal(self, field: impl Into<String>, value: impl Into<ConditionValue>) -> Self {
        self.add_chain(field, Operator::NotEqual, value, ChainOptions::default())
    }

    pub fn is_null(self, field: impl Into<String>) -> Self {
        self.add_chain(
            field,
            Operator::IsNull,
            ConditionValue::default(),
            ChainOptions::default(),
        )
    }

    pub fn is_not_null(self, field: impl Into<String>) -> Self {
        self.add_chain(
            field,
            Operator::IsNotNull,
            ConditionValue::default(),
            ChainOptions::default(),
        )
    }

    pub fn greater_than(self, field: impl Into<String>, value: impl Into<ConditionValue>) -> Self {
        self.add_chain(field, Operator::GreaterThan, value, ChainOptions::default())
    }

    pub fn less_than(self, field: impl Into<String>, value: impl Into<ConditionValue>) -> Self {
        self.add_chain(field, Operator::LessThan, value, ChainOptions::default())
    }

    pub fn greater_equal_than(
        self,
        field: impl Into<String>,
        value: impl Into<ConditionValue>,
    ) -> Self {
        self.add_chain(
            field,
            Operator::GreaterEqualThan,
            value,
            ChainOptions::default(),
        )
    }

    pub fn less_equal_than(
        self,
        field: impl Into<String>,
        value: impl Into<ConditionValue>,
    ) -> Self {
        self.add_chain(
            field,
            Operator::LessEqualThan,
            value,
            ChainOptions::default(),
        )
    }

    pub fn like(self, field: impl Into<String>, value: impl Into<ConditionValue>) -> Self {
        self.add_chain(field, Operator::Like, value, ChainOptions::default())
    }

    pub fn not_like(self, field: impl Into<String>, value: impl Into<ConditionValue>) -> Self {
        self.add_chain(field, Operator::NotLike, value, ChainOptions::default())
    }

    pub fn between(self, field: impl Into<String>, value: impl Into<ConditionValue>) -> Self {
        self.add_chain(field, Operator::Between, value, ChainOptions::default())
    }

    pub fn in_(self, field: impl Into<String>, value: impl Into<ConditionValue>) -> Self {
        self.add_chain(field, Operator::In, value, ChainOptions::default())
    }

    pub fn not_in(self, field: impl Into<String>, value: impl Into<ConditionValue>) -> Self {
        self.add_chain(field, Operator::NotIn, value, ChainOptions::default())
    }

    pub fn or(
        mut self,
        fields: impl IntoIterator<Item = impl Into<String>>,
        operators: impl IntoIterator<Item = Operator>,
        values: impl IntoIterator<Item = impl Into<ConditionValue>>,
        opts: ChainOptions,
    ) -> Self {
        let mut cond = Condition {
            skip: opts.skip,
            skip_fn: opts.skip_fn,
            or: true,
            or_operators: operators.into_iter().collect(),
            or_fields: fields.into_iter().map(Into::into).collect(),
            or_values: values.into_iter().map(Into::into).collect(),
            or_values_fn: opts.or_values_fn,
            field: String::new(),
            operator: Operator::Equal,
            value: ConditionValue::default(),
            value_fn: None,
            join: None,
            where_clause: None,
        };

        if let Some(f) = opts.value_fn {
            cond.value_fn = Some(f);
        }

        self.conditions.push(cond);
        self
    }

    pub fn order_by(self, value: impl Into<ConditionValue>) -> Self {
        self.add_chain("", Operator::OrderBy, value, ChainOptions::default())
    }

    pub fn order_by_desc(self, field: impl Into<String>) -> Self {
        self.add_chain(
            field,
            Operator::OrderByDesc,
            ConditionValue::default(),
            ChainOptions::default(),
        )
    }

    pub fn order_by_asc(self, field: impl Into<String>) -> Self {
        self.add_chain(
            field,
            Operator::OrderByAsc,
            ConditionValue::default(),
            ChainOptions::default(),
        )
    }

    pub fn limit(self, value: impl Into<ConditionValue>) -> Self {
        self.add_chain("", Operator::Limit, value, ChainOptions::default())
    }

    pub fn offset(self, value: impl Into<ConditionValue>) -> Self {
        self.add_chain("", Operator::Offset, value, ChainOptions::default())
    }

    pub fn page(self, page: i64, page_size: i64) -> Self {
        let offset = (page - 1) * page_size;
        self.offset(offset).limit(page_size)
    }

    pub fn group_by(self, field: impl Into<String>) -> Self {
        self.add_chain(
            field,
            Operator::GroupBy,
            ConditionValue::default(),
            ChainOptions::default(),
        )
    }

    pub fn join(
        mut self,
        option: JoinOption,
        table: impl Into<String>,
        on_expr: impl IntoIterator<Item = impl Into<String>>,
    ) -> Self {
        self.conditions.push(Condition {
            skip: false,
            skip_fn: None,
            or: false,
            or_operators: Vec::new(),
            or_fields: Vec::new(),
            or_values: Vec::new(),
            or_values_fn: None,
            field: String::new(),
            operator: Operator::Join,
            value: ConditionValue::default(),
            value_fn: None,
            join: Some(JoinCondition {
                option: Some(option),
                table: table.into(),
                on_expr: on_expr.into_iter().map(Into::into).collect(),
            }),
            where_clause: None,
        });
        self
    }

    pub fn where_clause(mut self, wc: WhereClauseRef) -> Self {
        self.conditions.push(Condition {
            skip: false,
            skip_fn: None,
            or: false,
            or_operators: Vec::new(),
            or_fields: Vec::new(),
            or_values: Vec::new(),
            or_values_fn: None,
            field: String::new(),
            operator: Operator::Equal,
            value: ConditionValue::default(),
            value_fn: None,
            join: None,
            where_clause: Some(wc),
        });
        self
    }

    pub fn build(self) -> Vec<Condition> {
        self.conditions
    }
}

/// UpdateField 操作类型。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpdateFieldOperator {
    Incr,
    Decr,
    Assign,
    Add,
    Sub,
    Mul,
    Div,
}

/// UpdateField 可选项。
#[derive(Clone, Default)]
pub struct UpdateFieldOptions {
    pub skip: bool,
    pub skip_fn: Option<Arc<dyn Fn() -> bool + Send + Sync>>,
    pub value_fn: Option<Arc<dyn Fn() -> Arg + Send + Sync>>,
}

impl UpdateFieldOptions {
    pub fn skip(mut self, skip: bool) -> Self {
        self.skip = skip;
        self
    }

    pub fn skip_fn(mut self, f: impl Fn() -> bool + Send + Sync + 'static) -> Self {
        self.skip_fn = Some(Arc::new(f));
        self
    }

    pub fn value_fn(mut self, f: impl Fn() -> Arg + Send + Sync + 'static) -> Self {
        self.value_fn = Some(Arc::new(f));
        self
    }
}

impl std::fmt::Debug for UpdateFieldOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UpdateFieldOptions")
            .field("skip", &self.skip)
            .field("has_skip_fn", &self.skip_fn.is_some())
            .field("has_value_fn", &self.value_fn.is_some())
            .finish()
    }
}

/// UpdateField 描述。
#[derive(Clone)]
pub struct UpdateField {
    pub skip: bool,
    pub skip_fn: Option<Arc<dyn Fn() -> bool + Send + Sync>>,
    pub field: String,
    pub operator: UpdateFieldOperator,
    pub value: Option<Arg>,
    pub value_fn: Option<Arc<dyn Fn() -> Arg + Send + Sync>>,
}

impl UpdateField {
    pub fn new(
        field: impl Into<String>,
        operator: UpdateFieldOperator,
        value: Option<Arg>,
        opts: UpdateFieldOptions,
    ) -> Self {
        Self {
            skip: opts.skip,
            skip_fn: opts.skip_fn,
            field: field.into(),
            operator,
            value,
            value_fn: opts.value_fn,
        }
    }
}

impl std::fmt::Debug for UpdateField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UpdateField")
            .field("skip", &self.skip)
            .field("field", &self.field)
            .field("operator", &self.operator)
            .field("value", &self.value)
            .field("has_skip_fn", &self.skip_fn.is_some())
            .field("has_value_fn", &self.value_fn.is_some())
            .finish()
    }
}

/// UpdateField 链。
#[derive(Debug, Default, Clone)]
pub struct UpdateFieldChain {
    fields: Vec<UpdateField>,
}

impl UpdateFieldChain {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn assign(
        mut self,
        field: impl Into<String>,
        value: impl Into<Arg>,
        opts: UpdateFieldOptions,
    ) -> Self {
        self.fields.push(UpdateField::new(
            field,
            UpdateFieldOperator::Assign,
            Some(value.into()),
            opts,
        ));
        self
    }

    pub fn incr(mut self, field: impl Into<String>, opts: UpdateFieldOptions) -> Self {
        self.fields.push(UpdateField::new(
            field,
            UpdateFieldOperator::Incr,
            None,
            opts,
        ));
        self
    }

    pub fn decr(mut self, field: impl Into<String>, opts: UpdateFieldOptions) -> Self {
        self.fields.push(UpdateField::new(
            field,
            UpdateFieldOperator::Decr,
            None,
            opts,
        ));
        self
    }

    pub fn add(
        mut self,
        field: impl Into<String>,
        value: impl Into<Arg>,
        opts: UpdateFieldOptions,
    ) -> Self {
        self.fields.push(UpdateField::new(
            field,
            UpdateFieldOperator::Add,
            Some(value.into()),
            opts,
        ));
        self
    }

    pub fn sub(
        mut self,
        field: impl Into<String>,
        value: impl Into<Arg>,
        opts: UpdateFieldOptions,
    ) -> Self {
        self.fields.push(UpdateField::new(
            field,
            UpdateFieldOperator::Sub,
            Some(value.into()),
            opts,
        ));
        self
    }

    pub fn mul(
        mut self,
        field: impl Into<String>,
        value: impl Into<Arg>,
        opts: UpdateFieldOptions,
    ) -> Self {
        self.fields.push(UpdateField::new(
            field,
            UpdateFieldOperator::Mul,
            Some(value.into()),
            opts,
        ));
        self
    }

    pub fn div(
        mut self,
        field: impl Into<String>,
        value: impl Into<Arg>,
        opts: UpdateFieldOptions,
    ) -> Self {
        self.fields.push(UpdateField::new(
            field,
            UpdateFieldOperator::Div,
            Some(value.into()),
            opts,
        ));
        self
    }

    pub fn build(self) -> Vec<(String, UpdateValue)> {
        let mut out = Vec::new();
        for mut field in self.fields {
            if let Some(f) = &field.skip_fn {
                if f() {
                    continue;
                }
            } else if field.skip {
                continue;
            }
            if let Some(f) = &field.value_fn {
                field.value = Some(f());
            }
            out.push((field.field.clone(), UpdateValue::from(field)));
        }
        out
    }
}

/// Update 更新值。
#[derive(Debug, Clone)]
pub enum UpdateValue {
    Field(UpdateField),
    Value(Arg),
}

impl From<UpdateField> for UpdateValue {
    fn from(v: UpdateField) -> Self {
        Self::Field(v)
    }
}

impl<T: Into<Arg>> From<T> for UpdateValue {
    fn from(v: T) -> Self {
        Self::Value(v.into())
    }
}

/// 将字段名转为字符串切片。
pub fn to_field_slice(fields: Vec<String>) -> Vec<String> {
    fields
}

/// 去除首尾反引号/双引号。
pub fn unquote(s: &str) -> String {
    let mut out = s.trim();
    if out.starts_with('`') || out.starts_with('"') {
        out = &out[1..];
    }
    if out.ends_with('`') || out.ends_with('"') {
        out = &out[..out.len() - 1];
    }
    out.to_string()
}

/// 按 Flavor 对字段名逐段 Quote（按 `.` 切分）。
pub fn quote_with_flavor(flavor: Flavor, s: &str) -> String {
    let parts: Vec<String> = s
        .split('.')
        .filter(|p| !p.is_empty())
        .map(|p| flavor.quote(&unquote(p)))
        .collect();
    parts.join(".")
}

fn should_skip(cond: &Condition) -> bool {
    if let Some(f) = &cond.skip_fn {
        return f();
    }
    cond.skip
}

fn materialize_value(cond: &Condition) -> ConditionValue {
    if let Some(f) = &cond.value_fn {
        f()
    } else {
        cond.value.clone()
    }
}

fn materialize_or_values(cond: &Condition) -> Vec<ConditionValue> {
    if let Some(f) = &cond.or_values_fn {
        f()
    } else {
        cond.or_values.clone()
    }
}

fn arg_to_string(arg: &Arg) -> Option<String> {
    match arg {
        Arg::Value(SqlValue::String(s)) => Some(s.to_string()),
        Arg::Value(SqlValue::I64(v)) => Some(v.to_string()),
        Arg::Value(SqlValue::U64(v)) => Some(v.to_string()),
        Arg::Value(SqlValue::F64(v)) => Some(v.to_string()),
        Arg::Value(SqlValue::Bool(v)) => Some(v.to_string()),
        Arg::SqlNamed(v) => Some(format!("@{}", v.name)),
        Arg::Raw(v) => Some(v.expr.clone()),
        _ => None,
    }
}

fn value_to_strings(value: &ConditionValue) -> Vec<String> {
    match value {
        ConditionValue::Single(v) => arg_to_string(v).into_iter().collect(),
        ConditionValue::List(vs) => vs.iter().filter_map(arg_to_string).collect(),
    }
}

fn value_to_i64(value: &ConditionValue) -> Option<i64> {
    match value {
        ConditionValue::Single(Arg::Value(SqlValue::I64(v))) => Some(*v),
        ConditionValue::Single(Arg::Value(SqlValue::U64(v))) => Some(*v as i64),
        ConditionValue::Single(Arg::Value(SqlValue::F64(v))) => Some(*v as i64),
        ConditionValue::Single(Arg::Value(SqlValue::Bool(v))) => Some(if *v { 1 } else { 0 }),
        ConditionValue::Single(Arg::Value(SqlValue::Null)) => Some(0),
        _ => None,
    }
}

fn build_expr(
    flavor: Flavor,
    cond: &Cond,
    field: &str,
    operator: Operator,
    value: &ConditionValue,
) -> Option<String> {
    let quoted_field = quote_with_flavor(flavor, field);
    match operator {
        Operator::Equal => value.first().map(|v| cond.equal(&quoted_field, v)),
        Operator::NotEqual => value.first().map(|v| cond.not_equal(&quoted_field, v)),
        Operator::GreaterThan => value.first().map(|v| cond.greater_than(&quoted_field, v)),
        Operator::LessThan => value.first().map(|v| cond.less_than(&quoted_field, v)),
        Operator::GreaterEqualThan => value
            .first()
            .map(|v| cond.greater_equal_than(&quoted_field, v)),
        Operator::LessEqualThan => value
            .first()
            .map(|v| cond.less_equal_than(&quoted_field, v)),
        Operator::Like => value.first().map(|v| cond.like(&quoted_field, v)),
        Operator::NotLike => value.first().map(|v| cond.not_like(&quoted_field, v)),
        Operator::IsNull => Some(cond.is_null(&quoted_field)),
        Operator::IsNotNull => Some(cond.is_not_null(&quoted_field)),
        Operator::Between => value.pair().map(|(l, r)| cond.between(&quoted_field, l, r)),
        Operator::NotBetween => value
            .pair()
            .map(|(l, r)| cond.not_between(&quoted_field, l, r)),
        Operator::In => {
            let vals = value.to_vec();
            if vals.is_empty() {
                let ph = cond.var(SqlValue::Null);
                Some(format!("{quoted_field} IN ({ph})"))
            } else {
                let phs: Vec<String> = vals.into_iter().map(|v| cond.var(v)).collect();
                Some(format!("{quoted_field} IN ({})", phs.join(", ")))
            }
        }
        Operator::NotIn => {
            let vals = value.to_vec();
            if vals.is_empty() {
                let ph = cond.var(SqlValue::Null);
                Some(format!("{quoted_field} NOT IN ({ph})"))
            } else {
                let phs: Vec<String> = vals.into_iter().map(|v| cond.var(v)).collect();
                Some(format!("{quoted_field} NOT IN ({})", phs.join(", ")))
            }
        }
        _ => None,
    }
}

fn build_where_clause(flavor: Flavor, conditions: &[Condition]) -> Option<WhereClauseRef> {
    if conditions.is_empty() {
        return None;
    }
    let wc = WhereClause::new();
    let cond_builder = Cond::new();
    let mut has_expr = false;

    for c in conditions {
        if should_skip(c) {
            continue;
        }
        if let Some(w) = &c.where_clause {
            wc.borrow_mut().add_where_clause(&w.borrow());
            has_expr = true;
            continue;
        }

        if c.or {
            let or_values = materialize_or_values(c);
            let iter_len = c
                .or_fields
                .len()
                .min(c.or_operators.len())
                .min(or_values.len());
            let mut exprs = Vec::new();
            for (i, value) in or_values.iter().enumerate().take(iter_len) {
                match build_expr(
                    flavor,
                    &cond_builder,
                    &c.or_fields[i],
                    c.or_operators[i],
                    value,
                ) {
                    Some(expr) if !expr.is_empty() => exprs.push(expr),
                    _ => {}
                }
            }
            if !exprs.is_empty() {
                let combined = cond_builder.or(exprs);
                wc.borrow_mut()
                    .add_where_expr(cond_builder.args.clone(), [combined]);
                has_expr = true;
            }
        } else {
            match build_expr(
                flavor,
                &cond_builder,
                &c.field,
                c.operator,
                &materialize_value(c),
            ) {
                Some(expr) if !expr.is_empty() => {
                    wc.borrow_mut()
                        .add_where_expr(cond_builder.args.clone(), [expr]);
                    has_expr = true;
                }
                _ => {}
            }
        }
    }

    if has_expr { Some(wc) } else { None }
}

fn apply_select_condition(flavor: Flavor, builder: &mut SelectBuilder, condition: &Condition) {
    if should_skip(condition) {
        return;
    }
    let value = materialize_value(condition);
    match condition.operator {
        Operator::Limit => {
            if let Some(v) = value_to_i64(&value) {
                builder.limit(v);
            }
        }
        Operator::Offset => {
            if let Some(v) = value_to_i64(&value) {
                builder.offset(v);
            }
        }
        Operator::OrderBy => {
            let cols = value_to_strings(&value);
            if !cols.is_empty() {
                builder.order_by(cols);
            }
        }
        Operator::OrderByDesc => {
            builder.order_by_desc(quote_with_flavor(flavor, &condition.field));
        }
        Operator::OrderByAsc => {
            builder.order_by_asc(quote_with_flavor(flavor, &condition.field));
        }
        Operator::GroupBy => {
            let cols = value_to_strings(&value);
            if !cols.is_empty() {
                builder.group_by(cols);
            } else if !condition.field.is_empty() {
                builder.group_by(vec![quote_with_flavor(flavor, &condition.field)]);
            }
        }
        Operator::Join => {
            if let Some(join) = &condition.join {
                builder.join_with_option(join.option, join.table.clone(), join.on_expr.clone());
            }
        }
        _ => {}
    }
}

fn apply_update_condition(flavor: Flavor, builder: &mut UpdateBuilder, condition: &Condition) {
    if should_skip(condition) {
        return;
    }
    let value = materialize_value(condition);
    match condition.operator {
        Operator::Limit => {
            if let Some(v) = value_to_i64(&value) {
                builder.limit(v);
            }
        }
        Operator::OrderBy => {
            let cols = value_to_strings(&value);
            if !cols.is_empty() {
                builder.order_by(cols);
            }
        }
        Operator::OrderByDesc => {
            builder.order_by_desc(quote_with_flavor(flavor, &condition.field));
        }
        Operator::OrderByAsc => {
            builder.order_by_asc(quote_with_flavor(flavor, &condition.field));
        }
        _ => {}
    }
}

fn apply_delete_condition(flavor: Flavor, builder: &mut DeleteBuilder, condition: &Condition) {
    if should_skip(condition) {
        return;
    }
    let value = materialize_value(condition);
    match condition.operator {
        Operator::Limit => {
            if let Some(v) = value_to_i64(&value) {
                builder.limit(v);
            }
        }
        Operator::OrderBy => {
            let cols = value_to_strings(&value);
            if !cols.is_empty() {
                builder.order_by(cols);
            }
        }
        Operator::OrderByDesc => {
            builder.order_by_desc(quote_with_flavor(flavor, &condition.field));
        }
        Operator::OrderByAsc => {
            builder.order_by_asc(quote_with_flavor(flavor, &condition.field));
        }
        _ => {}
    }
}

/// 构建 SELECT。
pub fn build_select(
    builder: SelectBuilder,
    conditions: impl IntoIterator<Item = Condition>,
) -> (String, Vec<Arg>) {
    build_select_with_flavor(default_flavor(), builder, conditions)
}

/// 构建 SELECT（指定 Flavor）。
pub fn build_select_with_flavor(
    flavor: Flavor,
    mut builder: SelectBuilder,
    conditions: impl IntoIterator<Item = Condition>,
) -> (String, Vec<Arg>) {
    builder.set_flavor(flavor);
    let conditions: Vec<Condition> = conditions.into_iter().collect();
    if let Some(wc) = build_where_clause(flavor, &conditions) {
        builder.add_where_clause_ref(&wc);
    }
    for c in &conditions {
        apply_select_condition(flavor, &mut builder, c);
    }
    builder.build_with_flavor(flavor, &[])
}

/// 构建 UPDATE。
pub fn build_update(
    builder: UpdateBuilder,
    data: impl IntoIterator<Item = (impl Into<String>, impl Into<UpdateValue>)>,
    conditions: impl IntoIterator<Item = Condition>,
) -> (String, Vec<Arg>) {
    build_update_with_flavor(default_flavor(), builder, data, conditions)
}

/// 构建 UPDATE（指定 Flavor）。
pub fn build_update_with_flavor(
    flavor: Flavor,
    mut builder: UpdateBuilder,
    data: impl IntoIterator<Item = (impl Into<String>, impl Into<UpdateValue>)>,
    conditions: impl IntoIterator<Item = Condition>,
) -> (String, Vec<Arg>) {
    builder.set_flavor(flavor);
    let conditions: Vec<Condition> = conditions.into_iter().collect();
    if let Some(wc) = build_where_clause(flavor, &conditions) {
        builder.add_where_clause_ref(&wc);
    }
    for c in &conditions {
        apply_update_condition(flavor, &mut builder, c);
    }

    for (field, value) in data {
        let field = field.into();
        match value.into() {
            UpdateValue::Value(v) => {
                builder.set_more([builder.assign(&quote_with_flavor(flavor, &field), v)]);
            }
            UpdateValue::Field(mut f) => {
                if let Some(skip_fn) = &f.skip_fn {
                    if skip_fn() {
                        continue;
                    }
                } else if f.skip {
                    continue;
                }
                if let Some(func) = &f.value_fn {
                    f.value = Some(func());
                }
                let quoted = quote_with_flavor(flavor, &f.field);
                match f.operator {
                    UpdateFieldOperator::Assign => {
                        if let Some(v) = f.value.clone() {
                            builder.set_more([builder.assign(&quoted, v)]);
                        }
                    }
                    UpdateFieldOperator::Incr => {
                        builder.set_more([builder.incr(&quoted)]);
                    }
                    UpdateFieldOperator::Decr => {
                        builder.set_more([builder.decr(&quoted)]);
                    }
                    UpdateFieldOperator::Add => {
                        if let Some(v) = f.value.clone() {
                            builder.set_more([builder.add_(&quoted, v)]);
                        }
                    }
                    UpdateFieldOperator::Sub => {
                        if let Some(v) = f.value.clone() {
                            builder.set_more([builder.sub(&quoted, v)]);
                        }
                    }
                    UpdateFieldOperator::Mul => {
                        if let Some(v) = f.value.clone() {
                            builder.set_more([builder.mul(&quoted, v)]);
                        }
                    }
                    UpdateFieldOperator::Div => {
                        if let Some(v) = f.value.clone() {
                            builder.set_more([builder.div(&quoted, v)]);
                        }
                    }
                }
            }
        }
    }

    builder.build_with_flavor(flavor, &[])
}

/// 构建 DELETE。
pub fn build_delete(
    builder: DeleteBuilder,
    conditions: impl IntoIterator<Item = Condition>,
) -> (String, Vec<Arg>) {
    build_delete_with_flavor(default_flavor(), builder, conditions)
}

/// 构建 DELETE（指定 Flavor）。
pub fn build_delete_with_flavor(
    flavor: Flavor,
    mut builder: DeleteBuilder,
    conditions: impl IntoIterator<Item = Condition>,
) -> (String, Vec<Arg>) {
    builder.set_flavor(flavor);
    let conditions: Vec<Condition> = conditions.into_iter().collect();
    if let Some(wc) = build_where_clause(flavor, &conditions) {
        builder.add_where_clause_ref(&wc);
    }
    for c in &conditions {
        apply_delete_condition(flavor, &mut builder, c);
    }
    builder.build_with_flavor(flavor, &[])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::select::SelectBuilder;
    use crate::update::UpdateBuilder;
    use crate::{DeleteBuilder, flavor::set_default_flavor};
    use pretty_assertions::assert_eq;

    #[test]
    fn select_with_condition_like_go() {
        set_default_flavor(Flavor::MySQL);
        let between = vec![
            ConditionValue::from(vec![24_i64, 48]),
            ConditionValue::from(vec![170_i64, 175]),
        ];
        let conditions = vec![
            Condition::new("name", Operator::Equal, "jaronnie"),
            Condition {
                skip: false,
                skip_fn: None,
                or: true,
                or_operators: vec![Operator::Between, Operator::Between],
                or_fields: vec!["age".into(), "height".into()],
                or_values: between,
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
        let (sql, args) = build_select(sb, conditions);
        assert_eq!(
            "SELECT name, age, height FROM user WHERE `name` = ? AND (`age` BETWEEN ? AND ? OR `height` BETWEEN ? AND ?)",
            sql
        );
        assert_eq!(
            args,
            vec![
                Arg::from("jaronnie"),
                Arg::from(24_i64),
                Arg::from(48_i64),
                Arg::from(170_i64),
                Arg::from(175_i64)
            ]
        );
    }

    #[test]
    fn chain_basic_and_order() {
        let chain = Chain::new()
            .equal_opts("field1", "value1", ChainOptions::default().skip(true))
            .equal("field2", "value2")
            .order_by_desc("create_time")
            .order_by_asc("sort");
        let mut sb = SelectBuilder::new();
        sb.select(vec!["name", "age"]).from(vec!["user"]);
        let (sql, args) = build_select_with_flavor(Flavor::MySQL, sb, chain.build());
        assert_eq!(
            "SELECT name, age FROM user WHERE `field2` = ? ORDER BY `create_time` DESC, `sort` ASC",
            sql
        );
        assert_eq!(args, vec![Arg::from("value2")]);
    }

    #[test]
    fn chain_join_and_null() {
        let chain = Chain::new()
            .equal("user.field", "value2")
            .join(
                JoinOption::InnerJoin,
                "user_info",
                ["user.id = user_info.user_id"],
            )
            .is_null("delete_at")
            .is_not_null("updated_at");
        let mut sb = SelectBuilder::new();
        sb.select(vec!["user.name", "user.age"]).from(vec!["user"]);
        let (sql, args) = build_select_with_flavor(Flavor::MySQL, sb, chain.build());
        assert_eq!(
            "SELECT user.name, user.age FROM user INNER JOIN user_info ON user.id = user_info.user_id WHERE `user`.`field` = ? AND `delete_at` IS NULL AND `updated_at` IS NOT NULL",
            sql
        );
        assert_eq!(args, vec![Arg::from("value2")]);
    }

    #[test]
    fn chain_equal_fluent_modifiers() {
        let chain = Chain::new()
            .equal("name", "placeholder")
            .value_fn(|| ConditionValue::from("jzero"))
            .skip(false)
            .skip_fn(|| false);

        let mut sb = SelectBuilder::new();
        sb.select(vec!["id", "name"]).from(vec!["user"]);
        let (sql, args) = build_select_with_flavor(Flavor::MySQL, sb, chain.build());

        assert_eq!("SELECT id, name FROM user WHERE `name` = ?", sql);
        assert_eq!(args, vec![Arg::from("jzero")]);
    }

    #[test]
    fn chain_page_and_group_by() {
        let chain = Chain::new()
            .equal("status", "active")
            .group_by("status")
            .page(2, 10)
            .order_by(vec!["status"]);

        let mut sb = SelectBuilder::new();
        sb.select(vec!["status", "COUNT(1)"]).from(vec!["users"]);
        let (sql, args) = build_select_with_flavor(Flavor::MySQL, sb, chain.build());

        assert_eq!(
            "SELECT status, COUNT(1) FROM users WHERE `status` = ? GROUP BY `status` ORDER BY status LIMIT ? OFFSET ?",
            sql
        );
        assert_eq!(
            args,
            vec![Arg::from("active"), Arg::from(10_i64), Arg::from(10_i64)]
        );
    }

    #[test]
    fn condition_value_fn_and_skip_fn() {
        let conds = vec![
            Condition {
                skip: false,
                skip_fn: Some(Arc::new(|| true)),
                or: false,
                or_operators: Vec::new(),
                or_fields: Vec::new(),
                or_values: Vec::new(),
                or_values_fn: None,
                field: "skip_me".into(),
                operator: Operator::Equal,
                value: ConditionValue::from("never"),
                value_fn: None,
                join: None,
                where_clause: None,
            },
            Condition {
                skip: false,
                skip_fn: None,
                or: false,
                or_operators: Vec::new(),
                or_fields: Vec::new(),
                or_values: Vec::new(),
                or_values_fn: None,
                field: "name".into(),
                operator: Operator::Equal,
                value: ConditionValue::from("placeholder"),
                value_fn: Some(Arc::new(|| ConditionValue::from("dynamic"))),
                join: None,
                where_clause: None,
            },
        ];

        let mut sb = SelectBuilder::new();
        sb.select(vec!["id"]).from(vec!["users"]);
        let (sql, args) = build_select_with_flavor(Flavor::MySQL, sb, conds);

        assert_eq!("SELECT id FROM users WHERE `name` = ?", sql);
        assert_eq!(args, vec![Arg::from("dynamic")]);
    }

    #[test]
    fn condition_delete_skip_and_value_func() {
        let conds = vec![
            Condition {
                skip: false,
                skip_fn: Some(Arc::new(|| true)),
                or: false,
                or_operators: Vec::new(),
                or_fields: Vec::new(),
                or_values: Vec::new(),
                or_values_fn: None,
                field: "name".into(),
                operator: Operator::Equal,
                value: ConditionValue::from("jaronnie"),
                value_fn: Some(Arc::new(|| ConditionValue::from("jaronnie2"))),
                join: None,
                where_clause: None,
            },
            Condition {
                skip: false,
                skip_fn: None,
                or: true,
                or_operators: vec![Operator::Between, Operator::Between],
                or_fields: vec!["age".into(), "height".into()],
                or_values: vec![
                    ConditionValue::from(vec![24_i64, 48]),
                    ConditionValue::from(vec![170_i64, 175]),
                ],
                or_values_fn: Some(Arc::new(|| {
                    vec![
                        ConditionValue::from(vec![24_i64, 49]),
                        ConditionValue::from(vec![170_i64, 176]),
                    ]
                })),
                field: String::new(),
                operator: Operator::Between,
                value: ConditionValue::default(),
                value_fn: None,
                join: None,
                where_clause: None,
            },
        ];
        let mut db = DeleteBuilder::new();
        db.delete_from(vec!["user"]);
        let (sql, args) = build_delete(db, conds);
        assert_eq!(
            "DELETE FROM user WHERE (`age` BETWEEN ? AND ? OR `height` BETWEEN ? AND ?)",
            sql
        );
        assert_eq!(
            args,
            vec![
                Arg::from(24_i64),
                Arg::from(49_i64),
                Arg::from(170_i64),
                Arg::from(176_i64)
            ]
        );
    }

    #[test]
    fn update_with_update_field_chain() {
        let data = UpdateFieldChain::new()
            .assign("name", "jaronnie", UpdateFieldOptions::default().skip(true))
            .incr("version", UpdateFieldOptions::default())
            .add(
                "age",
                12_i64,
                UpdateFieldOptions::default().value_fn(|| Arg::from(15_i64)),
            )
            .build();

        let chain = Chain::new().equal("id", 1_i64);
        let mut ub = UpdateBuilder::new();
        ub.update(vec!["users"]);
        let (sql, args) = build_update_with_flavor(Flavor::MySQL, ub, data, chain.build());
        assert_eq!(
            "UPDATE users SET `version` = `version` + 1, `age` = `age` + ? WHERE `id` = ?",
            sql
        );
        assert_eq!(args, vec![Arg::from(15_i64), Arg::from(1_i64)]);
    }

    #[test]
    fn condition_in_allows_empty_slice() {
        let mut sb = SelectBuilder::new();
        sb.select(vec!["id"]).from(vec!["users"]);
        let (sql, args) = build_select(
            sb,
            [Condition::new(
                "id",
                Operator::In,
                ConditionValue::from(Vec::<i64>::new()),
            )],
        );
        assert_eq!("SELECT id FROM users WHERE `id` IN (?)", sql);
        assert_eq!(args, vec![Arg::from(SqlValue::Null)]);
    }
}
