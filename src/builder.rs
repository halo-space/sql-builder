//! Build / BuildNamed / Buildf: helpers for format-style SQL builders.

use crate::args::Args;
use crate::flavor::Flavor;
use crate::modifiers::{Arg, Builder, escape, named};

#[derive(Debug, Clone)]
struct CompiledBuilder {
    args: Args,
    format: String,
}

impl CompiledBuilder {
    fn new(args: Args, format: String) -> Self {
        Self { args, format }
    }
}

impl Builder for CompiledBuilder {
    fn build_with_flavor(&self, flavor: Flavor, initial_arg: &[Arg]) -> (String, Vec<Arg>) {
        self.args
            .compile_with_flavor(&self.format, flavor, initial_arg)
    }

    fn flavor(&self) -> Flavor {
        self.args.flavor
    }
}

#[derive(Clone)]
struct FlavoredBuilder {
    inner: Box<dyn Builder>,
    flavor: Flavor,
}

impl Builder for FlavoredBuilder {
    fn build_with_flavor(&self, flavor: Flavor, initial_arg: &[Arg]) -> (String, Vec<Arg>) {
        self.inner.build_with_flavor(flavor, initial_arg)
    }

    fn flavor(&self) -> Flavor {
        self.flavor
    }
}

/// WithFlavor: bind a default flavor to a builder.
pub fn with_flavor(builder: impl Builder + 'static, flavor: Flavor) -> Box<dyn Builder> {
    Box::new(FlavoredBuilder {
        inner: Box::new(builder),
        flavor,
    })
}

/// Build: construct a builder using `$` placeholders.
pub fn build(
    format: impl Into<String>,
    args_in: impl IntoIterator<Item = impl Into<Arg>>,
) -> Box<dyn Builder> {
    let mut args = Args::default();
    for a in args_in {
        args.add(a);
    }
    Box::new(CompiledBuilder::new(args, format.into()))
}

/// BuildNamed: enable only `${name}` and `$$`, sourcing args from a map.
pub fn build_named(
    format: impl Into<String>,
    named_map: impl IntoIterator<Item = (String, Arg)>,
) -> Box<dyn Builder> {
    let mut args = Args {
        only_named: true,
        ..Args::default()
    };

    for (k, v) in named_map {
        args.add(named(k, v));
    }

    Box::new(CompiledBuilder::new(args, format.into()))
}

/// Buildf: fmt-like builder supporting `%v`/`%s` only.
pub fn buildf(format: &str, args_in: impl IntoIterator<Item = impl Into<Arg>>) -> Box<dyn Builder> {
    let mut args = Args::default();
    let escaped = escape(format);
    let mut out = String::new();

    let mut it = args_in.into_iter();
    let mut chars = escaped.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '%' {
            match chars.peek().copied() {
                Some('v') | Some('s') => {
                    chars.next();
                    if let Some(a) = it.next() {
                        let ph = args.add(a.into());
                        out.push_str(&ph);
                    } else {
                        // 没有足够参数：按字面输出，保持行为可见
                        out.push('%');
                        out.push('v');
                    }
                }
                Some('%') => {
                    chars.next();
                    out.push('%');
                }
                _ => out.push('%'),
            }
        } else {
            out.push(c);
        }
    }

    // Ignore extra arguments, mirroring fmt-like behavior (unused args are dropped).
    Box::new(CompiledBuilder::new(args, out))
}
