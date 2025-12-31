use criterion::{Criterion, black_box, criterion_group, criterion_main};
use halo_space::Flavor;
use halo_space::modifiers::Builder;
use halo_space::select::SelectBuilder;

fn bench_select_build(c: &mut Criterion) {
    c.bench_function("select_build_join_where_mysql", |b| {
        b.iter(|| {
            let mut sb = SelectBuilder::new();
            sb.select(vec!["id", "name", "score"]);
            sb.from(["users", "user_ext"]);
            sb.join("teams", ["users.team_id = teams.id"]);
            sb.where_(["score >= 100", "status = 'active'"]);
            sb.order_by(["score DESC", "id"]);

            let (sql, args) = sb.build_with_flavor(Flavor::MySQL, &[]);
            black_box((sql, args));
        });
    });
}

criterion_group!(benches, bench_select_build);
criterion_main!(benches);
