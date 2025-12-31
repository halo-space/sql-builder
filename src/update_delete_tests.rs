#[cfg(test)]
mod tests {
    use crate::modifiers::Builder;
    use crate::{DeleteBuilder, Flavor, UpdateBuilder, set_default_flavor_scoped};
    use pretty_assertions::assert_eq;

    #[test]
    fn update_basic_set_where() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let mut ub = UpdateBuilder::new();
        ub.update(["users"]);
        ub.set([ub.assign("level", 10_i64)]);
        ub.where_([ub.equal("id", 1234_i64)]);
        let (sql, args) = ub.build();
        assert_eq!(sql, "UPDATE users SET level = ? WHERE id = ?");
        assert_eq!(args.len(), 2);
    }

    #[test]
    fn update_returning_postgres() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let mut ub = UpdateBuilder::new();
        ub.update(["users"]);
        let set_expr = ub.assign("level", 10_i64);
        let where_expr = ub.equal("id", 1234_i64);
        ub.set([set_expr]).where_([where_expr]).returning(["id"]);
        let (sql, _args) = ub.build_with_flavor(Flavor::PostgreSQL, &[]);
        assert_eq!(
            sql,
            "UPDATE users SET level = $1 WHERE id = $2 RETURNING id"
        );
    }

    #[test]
    fn delete_basic_where_limit() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let mut db = DeleteBuilder::new();
        db.delete_from(["users"]);
        let where_expr = db.equal("id", 1234_i64);
        db.where_([where_expr]).limit(1);
        let (sql, args) = db.build();
        assert_eq!(sql, "DELETE FROM users WHERE id = ? LIMIT ?");
        assert_eq!(args.len(), 2);
    }

    #[test]
    fn update_assignments_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        // incr/decr emit no args
        let mut ub = UpdateBuilder::new();
        ub.set([ub.incr("f")]);
        let (sql, args) = ub.build_with_flavor(Flavor::PostgreSQL, &[]);
        assert_eq!(sql, "SET f = f + 1");
        assert_eq!(args.len(), 0);

        let mut ub = UpdateBuilder::new();
        ub.set([ub.decr("f")]);
        let (sql, args) = ub.build_with_flavor(Flavor::PostgreSQL, &[]);
        assert_eq!(sql, "SET f = f - 1");
        assert_eq!(args.len(), 0);

        // add/sub/mul/div emit one arg
        let mut ub = UpdateBuilder::new();
        let expr = ub.add("f", 123_i64);
        ub.set([expr]);
        let (sql, args) = ub.build_with_flavor(Flavor::PostgreSQL, &[]);
        assert_eq!(sql, "SET f = f + $1");
        assert_eq!(args.len(), 1);
    }

    #[test]
    fn update_returning_matrix_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let mut ub = UpdateBuilder::new();
        ub.update(["user"]);
        let set_expr = ub.assign("name", "Huan Du");
        let where_expr = ub.equal("id", 123_i64);
        ub.set([set_expr])
            .where_([where_expr])
            .returning(["id", "updated_at"]);

        assert_eq!(
            ub.build_with_flavor(Flavor::MySQL, &[]).0,
            "UPDATE user SET name = ? WHERE id = ?"
        );
        assert_eq!(
            ub.build_with_flavor(Flavor::PostgreSQL, &[]).0,
            "UPDATE user SET name = $1 WHERE id = $2 RETURNING id, updated_at"
        );
        assert_eq!(
            ub.build_with_flavor(Flavor::SQLite, &[]).0,
            "UPDATE user SET name = ? WHERE id = ? RETURNING id, updated_at"
        );
        assert_eq!(
            ub.build_with_flavor(Flavor::SQLServer, &[]).0,
            "UPDATE user SET name = @p1 OUTPUT INSERTED.id, INSERTED.updated_at WHERE id = @p2"
        );
    }

    #[test]
    fn delete_returning_matrix_like_go() {
        let _g = set_default_flavor_scoped(Flavor::MySQL);
        let mut db = DeleteBuilder::new();
        db.delete_from(["user"]);
        let where_expr = db.equal("id", 123_i64);
        db.where_([where_expr]).returning(["id", "deleted_at"]);

        assert_eq!(
            db.build_with_flavor(Flavor::MySQL, &[]).0,
            "DELETE FROM user WHERE id = ?"
        );
        assert_eq!(
            db.build_with_flavor(Flavor::PostgreSQL, &[]).0,
            "DELETE FROM user WHERE id = $1 RETURNING id, deleted_at"
        );
        assert_eq!(
            db.build_with_flavor(Flavor::SQLite, &[]).0,
            "DELETE FROM user WHERE id = ? RETURNING id, deleted_at"
        );
        assert_eq!(
            db.build_with_flavor(Flavor::SQLServer, &[]).0,
            "DELETE FROM user OUTPUT DELETED.id, DELETED.deleted_at WHERE id = @p1"
        );
    }
}
