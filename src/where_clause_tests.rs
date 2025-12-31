#[cfg(test)]
mod tests {
    use crate::cond::Cond;
    use crate::delete::DeleteBuilder;
    use crate::modifiers::{Arg, Builder, rc_builder};
    use crate::select::SelectBuilder;
    use crate::update::UpdateBuilder;
    use crate::where_clause::{WhereClause, copy_where_clause};
    use crate::{delete_from_tables, from_tables, select_cols, update_tables, where_exprs};
    use pretty_assertions::assert_eq;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn where_clause_shared_instances_like_go() {
        let mut sb = SelectBuilder::new();
        select_cols!(sb, "*");
        from_tables!(sb, "t");
        let mut ub = UpdateBuilder::new();
        update_tables!(ub, "t").set(["foo = 1"]);
        let mut db = DeleteBuilder::new();
        delete_from_tables!(db, "t");

        let where_clause = WhereClause::new();
        sb.set_where_clause(Some(where_clause.clone()));
        ub.set_where_clause(Some(where_clause.clone()));
        db.set_where_clause(Some(where_clause.clone()));

        sb.where_([sb.equal("id", 123)]);
        assert_eq!(sb.build().0, "SELECT * FROM t WHERE id = ?");
        assert_eq!(ub.build().0, "UPDATE t SET foo = 1 WHERE id = ?");
        assert_eq!(db.build().0, "DELETE FROM t WHERE id = ?");

        // Add more WhereClause (shared)
        let cond = Cond::new();
        let more_wc = WhereClause::new();
        more_wc
            .borrow_mut()
            .add_where_expr(cond.args.clone(), [cond.greater_equal_than("credit", 100)]);

        sb.add_where_clause_ref(&more_wc);
        assert_eq!(sb.build().0, "SELECT * FROM t WHERE id = ? AND credit >= ?");
        assert_eq!(
            ub.build().0,
            "UPDATE t SET foo = 1 WHERE id = ? AND credit >= ?"
        );
        assert_eq!(db.build().0, "DELETE FROM t WHERE id = ? AND credit >= ?");

        // Copied WhereClause is independent
        ub.set_where_clause(Some(copy_where_clause(&where_clause)));
        ub.where_([ub.greater_equal_than("level", 10)]);
        db.where_([db.in_("status", [1, 2])]);
        assert_eq!(
            sb.build().0,
            "SELECT * FROM t WHERE id = ? AND credit >= ? AND status IN (?, ?)"
        );
        assert_eq!(
            ub.build().0,
            "UPDATE t SET foo = 1 WHERE id = ? AND credit >= ? AND level >= ?"
        );
        assert_eq!(
            db.build().0,
            "DELETE FROM t WHERE id = ? AND credit >= ? AND status IN (?, ?)"
        );

        // Clear WhereClause and add new where clause and expressions.
        db.clear_where_clause();
        db.add_where_clause_ref(&ub.where_clause().unwrap());
        db.add_where_expr(db.args.clone(), [db.equal("deleted", 0)]);
        assert_eq!(
            sb.build().0,
            "SELECT * FROM t WHERE id = ? AND credit >= ? AND status IN (?, ?)"
        );
        assert_eq!(
            ub.build().0,
            "UPDATE t SET foo = 1 WHERE id = ? AND credit >= ? AND level >= ?"
        );
        assert_eq!(
            db.build().0,
            "DELETE FROM t WHERE id = ? AND credit >= ? AND level >= ? AND deleted = ?"
        );

        // Nested WhereClause + late-binding builder: pass sb as subquery first, then keep mutating sb.
        let sb_shared = Rc::new(RefCell::new(sb));
        let sb_arg: Arg = (Box::new(rc_builder(sb_shared.clone())) as Box<dyn Builder>).into();
        ub.where_([ub.not_in("id", [sb_arg])]);

        let expr = {
            let sb_ref = sb_shared.borrow();
            sb_ref.not_equal("flag", "normal")
        };
        sb_shared.borrow_mut().where_([expr]);

        assert_eq!(
            ub.build().0,
            "UPDATE t SET foo = 1 WHERE id = ? AND credit >= ? AND level >= ? AND id NOT IN (SELECT * FROM t WHERE id = ? AND credit >= ? AND status IN (?, ?) AND flag <> ?)"
        );
    }

    #[test]
    fn empty_where_expr_like_go() {
        let blank = ["", ""];

        let mut sb = SelectBuilder::new();
        select_cols!(sb, "*");
        from_tables!(sb, "t");
        where_exprs!(sb, blank);

        let mut ub = UpdateBuilder::new();
        update_tables!(ub, "t").set(["foo = 1"]);
        where_exprs!(ub, blank);

        let mut db = DeleteBuilder::new();
        delete_from_tables!(db, "t");
        where_exprs!(db, blank);

        assert_eq!(sb.build().0, "SELECT * FROM t");
        assert_eq!(ub.build().0, "UPDATE t SET foo = 1");
        assert_eq!(db.build().0, "DELETE FROM t");
    }

    #[test]
    fn empty_strings_where_like_go() {
        let empty = ["", "", ""];

        let mut sb = SelectBuilder::new();
        select_cols!(sb, "*");
        from_tables!(sb, "t");
        where_exprs!(sb, empty);

        let mut ub = UpdateBuilder::new();
        update_tables!(ub, "t").set(["foo = 1"]);
        where_exprs!(ub, empty);

        let mut db = DeleteBuilder::new();
        delete_from_tables!(db, "t");
        where_exprs!(db, empty);

        assert_eq!(sb.build().0, "SELECT * FROM t");
        assert_eq!(ub.build().0, "UPDATE t SET foo = 1");
        assert_eq!(db.build().0, "DELETE FROM t");
    }

    #[test]
    fn empty_add_where_expr_like_go() {
        let mut sb = SelectBuilder::new();
        select_cols!(sb, "*");
        from_tables!(sb, "t");

        let mut ub = UpdateBuilder::new();
        update_tables!(ub, "t").set(["foo = 1"]);

        let mut db = DeleteBuilder::new();
        delete_from_tables!(db, "t");

        let cond = Cond::new();
        let wc = WhereClause::new();
        wc.borrow_mut()
            .add_where_expr(cond.args.clone(), Vec::<String>::new());

        sb.add_where_clause_ref(&wc);
        ub.add_where_clause_ref(&wc);
        db.add_where_clause_ref(&wc);

        assert_eq!(sb.build().0, "SELECT * FROM t ");
        assert_eq!(ub.build().0, "UPDATE t SET foo = 1 ");
        assert_eq!(db.build().0, "DELETE FROM t ");
    }

    #[test]
    fn empty_strings_where_add_where_expr_like_go() {
        let mut sb = SelectBuilder::new();
        select_cols!(sb, "*");
        from_tables!(sb, "t");

        let mut ub = UpdateBuilder::new();
        update_tables!(ub, "t").set(["foo = 1"]);

        let mut db = DeleteBuilder::new();
        delete_from_tables!(db, "t");

        let cond = Cond::new();
        let wc = WhereClause::new();
        wc.borrow_mut()
            .add_where_expr(cond.args.clone(), ["", "", ""]);

        sb.add_where_clause_ref(&wc);
        ub.add_where_clause_ref(&wc);
        db.add_where_clause_ref(&wc);

        assert_eq!(sb.build().0, "SELECT * FROM t ");
        assert_eq!(ub.build().0, "UPDATE t SET foo = 1 ");
        assert_eq!(db.build().0, "DELETE FROM t ");
    }

    #[test]
    fn where_clause_get_flavor_like_go() {
        let wc = WhereClause::new();
        wc.borrow_mut()
            .set_flavor(crate::flavor::Flavor::PostgreSQL);
        assert_eq!(wc.borrow().flavor(), crate::flavor::Flavor::PostgreSQL);
    }

    #[test]
    fn where_clause_copy_get_flavor_like_go() {
        let wc = WhereClause::new();
        wc.borrow_mut()
            .set_flavor(crate::flavor::Flavor::PostgreSQL);

        let wc_copy = copy_where_clause(&wc);
        assert_eq!(wc_copy.borrow().flavor(), crate::flavor::Flavor::PostgreSQL);
    }
}
