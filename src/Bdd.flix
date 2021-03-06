namespace Bdd {

    use Dnf.Conjunction;
    use Dnf.Disjunction;
    use Dnf.Dnf.Dnf;
    use Dnf.Dnf;
    use Dnf.Literal.Neg;
    use Dnf.Literal.Pos;
    use Dnf.Literal;

    pub enum Bdd[a] {
        case False
        case ThenElse(a, Bdd[a], Bdd[a])
        case True
    }

    pub def union(bdd1: Bdd[a], bdd2: Bdd[a]): Bdd[a] with Order[a] = match (bdd1, bdd2) {
        case (True, _) => True
        case (_, True) => True
        case (False, bdd) => bdd
        case (bdd, False) => bdd
        case (ThenElse(a1, then1, else1), ThenElse(a2, then2, else2)) =>
            if (a1 == a2) {
                ThenElse(a1, union(then1, then2), union(else1, else2))
            } else if (a1 < a2) {
                ThenElse(a1, union(then1, bdd2), union(else1, bdd2))
            } else /* a1 > a2 */ {
                ThenElse(a2, union(bdd1, then2), union(bdd1, else2))
            }
    }

    pub def inter(bdd1: Bdd[a], bdd2: Bdd[a]): Bdd[a] with Order[a] = match (bdd1, bdd2) {
        case (True, bdd) => bdd
        case (bdd, True) => bdd
        case (False, _) => False
        case (_, False) => False
        case (ThenElse(a1, then1, else1), ThenElse(a2, then2, else2)) =>
            if (a1 == a2) {
                ThenElse(a1, inter(then1, then2), inter(else1, else2))
            } else if (a1 < a2) {
                ThenElse(a1, inter(then1, bdd2), inter(else1, bdd2))
            } else /* a1 > a2 */ {
                ThenElse(a2, inter(bdd1, then2), inter(bdd1, else2))
            }
    }

    pub def diff(bdd1: Bdd[a], bdd2: Bdd[a]): Bdd[a] with Order[a] = match (bdd1, bdd2) {
        case (_, True) => False
        case (False, _) => False
        case (bdd, False) => bdd
        case (True, ThenElse(a2, then2, else2)) => ThenElse(a2, diff(True, then2), diff(True, else2))
        case (ThenElse(a1, then1, else1), ThenElse(a2, then2, else2)) =>
            if (a1 == a2) {
                ThenElse(a1, diff(then1, then2), diff(else1, else2))
            } else if (a1 < a2) {
                ThenElse(a1, diff(then1, bdd2), diff(else1, bdd2))
            } else /* a1 > a2 */ {
                ThenElse(a2, diff(bdd1, then2), diff(bdd1, else2))
            }
    }


    /// Converts the BDD into disjunctive normal form,
    pub def toDnf(bdd0: Bdd[a]): Dnf[a] = {
        def loop(bdd, path) = match bdd {
            case True => (path) :: Nil
            case False => Nil
            case ThenElse(a, then, else_) =>
                loop(then, Pos(a) :: path) :::
                loop(else_, Neg(a) :: path)
        };
        Dnf(loop(bdd0, Nil)): & Pure
    }
}