namespace LazyBdd {
    pub enum LazyBdd[a] with Eq {
        case False
        case ThenElse(a, LazyBdd[a], LazyBdd[a], LazyBdd[a])
        case True
    }

    pub def union(bdd1: LazyBdd[a], bdd2: LazyBdd[a]): LazyBdd[a] with Order[a] = match (bdd1, bdd2) {
        case (True, _) => True
        case (_, True) => True
        case (False, bdd) => bdd
        case (bdd, False) => bdd
        case (ThenElse(a1, then1, union1, else1), ThenElse(a2, then2, union2, else2)) =>
            if (a1 == a2) {
                ThenElse(a1, union(then1, then2), union(union1, union2), union(else1, else2))
            } else if (a1 < a2) {
                ThenElse(a1, then1, union(union1, bdd2), else2)
            } else /* a1 > a2 */ {
                ThenElse(a2, then2, union(bdd1, union2), else2)
            }
    }

    pub def inter(bdd1: LazyBdd[a], bdd2: LazyBdd[a]): LazyBdd[a] with Order[a] = match (bdd1, bdd2) {
        case (True, bdd) => bdd
        case (bdd, True) => bdd
        case (False, _) => False
        case (_, False) => False
        case (ThenElse(a1, then1, union1, else1), ThenElse(a2, then2, union2, else2)) =>
            if (a1 == a2) {
                ThenElse(
                    a1,
                    union(then1, union1) `inter` union(then2, union2),
                    False,
                    union(else1, union1) `inter` union(else2, union2)
                )
            } else if (a1 < a2) {
                ThenElse(
                    a1,
                    then1 `inter` bdd2,
                    union1 `inter` bdd2,
                    else1 `inter` bdd2
                )
            } else /* a1 > a2 */ {
                ThenElse(
                    a2,
                    bdd1 `inter` then2,
                    bdd1 `inter` union2,
                    bdd1 `inter` else2
                )
            }
    }

    pub def diff(bdd1: LazyBdd[a], bdd2: LazyBdd[a]): LazyBdd[a] with Order[a] = match (bdd1, bdd2) {
        case (_, True) => False
        case (False, _) => False
        case (bdd, False) => bdd
        // MATT not sure if True \ union is right
        case (True, ThenElse(a2, then2, union2, else2)) => ThenElse(a2, diff(True, then2), diff(True, union2), diff(True, else2))
        case (ThenElse(a1, then1, union1, else1), ThenElse(a2, then2, union2, else2)) =>
            if (a1 == a2) {
                ThenElse(
                    a1,
                    union(then1, union1) `diff` union(then2, union2),
                    False,
                    union(else1, union1) `diff` union(else2, union2)
                )
            } else if (a1 < a2) {
                ThenElse(
                    a1,
                    union(then1, union1) `diff` bdd2,
                    False,
                    union(else1, union1) `diff` bdd2
                )
            } else /* (a1 > a2) */ {
                ThenElse(
                    a1,
                    bdd1 `diff` union(then2, union2),
                    False,
                    bdd1 `diff` union(else2, union2)
                )
            }
    }

    // simplifications:
    // ThenElse(_, True, _) => True
    // ThenElse(_, then1, _, else1) if then1 == else1 => B \/ C
}