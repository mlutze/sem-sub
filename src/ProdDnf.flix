namespace ProdDnf {
    use LazyBdd.LazyBdd;
    use Indexed.Indexed;
    use Uuid.Uuid;
    use LazyBdd.LazyBdd.True;
    use LazyBdd.LazyBdd.False;
    use LazyBdd.LazyBdd.ThenElse;

    pub opaque type ProdDnf[a] = LazyBdd[(Indexed[Uuid, a], Indexed[Uuid, a])]

    instance SetLike[ProdDnf[a]] with SetLike[a] {
    
        pub def union(x: ProdDnf[a], y: ProdDnf[a]): ProdDnf[a] =
            let ProdDnf(bddX) = x;
            let ProdDnf(bddY) = y;
            let bddZ = SetLike.union(bddX, bddY);
            ProdDnf(bddZ)
    
        pub def empty(): ProdDnf[a] =
            let bdd = SetLike.empty();
            ProdDnf(bdd)
    
        pub def isEmpty(x: ProdDnf[a]): Bool =
            let ProdDnf(bddX) = x;
            match bddX {
                case True => false
                case False => true
                case ThenElse(atom, then, union, else_) =>
                    ???
            }
    
        pub def neg(x: ProdDnf[a]): ProdDnf[a] =
            let ProdDnf(bddX) = x;
            let bdd = SetLike.neg(bddX);
            ProdDnf(bdd)
    
    }
    

    
}