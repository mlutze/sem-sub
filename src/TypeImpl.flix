namespace TypeImpl {
    use Interval.Interval;
    use Interval.Interval.Interval;
    use Interval.Bound.NegInf;
    use Interval.Bound.PosInf;
    use Interval.Bound.Number;
    use Interval.IntervalResult.Cancelled;
    use Interval.IntervalResult.Single;
    use Interval.IntervalResult.Sorted;

    use Indexed.Indexed;
    use Uuid.Uuid;

    use IntDnf.IntDnf;

    use TagDnf.TagDnf;

    type alias Atom = Indexed[Uuid, TypeImpl]

    pub opaque type TypeImpl = {
        tags :: TagDnf,
        ints :: IntDnf,
        prods :: LazyBdd.LazyBdd[Atom],
        arrows :: LazyBdd.LazyBdd[Atom]
    }

    instance SetLike[TypeImpl] {
    
        pub def union(x: TypeImpl, y: TypeImpl): TypeImpl =
            let TypeImpl(rx) = x;
            let TypeImpl(ry) = y;
            let rz = {
                tags = SetLike.union(rx.tags, ry.tags),
                ints = SetLike.union(rx.ints, ry.ints),
                prods = SetLike.union(rx.prods, ry.prods),
                arrows = SetLike.union(rx.arrows, ry.arrows)
            };
            TypeImpl(rz)
    
        pub def empty(): TypeImpl =
            let tpes = {
                tags = SetLike.empty(),
                ints = SetLike.empty(),
                prods = SetLike.empty(),
                arrows = SetLike.empty()
            };
            TypeImpl(tpes)
    
        pub def isEmpty(x: TypeImpl): Bool = ???
    
        pub def neg(x: TypeImpl): TypeImpl =
            let TypeImpl(tpes0) = x;
            let tpes = {
                tags = SetLike.neg(tpes0.tags),
                ints = SetLike.neg(tpes0.ints),
                prods = SetLike.neg(tpes0.prods),
                arrows = SetLike.neg(tpes0.arrows)
            };
            TypeImpl(tpes)
    }
    

}