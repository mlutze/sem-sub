namespace TypeImpl {
    use Interval.Interval;
    use Interval.Interval.Interval;
    use Interval.Bound.NegInf;
    use Interval.Bound.PosInf;
    use Interval.Bound.Number;
    use Interval.IntervalResult.Cancelled;
    use Interval.IntervalResult.Single;
    use Interval.IntervalResult.Sorted;

    use IntDnf.IntDnf;

    use TagDnf.TagDnf;

    /// pub enum Atom {
    ///     case Interval(IntDnf)
    ///     case Prod(TypeImpl, TypeImpl)
    ///     case Arrow(TypeImpl, TypeImpl)
    /// }

    pub opaque type TypeImpl = {
        tags :: TagDnf,
        ints :: IntDnf,
        prods :: LazyBdd.LazyBdd[Atom],
        arrows :: LazyBdd.LazyBdd[Atom]
    }

    /// instance SetLike[TypeImpl] {
    
    ///     pub def union(x: TypeImpl, y: TypeImpl): TypeImpl =
    ///         let TypeImpl(rx) = x;
    ///         let TypeImpl(ry) = y;
    ///         let rz = {
    ///             tags = SetLike.union(rx.tags, ry.tags),
    ///             ints = SetLike.union(rx.ints, ry.ints),
    ///             prods = SetLike.union(rx.prods, ry.prods),
    ///             arrows = SetLike.union(rx.arrows, ry.arrows)
    ///         };
    ///         TypeImpl(rz)
    
    ///     pub def empty(): TypeImpl = ???
    
    ///     pub def isEmpty(x: TypeImpl): Bool = ???
    
    ///     pub def neg(x: TypeImpl): TypeImpl = ???
    
    /// }
    

}