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

    pub opaque type TypeImpl[a] = {
        tags :: TagDnf,
        ints :: IntDnf,
        prods :: LazyBdd.LazyBdd[a],
        arrows :: LazyBdd.LazyBdd[a]
    }

}