namespace Interval {
    use Util.sortPair;

    /// An interval bound.
    pub enum Bound with Eq, Order {
        case NegInf
        case Number(BigInt)
        case PosInf
    }

    /// A (possibly unbounded) integer interval.
    pub opaque type Interval with Eq, Order = (Bound, Bound)

    /// The result of taking the intersection of two intervals.
    pub enum InterResult with Eq {
        case Empty
        case Remaining(Interval)
    }

    /// The result of taking the union of two intervals.
    pub enum UnionResult with Eq {
        case Merged(Interval)
        case Sorted(Interval, Interval)
    }

    /// Returns the union of two intervals.
    pub def union(i1: Interval, i2: Interval): UnionResult = {
        let (i1Sorted, i2Sorted) = sortPair(i1, i2);
        let Interval(lo1, hi1) = i1Sorted;
        let Interval(lo2, hi2) = i2Sorted;
        // We now know that lo1 <= lo2

        // |---|
        //   |---|
        //   or
        // |---|
        //      |---|
        if (succ(hi1) >= lo2) { // we have an intersection or adjacency
            let lo = lo1;
            let hi = Order.max(hi1, hi2);
            Merged(Interval(lo, hi))
        // |---|
        //       |---|
        } else {
            Sorted(i1Sorted, i2Sorted)
        }

    }

    /// Returns the intersection of two intervals.
    pub def inter(i1: Interval, i2: Interval): InterResult = {
        let (i1Sorted, i2Sorted) = sortPair(i1, i2);
        let Interval(_lo1, hi1) = i1Sorted;
        let Interval(lo2, hi2) = i2Sorted;
        // We now know that lo1 <= lo2

        // |---|
        //   |---|
        if (hi1 >= lo2) { // we have an intersection
            let lo = lo2;
            let hi = Order.min(hi1, hi2);
            Remaining(Interval(lo, hi))
        // |---|
        //       |---|
        } else {
            Empty
        }
    }

    /// Returns the successor of the bound,
    /// or the bound if it is infinite.
    def succ(b: Bound): Bound = match b {
        case NegInf => NegInf
        case Number(i) => Number(i + 1ii)
        case PosInf => PosInf
    }

}