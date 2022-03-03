namespace Interval {
    use Util.sortPair;

    pub enum Bound with Eq, Order {
        case NegInf
        case Number(BigInt)
        case PosInf
    }

    pub opaque type Interval with Eq, Order = (Bound, Bound)

    pub enum IntervalResult with Eq {
        case Cancelled
        case Single(Interval)
        case Sorted(Interval, Interval)
    }


    pub def union(i1: Interval, i2: Interval): IntervalResult = {
        let (i1Sorted, i2Sorted) = sortPair(i1, i2);
        let Interval(lo1, hi1) = i1Sorted;
        let Interval(lo2, hi2) = i2Sorted;
        // We now know that lo1 <= lo2

        // TODO: must handle adjacency case!

        // |---|
        //   |---|
        if (hi1 >= lo2) { // we have an intersection
            let lo = lo1;
            let hi = Order.max(hi1, hi2);
            Single(Interval(lo, hi))
        // |---|
        //       |---|
        } else {
            Sorted(i1Sorted, i2Sorted)
        }

    }

    pub def inter(i1: Interval, i2: Interval): IntervalResult = {
        let (i1Sorted, i2Sorted) = sortPair(i1, i2);
        let Interval(_lo1, hi1) = i1Sorted;
        let Interval(lo2, hi2) = i2Sorted;
        // We now know that lo1 <= lo2

        // |---|
        //   |---|
        if (hi1 >= lo2) { // we have an intersection
            let lo = lo2;
            let hi = Order.min(hi1, hi2);
            Single(Interval(lo, hi))
        // |---|
        //       |---|
        } else {
            Cancelled
        }
    }
}