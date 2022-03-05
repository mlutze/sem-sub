namespace IntDnf {
    use Interval.Interval;
    use Interval.Interval.Interval;

    use Interval.Bound;
    use Interval.Bound.NegInf;
    use Interval.Bound.Number;
    use Interval.Bound.PosInf;

    use Interval.InterResult.Empty;
    use Interval.InterResult.Remaining;
    use Interval.UnionResult.Merged;
    use Interval.UnionResult.Sorted;

    pub opaque type IntDnf = List[Interval]

    pub def unionInts(t1: IntDnf, t2: IntDnf): IntDnf = {
        let IntDnf(is1) = t1;
        let IntDnf(is2) = t2;
        let is = Util.merge(is1, is2);

        // Traverse the intervals in order, merging where possible.
        // Builds the result in reverse for efficiency.
        let unioned = List.foldLeft(
            (acc, i) -> match acc {
                // Nothing on the stack. Push the interval on.
                case Nil => i :: Nil
                // Something on the stack. Merge it with the interval.
                case hd :: tl =>
                    match Interval.union(hd, i) {
                        // Merge successful. Push it onto the stack.
                        case Merged(merged) => merged :: tl
                        // Merge failed. Push the parts separately onto the stack.
                        case Sorted(i1, i2) => i2 :: i1 :: tl
                    }

            }
        )(Nil, is);
        IntDnf(List.reverse(unioned))
    }

    pub def interInts(t1: IntDnf, t2: IntDnf): IntDnf = {
        let IntDnf(is1) = t1;
        let IntDnf(is2) = t2;
        let is = Util.merge(is1, is2);

        // Traverse the intervals in order, merging where possible.
        // Builds the result in reverse for efficiency.
        let intered = List.foldLeft(
            (acc, i) -> match acc {
                // Nothing on the stack. Push the interval on.
                case Nil => i :: Nil
                // Something on the stack. Merge it with the interval.
                case hd :: tl =>
                    match Interval.inter(hd, i) {
                        // Disjoint intervals. Remove it from the stack.
                        case Empty => tl
                        // Merge successful. Push it onto the stack.
                        case Remaining(merged) => merged :: tl
                    }

            }
        )(Nil, is);
        IntDnf(List.reverse(intered))
    }

    def negInts(t1: IntDnf): IntDnf = {
        let IntDnf(is) = t1;
        match is {
            case Nil => IntDnf(Interval(NegInf, PosInf) :: Nil)
            case hd :: tl =>
                let pairs = List.zip(is, tl);
                let negIs = pairs |> List.map(
                    pair -> match pair {
                        case (Interval(_, Number(hi1)), Interval(Number(lo2), _)) => Interval(Number(hi1 + 1ii), Number(lo2 - 1ii))
                        case (_, _) => bug!("invalid interval")
                    }
                );
                let negIsHd = match hd {
                    case Interval(NegInf, _) => Nil
                    case Interval(Number(lo), _) => Interval(NegInf, Number(lo - 1ii)) :: Nil
                    case _ => bug!("invalid interval")
                };
                let negIsLast = match (List.last(tl) |> Option.getWithDefault(hd)) {
                    case Interval(_, PosInf) => Nil
                    case Interval(_, Number(hi)) => Interval(Number(hi + 1ii), PosInf) :: Nil
                    case _ => bug!("invalid interval")
                };
                IntDnf(negIsHd ++ negIs ++ negIsLast)
        }

    }

    pub def diffInts(t1: IntDnf, t2: IntDnf): IntDnf = {
        t1 `interInts` negInts(t2)
    }

    instance SetLike[IntDnf] {
    
        pub def union(x: IntDnf, y: IntDnf): IntDnf = unionInts(x, y)
    
        pub def empty(): IntDnf = IntDnf(Nil)
    
        pub def neg(x: IntDnf): IntDnf = negInts(x)

        override pub def inter(x: IntDnf, y: IntDnf): IntDnf = interInts(x, y)

        override pub def any(): IntDnf = IntDnf(Interval(NegInf, PosInf) :: Nil)
    
    }
    
}