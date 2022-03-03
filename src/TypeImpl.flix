namespace TypeImpl {
    use Interval.Interval;
    use Interval.Interval.Interval;
    use Interval.Bound.NegInf;
    use Interval.Bound.PosInf;
    use Interval.Bound.Number;
    use Interval.IntervalResult.Cancelled;
    use Interval.IntervalResult.Single;
    use Interval.IntervalResult.Sorted;

    pub opaque type TypeImpl[a] = {
        tags :: TagDnf,
        ints :: IntDnf,
        prods :: LazyBdd.LazyBdd[a],
        arrows :: LazyBdd.LazyBdd[a]
    }

    enum TagDnf {
        // a union of tags
        case AllOf(Set[String])
        // a negation of a union of tags
        case AllBut(Set[String])
    }

    opaque type IntDnf = List[Interval]

    pub def unionTags(t1: TagDnf, t2: TagDnf): TagDnf = match (t1, t2) {
        // {a, b} \/ {b, c} = {a, b, c}
        case (AllOf(set1), AllOf(set2)) => AllOf(set1 `Set.union` set2)
        // {a, b} \/ ~{b, c} = ~({b, c} \ {a, b}) = ~{c}
        case (AllOf(pos), AllBut(neg)) => AllBut(neg `Set.difference` pos)
        case (AllBut(neg), AllOf(pos)) => AllBut(neg `Set.difference` pos)
        // ~{a, b} \/ ~{b, c} = ~({a, b} /\ {b, c}) = ~{b}
        case (AllBut(set1), AllBut(set2)) => AllBut(set1 `Set.intersection` set2)
    }

    pub def interTags(t1: TagDnf, t2: TagDnf): TagDnf = match (t1, t2) {
        // {a, b} /\ {b, c} = {b}
        case (AllOf(set1), AllOf(set2)) => AllOf(set1 `Set.intersection` set2)
        // {a, b} /\ ~{b, c} = {a, b} \ {b, c} = {a}
        case (AllOf(pos), AllBut(neg)) => AllOf(pos `Set.difference` neg)
        case (AllBut(neg), AllOf(pos)) => AllOf(pos `Set.difference` neg)
        // ~{a, b} /\ ~{b, c} = ~({a, b} \/ {b, c}) = ~{a, b, c}
        case (AllBut(set1), AllBut(set2)) => AllBut(set1 `Set.union` set2)
    }

    pub def diffTags(t1: TagDnf, t2: TagDnf): TagDnf = match (t1, t2) {
        // {a, b} \ {b, c} = {a}
        case (AllOf(set1), AllOf(set2)) => AllOf(set1 `Set.difference` set2)
        // {a, b} \ ~{b, c} = {a, b} /\ {b, c} = b
        case (AllOf(pos), AllBut(neg)) => AllBut(pos `Set.intersection` neg)
        // ~{a, b} \ {b, c} = ~({a, b} \/ {b, c}) = ~{a, b, c}
        case (AllBut(neg), AllOf(pos)) => AllBut(neg `Set.union` pos)
        // ~{a, b} \ ~{b, c} = ~{a, b}
        case (AllBut(set1), AllBut(set2)) => AllOf(set2 `Set.difference` set1)
    }

    def merge(l1: List[a], l2: List[a]): List[a] with Order[a] = match (l1, l2) {
        case (Nil, l) => l
        case (l, Nil) => l
        case (hd1 :: tl1, hd2 :: tl2) =>
            if (hd1 < hd2) {
                hd1 :: merge(tl1, hd2 :: tl2)
            } else {
                hd2 :: merge(hd1 :: tl1, tl2)
            }
    }

    pub def unionInts(t1: IntDnf, t2: IntDnf): IntDnf = {
        let IntDnf(is1) = t1;
        let IntDnf(is2) = t2;
        let is = merge(is1, is2);

        // Traverse the intervals in order, merging where possible.
        // Builds the result in reverse for efficiency.
        let unioned = List.foldLeft(
            (acc, i) -> match acc {
                // Nothing on the stack. Push the interval on.
                case Nil => i :: Nil
                // Something on the stack. Merge it with the interval.
                case hd :: tl =>
                    match Interval.union(hd, i) {
                        case Cancelled => unreachable!()
                        // Merge successful. Push it onto the stack.
                        case Single(merged) => merged :: tl
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
        let is = merge(is1, is2);

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
                        case Cancelled => tl
                        // Merge successful. Push it onto the stack.
                        case Single(merged) => merged :: tl
                        case Sorted(_, _) => unreachable!()
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

}