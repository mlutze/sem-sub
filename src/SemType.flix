class SemType[a] with SetLike[a] {
    // Bottom type
    pub def empty(): a

    // Constructors
    pub def tag(name: String): a
    pub def range(lo: Interval.Bound, hi: Interval.Bound): a
    pub def arrow(t1: a, t2: a): a
    pub def prod(t1: a, t2: a): a

    // Helpers
    pub def anyProd(): a = SemType.prod(SetLike.any(), SetLike.any())
    pub def anyArrow(): a = SemType.arrow(SetLike.any(), SetLike.any())
    pub def anyInt(): a = 
        use Interval.Bound.NegInf;
        use Interval.Bound.PosInf;
        SemType.range(NegInf, PosInf)
    pub def anyTag(): a =
        use SetLike.union;
        SetLike.neg(
            SemType.anyProd() `union`
            SemType.anyArrow() `union`
            SemType.anyInt()
        )
}

namespace SemType {
    pub def --(lo: Interval.Bound, hi: Interval.Bound): a with SemType[a] = SemType.range(lo, hi)
    pub def -->(t1: a, t2: a): a with SemType[a] = SemType.arrow(t1, t2)
}