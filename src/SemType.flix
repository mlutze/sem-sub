lawless class SemType[a] {
    // Bottom type
    pub def empty(): a

    // Constructors
    pub def tag(name: String): a
    pub def range(lo: Interval.Bound, hi: Interval.Bound): a
    pub def arrow(t1: a, t2: a): a
    pub def prod(t1: a, t2: a): a

    // Connectives
    pub def tand(t1: a, t2: a): a
    pub def tnot(t: a): a

    // Helpers
    pub def tor(t1: a, t2: a): a = 
        use SemType.tnot;
        tnot(tnot(t1) `SemType.tand` tnot(t2))
    pub def any(): a = SemType.tnot(SemType.empty())
    pub def anyProd(): a = SemType.prod(SemType.any(), SemType.any())
    pub def anyArrow(): a = SemType.arrow(SemType.any(), SemType.any())
    pub def anyInt(): a = 
        use Interval.Bound.NegInf;
        use Interval.Bound.PosInf;
        SemType.range(NegInf, PosInf)
    pub def anyTag(): a =
        use SemType.tor;
        SemType.tnot(
            SemType.anyProd() `tor`
            SemType.anyArrow() `tor`
            SemType.anyInt()
        )

}

namespace SemType {
    pub def --(lo: Interval.Bound, hi: Interval.Bound): a with SemType[a] = SemType.range(lo, hi)
    pub def -->(t1: a, t2: a): a with SemType[a] = SemType.arrow(t1, t2)
}