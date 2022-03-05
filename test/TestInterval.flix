namespace TestInterval {
    use Interval.union;
    use Interval.inter;

    use Interval.Bound;
    use Interval.Bound.NegInf;
    use Interval.Bound.Number;
    use Interval.Bound.PosInf;

    use Interval.Interval;
    use Interval.Interval.Interval;

    use Interval.InterResult.Empty;
    use Interval.InterResult.Remaining;
    use Interval.UnionResult.Merged;
    use Interval.UnionResult.Sorted;

    @test
    def testUnion01(): Bool =
        union(
            Interval(Number(0ii), Number(4ii)),
            Interval(Number(2ii), Number(6ii))
        )
        ==
        Merged(Interval(Number(0ii), Number(6ii)))

    @test
    def testUnion02(): Bool =
        union(
            Interval(Number(0ii), Number(6ii)),
            Interval(Number(2ii), Number(4ii))
        )
        ==
        Merged(Interval(Number(0ii), Number(6ii)))
    
    @test
    def testUnion03(): Bool =
        union(
            Interval(Number(0ii), Number(2ii)),
            Interval(Number(4ii), Number(6ii))
        )
        ==
        Sorted(
            Interval(Number(0ii), Number(2ii)),
            Interval(Number(4ii), Number(6ii))
        )

    @test
    def testUnion04(): Bool =
        union(
            Interval(Number(4ii), Number(6ii)),
            Interval(Number(0ii), Number(2ii))
        )
        ==
        Sorted(
            Interval(Number(0ii), Number(2ii)),
            Interval(Number(4ii), Number(6ii))
        )
    
    @test
    def testUnion05(): Bool =
        union(
            Interval(Number(0ii), Number(2ii)),
            Interval(Number(3ii), Number(5ii))
        )
        ==
        Merged(Interval(Number(0ii), Number(5ii)))

    @test
    def testInter01(): Bool =
        inter(
            Interval(Number(0ii), Number(4ii)),
            Interval(Number(2ii), Number(6ii))
        )
        ==
        Remaining(Interval(Number(2ii), Number(4ii)))

    @test
    def testInter02(): Bool =
        inter(
            Interval(Number(0ii), Number(6ii)),
            Interval(Number(2ii), Number(4ii))
        )
        ==
        Remaining(Interval(Number(2ii), Number(4ii)))
    
    @test
    def testInter03(): Bool =
        inter(
            Interval(Number(0ii), Number(2ii)),
            Interval(Number(4ii), Number(6ii))
        )
        ==
        Empty

    @test
    def testInter04(): Bool =
        inter(
            Interval(Number(4ii), Number(6ii)),
            Interval(Number(0ii), Number(2ii))
        )
        ==
        Empty
    
    @test
    def testInter05(): Bool =
        inter(
            Interval(Number(0ii), Number(2ii)),
            Interval(Number(3ii), Number(5ii))
        )
        ==
        Empty
}