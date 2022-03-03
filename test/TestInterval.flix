namespace TestInterval {
    use Interval.union;
    use Interval.inter;

    use Interval.Bound;
    use Interval.Bound.NegInf;
    use Interval.Bound.Number;
    use Interval.Bound.PosInf;

    use Interval.Interval;
    use Interval.Interval.Interval;

    use Interval.IntervalResult;
    use Interval.IntervalResult.Cancelled;
    use Interval.IntervalResult.Single;
    use Interval.IntervalResult.Sorted;

    @test
    def testUnion01(): Bool =
        union(
            Interval(Number(0ii), Number(4ii)),
            Interval(Number(2ii), Number(6ii))
        )
        ==
        Single(Interval(Number(0ii), Number(6ii)))

    @test
    def testUnion02(): Bool =
        union(
            Interval(Number(0ii), Number(6ii)),
            Interval(Number(2ii), Number(4ii))
        )
        ==
        Single(Interval(Number(0ii), Number(6ii)))
    
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
        Single(Interval(Number(0ii), Number(5ii)))
}