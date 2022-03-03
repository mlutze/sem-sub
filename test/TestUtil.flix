namespace TestUtil {
    use Util.sortPair;

    @test
    pub def testSortPair01(): Bool = sortPair(1, 2) == (1, 2)

    @test
    pub def testSortPair02(): Bool = sortPair(2, 1) == (1, 2)
}