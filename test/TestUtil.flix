namespace TestUtil {
    use Util.sortPair;
    use Util.merge;

    @test
    pub def testSortPair01(): Bool = sortPair(1, 2) == (1, 2)

    @test
    pub def testSortPair02(): Bool = sortPair(2, 1) == (1, 2)

    @test
    pub def testMerge01(): Bool =
        merge(1 :: 3 :: Nil, 2 :: 4 :: Nil) == 1 :: 2 :: 3 :: 4 :: Nil

    @test
    pub def testMerge02(): Bool =
        merge(2 :: 4 :: Nil, 1 :: 3 :: Nil) == 1 :: 2 :: 3 :: 4 :: Nil
}