namespace TestUtil {
    use Util.inits;
    use Util.merge;
    use Util.sortPair;
    use Util.sublists;
    use Util.tails;

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

    @test
    pub def testTails01(): Bool =
        tails(Nil: List[Int32]) == Nil :: Nil
    
    @test
    pub def testTails02(): Bool =
        tails(1 :: 2 :: 3 :: Nil) == 
            (1 :: 2 :: 3 :: Nil) ::
            (2 :: 3 :: Nil) ::
            (3 :: Nil) ::
            (Nil) ::
            Nil

    @test
    pub def testInits01(): Bool =
        inits(Nil: List[Int32]) == Nil :: Nil
        
    @test
    pub def testInits02(): Bool =
        inits(1 :: 2 :: 3 :: Nil) == 
            (1 :: 2 :: 3 :: Nil) ::
            (1 :: 2 :: Nil) ::
            (1 :: Nil) ::
            (Nil) ::
            Nil

    @test
    pub def testSublists01(): Bool =
        sublists(Nil: List[Int32]) == Nil :: Nil
    
    @test
    pub def testSublists02(): Bool =
        sublists(1 :: 2 :: 3 :: Nil) ==
            (1 :: 2 :: 3 :: Nil) ::
            (2 :: 3 :: Nil) ::
            (3 :: Nil) ::
            (1 :: 2 :: Nil) ::
            (2 :: Nil) ::
            (1 :: Nil) ::
            (Nil) ::
            Nil
}