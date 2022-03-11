namespace Indexed {
    pub opaque type Indexed[i, a] = (i, a)

    instance Eq[Indexed[i, a]] with Eq[i] {
    
        pub def eq(x: Indexed[i, a], y: Indexed[i, a]): Bool =
            let Indexed(iX, _) = x;
            let Indexed(iY, _) = y;
            iX == iY
    }

    instance Order[Indexed[i, a]] with Order[i] {
    
        pub def compare(x: Indexed[i, a], y: Indexed[i, a]): Comparison =
            let Indexed(iX, _) = x;
            let Indexed(iY, _) = y;
            Order.compare(iX, iY)
    
    }
    
    
}