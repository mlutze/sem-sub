lawless class SetLike[a] {
    // Base operations
    pub def union(x: a, y: a): a
    pub def neg(x: a): a
    pub def empty(): a

    // Derived operations
    pub def any(): a = SetLike.neg(SetLike.empty())
    pub def inter(x: a, y: a): a =
        use SetLike.neg;
        use SetLike.union;
        neg(neg(x) `union` neg(y))
}