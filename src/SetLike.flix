class SetLike[a] {
    // Base operations
    pub def union(x: a, y: a): a
    pub def neg(x: a): a
    pub def empty(): a
    pub def isEmpty(x: a): Bool

    // Derived operations
    pub def any(): a = SetLike.neg(SetLike.empty())
    pub def inter(x: a, y: a): a =
        use SetLike.neg;
        use SetLike.union;
        neg(neg(x) `union` neg(y))
    pub def diff(x: a, y: a): a =
        use SetLike.neg;
        use SetLike.inter;
        x `inter` neg(y)
    pub def subset(x: a, y: a): Bool =
        use SetLike.diff;
        use SetLike.isEmpty;
        isEmpty(x `diff` y)
}