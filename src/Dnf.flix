namespace Dnf {
    pub opaque type Dnf[a] = List[List[Literal[a]]]
    pub enum Literal[a] {
        case Pos(a)
        case Neg(a)
    }

    /// Decomposes the given disjunction into two sets, P and N,
    /// where P contains all the positive atoms,
    /// and N contains all the negative atoms.
    pub def decompose(disj: List[Literal[a]]): (List[a], List[a]) = {
        def isPos(l: Literal[a]) = match l {
            case Pos(_) => true
            case Neg(_) => false
        };
        def toAtom(l: Literal[a]) = match l {
            case Pos(x) => x
            case Neg(x) => x
        };
        let (pos, neg) = List.partition(isPos, disj);
        (List.map(toAtom, pos), List.map(toAtom, neg))
    }
}