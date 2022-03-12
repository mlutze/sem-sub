namespace Dnf {
    pub opaque type Dnf[a] = List[List[Literal[a]]]
    pub enum Literal[a] {
        case Pos(a)
        case Neg(a)
    }
}