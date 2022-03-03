// The main entry point.
def main(_args: Array[String]): Int32 & Impure =
    println("Hello World!");
    0 // exit code

enum Atom with Eq {
    case Tag(String)
    case Range(Option[Int32], Option[Int32])
    case Prod(Ty, Ty)
    case Arrow(Ty, Ty)
}

enum Ty with Eq {
    case At(Atom)
    case Var(String)
    case Rec(String, Atom)
    case Or(Ty, Ty)
    case And(Ty, Ty)
    case Not(Ty)
    case Any
}

enum Lit with Eq {
    case Just(Atom)
    case LNot(Atom)
}