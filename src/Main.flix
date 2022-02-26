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

type alias Dnf = List[List[Lit]]

def tor(t1: Ty, t2: Ty): Ty = Or(t1, t2)

def tand(t1: Ty, t2: Ty): Ty = And(t1, t2)

def tag(s: String): Ty = At(Tag(s))

def -->(t1: Ty, t2: Ty): Ty = At(Arrow(t1, t2))

def --(t1: Option[Int32], t2: Option[Int32]): Ty = At(Range(t1, t2))

def empty(): Ty = Not(Any)

def anyProd(): Ty = At(Prod(Any, Any))

def anyArrow(): Ty = empty() --> Any

def anyInt(): Ty = None -- None 

def _anyTag(): Ty = Not(anyProd() `tor` anyArrow() `tor` anyInt())


def toNel(l: List[a]): Option[Nel[a]] = match l {
    case Nil => None
    case Cons(hd, tl) => Some(Nel(hd, tl))
}

def andAll(ts: List[Ty]): Ty = match toNel(ts) {
    case None => empty()
    case Some(nel) => nel |> Nel.reduceLeft(tand)
}

def orAll(ts: List[Ty]): Ty = match toNel(ts) {
    case None => Any
    case Some(nel) => nel |> Nel.reduceLeft(tor)
}

def flipLit(l: Lit): Ty = match l {
    case Just(a) => Not(At(a))
    case LNot(a) => At(a)
}

def deepMap(f: a -> b, l: List[List[a]]): List[List[b]] = List.map(List.map(f), l)

def reduceReduce(outer: List[b] -> c, inner: List[a] -> b, l: List[List[a]]): c =
    l
    |> List.map(inner)
    |> outer


/// def isEmpty(t1: Ty): Bool = ???
/// def <<(t1: Ty, t2: Ty): Bool = (t1 `tand` Not(t2)) |> isEmpty

def dnf(t: Ty): Dnf = match t {
    case At(a) => (Just(a) :: Nil) :: Nil
    case Var(_) => ???
    case Rec(_, _) => ???
    case Or(t1, t2) => 
        let d1 = dnf(t1);
        let d2 = dnf(t2);
        d1 ::: d2
    case And(t1, t2) =>
        use List.flatMap;
        let d1 = dnf(t1);
        let d2 = dnf(t2);
        let* conj1 = d1;
        let* conj2 = d2;
        (conj1 ::: conj2) :: Nil
    case Not(t1) =>
        let d1 = dnf(t1);
        let cnf = deepMap(flipLit, d1);
        cnf |> reduceReduce(andAll, orAll) |> dnf
    case Any => ???
}

@test
def testDnf01(): Bool = dnf(tag("a") `tand` tag("b")) == (Just(Tag("a")) :: Just(Tag("b")) :: Nil) :: Nil

@test
def testDnf02(): Bool = dnf(tag("a") `tor` tag("b")) == (Just(Tag("a")) :: Nil) :: (Just(Tag("b")) :: Nil) :: Nil

@test
def testDnf03(): Bool = dnf((tag("a") `tor` tag("b")) `tand` (tag("c") `tor` tag("d"))) ==
    (Just(Tag("a")) :: Just(Tag("c")) :: Nil) ::
    (Just(Tag("a")) :: Just(Tag("d")) :: Nil) ::
    (Just(Tag("b")) :: Just(Tag("c")) :: Nil) ::
    (Just(Tag("b")) :: Just(Tag("d")) :: Nil) ::
    Nil

@test
def testDnf04(): Bool = dnf(Not(tag("a") `tor` tag("b") `tor` tag("c"))) == (LNot(Tag("a")) :: LNot(Tag("b")) :: LNot(Tag("c")) :: Nil) :: Nil