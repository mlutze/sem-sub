namespace Util {
    pub def sortPair(x: a, y: a): (a, a) with Order[a] = {
        (Order.min(x, y), Order.max(x, y))
    }

    pub def merge(l1: List[a], l2: List[a]): List[a] with Order[a] = match (l1, l2) {
        case (Nil, l) => l
        case (l, Nil) => l
        case (hd1 :: tl1, hd2 :: tl2) =>
            if (hd1 < hd2) {
                hd1 :: merge(tl1, hd2 :: tl2)
            } else {
                hd2 :: merge(hd1 :: tl1, tl2)
            }
    }

}