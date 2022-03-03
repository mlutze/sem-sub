namespace Util {
    pub def sortPair(x: a, y: a): (a, a) with Order[a] = {
        (Order.min(x, y), Order.max(x, y))
    }
}