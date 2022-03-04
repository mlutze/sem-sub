namespace TagDnf {
    
    pub enum TagDnf {
        // a union of tags
        case AllOf(Set[String])
        // a negation of a union of tags
        case AllBut(Set[String])
    }

    pub def unionTags(t1: TagDnf, t2: TagDnf): TagDnf = match (t1, t2) {
        // {a, b} \/ {b, c} = {a, b, c}
        case (AllOf(set1), AllOf(set2)) => AllOf(set1 `Set.union` set2)
        // {a, b} \/ ~{b, c} = ~({b, c} \ {a, b}) = ~{c}
        case (AllOf(pos), AllBut(neg)) => AllBut(neg `Set.difference` pos)
        case (AllBut(neg), AllOf(pos)) => AllBut(neg `Set.difference` pos)
        // ~{a, b} \/ ~{b, c} = ~({a, b} /\ {b, c}) = ~{b}
        case (AllBut(set1), AllBut(set2)) => AllBut(set1 `Set.intersection` set2)
    }

    pub def interTags(t1: TagDnf, t2: TagDnf): TagDnf = match (t1, t2) {
        // {a, b} /\ {b, c} = {b}
        case (AllOf(set1), AllOf(set2)) => AllOf(set1 `Set.intersection` set2)
        // {a, b} /\ ~{b, c} = {a, b} \ {b, c} = {a}
        case (AllOf(pos), AllBut(neg)) => AllOf(pos `Set.difference` neg)
        case (AllBut(neg), AllOf(pos)) => AllOf(pos `Set.difference` neg)
        // ~{a, b} /\ ~{b, c} = ~({a, b} \/ {b, c}) = ~{a, b, c}
        case (AllBut(set1), AllBut(set2)) => AllBut(set1 `Set.union` set2)
    }

    pub def diffTags(t1: TagDnf, t2: TagDnf): TagDnf = match (t1, t2) {
        // {a, b} \ {b, c} = {a}
        case (AllOf(set1), AllOf(set2)) => AllOf(set1 `Set.difference` set2)
        // {a, b} \ ~{b, c} = {a, b} /\ {b, c} = b
        case (AllOf(pos), AllBut(neg)) => AllBut(pos `Set.intersection` neg)
        // ~{a, b} \ {b, c} = ~({a, b} \/ {b, c}) = ~{a, b, c}
        case (AllBut(neg), AllOf(pos)) => AllBut(neg `Set.union` pos)
        // ~{a, b} \ ~{b, c} = ~{a, b}
        case (AllBut(set1), AllBut(set2)) => AllOf(set2 `Set.difference` set1)
    }
}