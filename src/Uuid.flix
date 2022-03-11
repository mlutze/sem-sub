namespace Uuid {
    type alias JUuid = ##java.util.UUID
    type alias Object = ##java.lang.Object

    pub opaque type Uuid = JUuid

    pub def random(): Uuid & Impure =
        import static java.util.UUID.randomUUID(): JUuid & Impure;
        Uuid(randomUUID())

    instance Eq[Uuid] {
    
        pub def eq(x: Uuid, y: Uuid): Bool =
            import java.util.UUID.equals(Object): Bool & Pure;
            let Uuid(idX) = x;
            let Uuid(idY) = y;
            equals(idX, idY as Object)
    
    }

    instance Order[Uuid] {
    
        pub def compare(x: Uuid, y: Uuid): Comparison =
            import java.util.UUID.compareTo(JUuid): Int32 & Pure;
            let Uuid(idX) = x;
            let Uuid(idY) = y;
            Comparison.fromInt32(compareTo(idX, idY))
    
    }
    
    
}