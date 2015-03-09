namespace FRest.Contracts

type ReaderM<'conf,'output> = 
    { compute : 'conf -> 'output }

module Reader =

    // basic operations

    let run conf (r : ReaderM<_,_>) =
        r.compute conf

    let constant c =
        { compute = fun _ -> c }

    // lifting of functions and state

    let lift1 (f : 'conf -> 'a -> 'out)
              : 'a -> ReaderM<'conf, 'out> =
        fun a -> { compute = fun conf -> f conf a }

    let lift2 (f : 'conf -> 'a -> 'b -> 'out)
              : 'a -> 'b -> ReaderM<'conf, 'out> =
        fun a b -> { compute = fun conf -> f conf a b }

    let lift3 (f : 'conf -> 'a -> 'b -> 'c -> 'out)
              : 'a -> 'b -> 'c -> ReaderM<'conf, 'out> =
        fun a b c -> { compute = fun conf -> f conf a b c }

    let liftConf (proj : 'confTo -> 'conf) 
                 (scomp : ReaderM<'conf, 'output>) 
                 : ReaderM<'confTo, 'output> =
        { compute = proj >> scomp.compute }
            
    // functor

    let fmap (f : 'a -> 'b) (g : 'c -> 'a) : ('c -> 'b) =
        g >> f

    let map (f : 'a -> 'b) 
            (scomp : ReaderM<'conf, 'a>) 
            : ReaderM<'conf,'b> =
        { compute = scomp.compute >> f }

    let (<?>) = map

    // applicative-functor
        
    let apply (f : ReaderM<'conf, 'a->'b>)
              (scomp : ReaderM<'conf, 'a>)
              : ReaderM<'conf, 'b> =
        { compute = fun conf ->
            let f' = run conf f
            let a  = run conf scomp
            f' a
        }

    let (<*>) = apply

    // monad

    let bind (f : 'a -> ReaderM<'conf,'b>) 
             (scomp : ReaderM<'conf, 'a>) 
             : ReaderM<'conf, 'b> =
        { compute = fun conf ->
            f (scomp.compute conf) 
            |> run conf 
        }

    type ReaderMBuilder internal () =
        member __.Bind(m, f)    = bind f m
        member __.Return(v)     = constant v
        member __.ReturnFrom(v) = v
        member __.Delay(f)      = f ()

    let Do = ReaderMBuilder()