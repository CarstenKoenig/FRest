namespace FRest.Contracts

type 'a ErrorOrSuccess = Choice<exn,'a>

module ErrorOrSuccess =

    let failedWith err : 'a ErrorOrSuccess =
        Choice1Of2 err

    let succeededWith a : 'a ErrorOrSuccess = 
        Choice2Of2 a

    let (|Error|Success|) (a : 'a ErrorOrSuccess) =
        match a with
        | Choice1Of2 err -> Error err
        | Choice2Of2 a   -> Success a

    let fold e s =
        function
        | Error err -> e err
        | Success v -> s v

    let bind fa =
        fold failedWith fa

    let map f = 
        fold failedWith (f >> succeededWith)

    let guard err =
        function
        | true  -> succeededWith ()
        | false -> failedWith err

    let fromOption err =
        function 
        | Some v -> succeededWith v
        | None   -> failedWith err

    let toOption es =
        fold (fun _ -> None) Some es

    let valueOf es =
        fold raise id es

    let errorOf es =
        fold id (fun _ -> failwith "could not get error from success") es

    let raiseErr _ =
        fold raise (fun _ -> ())

    type ErrorOrSuccessBuilder internal () =
        member __.Bind(m, f)    = bind f m
        member __.Return(v)     = succeededWith v
        member __.ReturnFrom(v) = v
        member __.Delay(f)      = f ()

    let Do = ErrorOrSuccessBuilder()