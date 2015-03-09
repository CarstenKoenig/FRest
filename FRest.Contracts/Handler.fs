namespace FRest.Contracts

module Handler =
    
    type T =
        { 
            OnEcho : string -> Async<string>
        }

    let initial : T =
        { 
            OnEcho = fun s -> async { return s }
        }

    let onEcho (handler : T) (message : string) =
        handler.OnEcho message