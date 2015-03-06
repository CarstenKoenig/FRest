namespace FRest.Client

open FRest.Contracts

module Api =

    type IApi =
        abstract echo : string -> Async<Messages.Echo>

    let private echo (url : Url) (echo : string) : Async<Messages.Echo> =
        async {
            let! response =
                sprintf "/Echo?msg=%s" echo
                |> getAsync url
            return ErrorOrSuccess.valueOf response
        }

    let createApi (url : Url) : IApi =
        { new IApi with
            member __.echo msg = echo url msg
        }