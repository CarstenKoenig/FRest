namespace FRest.Client

open System
open RestSharp

open FRest.Contracts

type Url = string
exception Failed of int * string
    
[<AutoOpen>]    
module AsyncRequests =

    let requestWithAsync (url : Url) (request : string, verb : Method) (addData : IRestRequest -> IRestRequest) : Async<ErrorOrSuccess<'a>> =
        let client  = RestClient(url)
        let request = 
            RestRequest(request, verb)
            |> addData

        Async.FromContinuations(fun (result, error, cancel) ->
            client.ExecuteAsync(
                request, 
                Action<IRestResponse<'a>> (fun resp ->  
                    match resp.ResponseStatus with
                    | ResponseStatus.Aborted ->
                        OperationCanceledException () 
                        |> cancel
                    | ResponseStatus.Completed ->
                        match resp.StatusCode with
                        | Net.HttpStatusCode.OK ->
                            resp.Data 
                            |> ErrorOrSuccess.succeededWith 
                            |> result
                        | _ -> exn (resp.StatusDescription) |> error
                    | ResponseStatus.Error ->
                        match resp.ErrorException with
                        | :? Failed as f ->
                            ErrorOrSuccess.failedWith f
                            |> result
                        | _ as err when err <> null ->
                            error err
                        | _ ->
                            exn "unknown server exception" 
                            |> error
                    | ResponseStatus.TimedOut ->
                        TimeoutException () 
                        |> error
                    | ResponseStatus.None ->
                        exn "no reply" 
                        |> error
                    | _ -> failwith "unhandeled ResponseStatus"))
            |> ignore)

    let addXmlBody (body : 'b) (request : IRestRequest) : IRestRequest =
        request.AddXmlBody body

    let noData (request : IRestRequest) : IRestRequest =
        request

    let getAsync (url : Url) (request : string) : Async<ErrorOrSuccess<'a>> =
        requestWithAsync url (request, Method.GET) noData

    let postXmlAsync (url : Url) (request : string, data : 'b) : Async<ErrorOrSuccess<'a>> =
        requestWithAsync url (request, Method.POST) (addXmlBody data)
