#I "packages/Http.fs/lib/net40/"
#I "packages/FSharp.Data/lib/net40"

#r "HttpClient.dll"
#r "FSharp.Data.dll"

open HttpClient
open FSharp.Data
open FSharp.Data.JsonExtensions
open System.Text.RegularExpressions

type ErrorType =
    | OpenErrors
    | IgnoredErrors
    | ClosedErrors
    override x.ToString() =
        match x with
        | OpenErrors -> "open-errors"
        | IgnoredErrors -> "ignored-errors"
        | ClosedErrors -> "closed-errors"

type ErrorceptionBase = JsonProvider<"baseResponseSample.json">
type ErrorceptionProjects = JsonProvider<"projectsResponseSample.json">
type Errorception = JsonProvider<"exceptionSummarySample.json">

let baseUrl = "https://api.errorception.com/"

let addAuthHeader r =
    r |> withHeader (Authorization "Basic <ENTER YOUR HASHED USERNAME / PASSWORD HERE>=")

let addVersionHeader r =
    r |> withHeader (Accept "application/vnd.errorception.v1+json")

let parseBaseResponse (response : Response) =
    match response.EntityBody with
    | None -> response.Headers, None
    | Some b -> response.Headers, Some (ErrorceptionBase.Parse b)

let parseProjectsResponse (response : Response) =
    match response.EntityBody with
    | None -> response.Headers, None
    | Some b -> response.Headers, Some (ErrorceptionProjects.Parse b)

let parseSummaryResponse (response : Response) =
    match response.EntityBody with
    | None -> response.Headers, None
    | Some b -> response.Headers, Some (Errorception.Parse b)

let getBaseRequest () =
    createRequest Get baseUrl
    |> addVersionHeader
    |> addAuthHeader

let getRequest (url : string) =
    createRequest Get url
    |> addVersionHeader
    |> addAuthHeader    

let getBaseResponse = getBaseRequest >> getResponse >> parseBaseResponse

let getProjectsResponse (request : Request) =
    request |> getResponse |> parseProjectsResponse

let getSummaryResponse (request : Request) =
    request |> getResponse |> parseSummaryResponse

let getNextUrl (responseHeaders : Map<ResponseHeader, string>) =
    let linkHeader =
        responseHeaders
        |> Map.tryPick (fun k v -> 
            match k with
            | HttpClient.ResponseHeader.Link -> Some v
            | _ -> None)

    match linkHeader with
    | None -> None
    | Some l ->
        let commands = l.Split [|','|]
        let next = 
            commands
            |> Array.tryPick (fun c -> 
                let regex = Regex("rel=\"next\"")
                match regex.IsMatch c with
                | true -> Some c
                | _ -> None)
        match next with
        | None -> None
        | Some l ->
            //printf "There is a rel=next header. Here is the rel=next header string: '%s'\r\n" next.Value
            let cleanedValue = l.Replace('<', ' ').Replace(">; rel=\"next\"", " ").Trim()
            let regex = Regex("^((http|ftp|https|www)://)?([\w+?\.\w+])+([a-zA-Z0-9\~\!\@\#\$\%\^\&\*\(\)_\-\=\+\\\/\?\.\:\;\'\,]*)?$", RegexOptions.IgnoreCase)
            match regex.IsMatch cleanedValue with
            | true -> Some (cleanedValue)
            | false -> None

let getErrors (errorType : ErrorType) = 
    match getBaseResponse() with
    | headers, None -> []
    | headers, Some body -> 
        
        let projectsLink = body.Links
                           |> Array.pick (fun i -> 
                                match i.Rel with
                                | "projects" -> Some i.Href
                                | _ -> None)

        match (getRequest projectsLink |> getProjectsResponse) with
        | headers, None -> []
        | headers, Some body ->
                let errorsHref = body.[0].Links |> Array.filter (fun i -> i.Rel = errorType.ToString())
                
                let rec printOpenErrors url (lst : Errorception.Root list) =
                    match (getRequest url |> getSummaryResponse) with
                    | headers, None -> []
                    | headers, Some body ->
                        let nextUrl = getNextUrl headers
                        match nextUrl with
                        | None -> body.[0] :: lst
                        | Some n ->
                            printOpenErrors nextUrl.Value (body.[0] :: lst)
                printOpenErrors errorsHref.[0].Href []

let outputToFile fileName (errors : Errorception.Root list) =
   use file = System.IO.File.CreateText(fileName)
   fprintf file "Message|Example Page|OccurenceCount\r\n"
   errors
   |> Seq.iter (fun elem -> fprintf file "\"%s\"|\"%s\"|%d\r\n" elem.Message elem.ExamplePage elem.OccurrenceCount)

let outputOpenErrors () =
    getErrors ErrorType.OpenErrors
    |> outputToFile "Errors.txt"