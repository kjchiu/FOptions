module Util
open FParsec
open System.Diagnostics

let debugf fmt =
    Printf.ksprintf (fun s -> Trace.Write(s)) fmt
let debugfn fmt =
    Printf.ksprintf (fun s -> Trace.WriteLine(s)) fmt

let str = pstring 
let ws = spaces
let p_ws p = p <|> p .>> ws
let str_ws s = pstring s .>> spaces
#if VERBOSE_DEBUG
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        debugfn "%A: Entering %s" stream.Position label
        let reply = p stream
        debugfn "%A: Leaving %s (%A)(%c)" stream.Position label reply.Status (stream.Peek())
        reply
#else
let (<!>) p label =
    fun strm ->
        p strm
#endif
        

 
let test p str =
    debugfn "test: %s" str
    match run p str with
    | Success(result, _, _)   -> debugfn "Success: %A" result
    | Failure(errorMsg, _, _) -> debugfn "Failure: %s" errorMsg