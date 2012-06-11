module IronOptions
open FParsec
open Util
type UserState = unit
type Parser<'a> = Parser<'a, UserState>

type ScalarAst =
    | Int of int32
    | Float of float
    | Boolean of bool
    | String of string

let option_delim : Parser<_>  = pstring "--"

let flag s = option_delim >>? pstring s |>> (fun name-> (name, Boolean(true)))

let string_literal = 
    let validChar = satisfy (fun c -> c <> '\\' && c <> '"')
    between (str "\"") (str "\"") (manyChars validChar)
    
let scalar_option (name) : Parser<string*ScalarAst>=
    let scalar_option_p parser type_convert =
        name |> flag |> p_ws .>>.? parser
        |>> (fun (_flag, _value) -> 
            let _name, _ = _flag
            (_name, type_convert _value))
    scalar_option_p pfloat Float
    <|> scalar_option_p string_literal String 

let option name = scalar_option name
                  <|> flag name 

let options names = 
    let gen_parser name =  option name |> p_ws <!> name
    let first = Seq.head names
    many (Seq.fold (<|>)  (gen_parser first)  (Seq.map  gen_parser (Seq.skip 1 names)))

test (options ["a"; "b"; "c"]) "--b 0.5 --a --a 123 --b \"hello there\""




