// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

let rec ConvertToClojure expr =
    match expr with
    | Call(o, m, ps ) ->
        printf "( %s " m.Name
        for e in ps do
            ConvertToClojure e
        printf " )"
    | Value(v,ty) -> printf " %A " v
    | _ -> printf "unknown"

[<EntryPoint>]
let main argv = 
    let test = <@ 2 + 2 * 3 @>
    ConvertToClojure test
    printfn "\n\ndone"
    0 // return an integer exit code
