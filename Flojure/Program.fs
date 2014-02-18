// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

let ConvertToClojure expr =
    let rec ConvertToClojure expr =
        match expr with
        | Call(o, m, ps ) ->
            printf "( %s " m.Name
            for e in ps do
                ConvertToClojure e
            printf ") "
        | Value(v,ty) -> printf " %A " v
        | Let(var, definition, useIn) ->
            printf "( def %s " var.Name
            ConvertToClojure definition
            printf " )\n"
            ConvertToClojure useIn
        | Application(func, expr) -> 
            printf "( "
            ConvertToClojure func
            ConvertToClojure expr
            printf ") "
        | Lambda(var1, e) ->
            printf "( fn "
            printf "[%s] " var1.Name
            ConvertToClojure e
            printf ") "
        | Var(v) -> printf " %s " v.Name
        | IfThenElse(clause, thenExpr, elseExpr ) ->
            printf "(if "
            ConvertToClojure clause
            ConvertToClojure thenExpr
            ConvertToClojure elseExpr
            printf ") "
        | NewUnionCase(info, expr ) ->
            printf "( "
            match info.Name with
            | "Cons" -> 
                printf "cons "
                expr |> List.iter ( fun e -> ConvertToClojure e )
            | "Empty" -> printf "list"
            | _ -> failwithf "Unrecognized UnionCase info type %A" info.Name
            printf ") "
        | _ -> printf "unknown"
    ConvertToClojure expr
    printfn "\n\n"

[<EntryPoint>]
let main argv = 
    let test = <@ let x y z = y + z * 3 in let q = 3 in x 1 q + 2 @>
    ConvertToClojure test
    printfn "\n\ndone"

    printfn "\n\nTest Lists"
    let test = <@ [1; 2; 3 + 3; 5; 6] @>
    ConvertToClojure test
    

    let test = <@ let x y z = y + z * 3 in let q = 3 in [1; 2; x 3 3; 5; 6; q] @>
    ConvertToClojure test


    let test = <@ if 1 = 1 then 1 else 2 @>
    ConvertToClojure test

    let test = <@ let f x = x / 2 in let q = 2 in if f q = 1 then 1 else 2 @>
    ConvertToClojure test

    printfn "\n\n"
    0 // return an integer exit code
