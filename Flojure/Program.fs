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
        | ForIntegerRangeLoop( var, start, finish, body ) ->
            printf  "(for (%s " var.Name
            printf "( range "
            ConvertToClojure start
            ConvertToClojure finish
            printf ") "
            ConvertToClojure body
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

    let test = <@ let f x = x / 2 in let q = 2 in if 1 = 1 then 1 else 2 @>
    ConvertToClojure test

    let test = <@ let f x = x / 2 in let q = 2 in if f q = 3 then 1 else if q = 1 then 2 else 3 @>
    ConvertToClojure test

    let test = <@ let f x = x / 2 in let q = 2 in if f q = 3 then 1 elif q = 1 then 2 else 3 @>
    ConvertToClojure test

    /// For Loop Tests: 
    /// for i in start..finish do => ForIntegerRangeLoop( var, start, finish, expr )
    /// for i = start to finish do => ForIntegerRangeLoop( var, start, finish, expr )
    ///         In Clojure ForIntegerRangeLoop( var, start, finish, expr ) would translate to => (for [var (range start finish) ] (expr) )
    let test = <@ for i = 1 to 10 do printfn "test" @>
    ConvertToClojure test

    /// for i = start downtofinish do => Quotations cannot contain descending for loops
    /// for i in start..inc..finish do => this actually becomes a whileLoop with some wrappers to create the sequence
    /// for i in [list] do
    /// for i in 

    printfn "\n\n"
    0 // return an integer exit code
