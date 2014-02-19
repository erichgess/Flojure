module ConvertToClojure

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

let PrintFormat expr =
    let h = expr |> List.head
    match h with
    | Coerce(expr, ty) ->
        match expr with
        | NewObject(ty, [Value(v,vty)]) ->
            sprintf "print %A" v
    | _ -> failwith "Unexpected expression in print statement"

let ConvertToClojure expr =
    let rec ConvertToClojure expr =
        match expr with
        | Call(o, m, ps ) ->
            printf "( "
            if m.Name = "PrintFormat" then
                printf "%s" (PrintFormat ps)
            else
                printf "%s" m.Name
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
            printf  "(for [%s ( range " var.Name
            ConvertToClojure start
            ConvertToClojure (<@ %%finish + 1 @>)
            printf ")] "
            ConvertToClojure body
            printf ") "
        | WhileLoop( clause, body ) -> 
            printf "(loop [] (if "
            printf "(not " 
            ConvertToClojure clause
            printf ") "
            printf "() "
            printf "(do "
            ConvertToClojure body
            printf " (recur))))"
        | _ -> printf "<<unknown: %A>>" expr
    ConvertToClojure expr
    printfn "\n\n"