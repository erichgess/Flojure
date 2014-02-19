module ConvertToClojure

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

let PrintFormat methodName expr =
    let h = expr |> List.head
    let ioMethod = if methodName = "PrintFormat" then "print" elif methodName = "PrintFormatLine" then "println" else "<<unknown>>"
    match h with
    | Coerce(expr, ty) ->
        match expr with
        | NewObject(ty, [Value(v,vty)]) ->
            sprintf "%s %A" ioMethod v
    | _ -> failwith "Unexpected expression in print statement"

let ConvertToClojure expr =
    let rec ConvertToClojure expr =
        match expr with
        | Call(o, m, ps ) ->
             "( " ::
             if m.Name = "PrintFormat" || m.Name = "PrintFormatLine" then
                 (PrintFormat m.Name ps)
             else
                 m.Name :: (ps |> List.map ( fun e -> ConvertToClojure e )
             :: ") " :: []
        | Value(v,ty) -> (sprintf " %A " v) ::[]
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