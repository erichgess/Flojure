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
        | _ -> failwithf "Does not yet support: %A" expr
    | _ -> failwith "Unexpected expression in print statement"

let ConvertToClojure expr =
    let rec ConvertToClojure expr =
        let convertExpressionListToStringList exprList =
            (exprList |> List.map ( fun e -> " " :: ConvertToClojure e ) |> List.fold ( fun acc sl -> acc @ sl ) [] )

        let clojure =
            match expr with
            | Call(o, m, ps ) ->
                 "(" ::
                 if m.Name = "PrintFormat" || m.Name = "PrintFormatLine" then
                     (PrintFormat m.Name ps) :: []
                 else
                     let parametersInClojure = convertExpressionListToStringList ps
                     m.Name :: parametersInClojure
                 @ ( ")" :: [])
            | Value(v,ty) -> (sprintf "%A" v) ::[]
            | Let(var, definition, useIn) ->
                "(def " :: var.Name :: " " :: [] 
                @ (ConvertToClojure definition) 
                @ ")\n" :: [] 
                @ ConvertToClojure useIn
            | Application(func, expr) -> 
                "(" :: []
                @ ConvertToClojure func
                @ " " :: ConvertToClojure expr
                @ ") " :: []
            | Lambda(var1, e) ->
                "( fn " :: (sprintf "[%s] " var1.Name) :: []
                @ ConvertToClojure e
                @ ") " :: []
            | Var(v) -> v.Name :: []
            | IfThenElse(clause, thenExpr, elseExpr ) ->
                "(if " :: []
                @ ConvertToClojure clause
                @ " " :: ConvertToClojure thenExpr
                @ " " :: ConvertToClojure elseExpr
                @  ")" :: []
            | NewUnionCase(info, expr ) ->
                "( " :: []
                @ match info.Name with
                  | "Cons" -> 
                      printf "cons "
                      expr |> convertExpressionListToStringList
                  | "Empty" -> "list" :: []
                  | _ -> failwithf "Unrecognized UnionCase info type %A" info.Name
                @ ") " :: []
            | ForIntegerRangeLoop( var, start, finish, body ) ->
                "(for " :: "[" :: var.Name :: " (range " :: []
                @ ConvertToClojure start
                @ " " :: ConvertToClojure (<@ %%finish + 1 @>)
                @ ")] " :: []
                @ ConvertToClojure body
                @ ")" :: []
            | _ -> (sprintf "<<unknown: %A>>" expr) :: []
        clojure
    ConvertToClojure expr |> List.fold ( fun acc s -> acc + s ) ""