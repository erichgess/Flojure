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
        let clojure =
            match expr with
            | Call(o, m, ps ) ->
                 "( " ::
                 if m.Name = "PrintFormat" || m.Name = "PrintFormatLine" then
                     (PrintFormat m.Name ps) :: []
                 else
                     let parametersInClojure = (ps |> List.map ( fun e -> " " :: ConvertToClojure e ) |> List.fold ( fun acc sl -> acc @ sl ) [] ) 
                     m.Name :: parametersInClojure
                 @ ( ") " :: [])
            | Value(v,ty) -> (sprintf "%A" v) ::[]
            | Let(var, definition, useIn) ->
                "( def " :: var.Name :: " " :: [] 
                @ (ConvertToClojure definition) 
                @ " )\n" :: [] 
                @ ConvertToClojure useIn
            | Application(func, expr) -> 
                "( " :: []
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
                @ ConvertToClojure thenExpr
                @ " " :: ConvertToClojure elseExpr
                @  ") " :: []
            | _ -> (sprintf "<<unknown: %A>>" expr) :: []
        clojure
    ConvertToClojure expr |> List.fold ( fun acc s -> acc + s ) ""