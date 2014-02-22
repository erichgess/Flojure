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

let ConvertSharedFunctions name =
    match name with
    | "op_Addition" -> "+"
    | "op_Multiply" -> "*"
    | "op_Subtraction" -> "-"
    | "op_Division" -> "/"
    | "op_Modulus" -> "mod"
    | "op_Equality" -> "="
    | "op_Inequality" -> "not="
    | "op_LessThan" -> "<"
    | "op_LessThanOrEqual" -> "<="
    | "op_GreaterThan" -> ">"
    | "op_GreaterThanOrEqual" -> ">="
    | _ -> name

let ConvertToClojure expr =
    /// This recursive function will build a list of Clojure language tokens, 
    /// which can then be easily converted to a single string of Clojure Code
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
                     ConvertSharedFunctions m.Name :: parametersInClojure
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
                "(" :: []
                @ match info.Name with
                  | "Cons" -> 
                      "cons" ::
                      (expr |> convertExpressionListToStringList)
                  | "Empty" -> "list" :: []
                  | _ -> failwithf "Unrecognized UnionCase info type %A" info.Name
                @ ")" :: []
            | ForIntegerRangeLoop( var, start, finish, body ) ->
                "(for " :: "[" :: var.Name :: " (range " :: []
                @ ConvertToClojure start
                @ " " :: ConvertToClojure (<@ %%finish + 1 @>)
                @ ")] " :: []
                @ ConvertToClojure body
                @ ")" :: []
            | WhileLoop( clause, body ) -> 
                "(loop [] (if (not " :: []
                @ ConvertToClojure clause
                @ ") () (do " 
                :: ConvertToClojure body
               @ " (recur))))" :: []
            | _ -> (sprintf "<<unknown: %A>>" expr) :: []
        clojure
    ConvertToClojure expr |> List.fold ( fun acc s -> acc + s ) ""      // Convert the list of Clojure tokens into a single string