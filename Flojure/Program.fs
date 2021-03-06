﻿// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open ConvertToClojure

[<EntryPoint>]
let main argv = 
    let test = <@ 1 + 2 @>
    (ConvertToClojure test) |> printf "%s\n"

    let test = <@ let x = 2 in x + 2 @>
    (ConvertToClojure test) |> printf "%s\n"

    let test = <@ let x y z = y + z * 3 in let q = 3 in x 1 q + 2 @>
    ConvertToClojure test |> printf "%s\n"
//    printfn "\n\ndone"
//
    printfn "\n\nTest Lists"
    let test = <@ [1; 2; 3] @>
    ConvertToClojure test |> printf "%s\n"

    let test = <@ [1; 2; 3 + 3; 5; 6] @>
    ConvertToClojure test |> printf "%s\n"
//    
//
    let test = <@ let x y z = y + z * 3 in let q = 3 in [1; 2; x 3 3; 5; 6; q] @>
    ConvertToClojure test |> printf "%s\n"
//
//
    let test = <@ if 1 = 1 then 1 else 2 @>
    ConvertToClojure test |> printf "%s\n"
//
//    let test = <@ let f x = x / 2 in let q = 2 in if 1 = 1 then 1 else 2 @>
//    ConvertToClojure test |> printf "%s\n"
//
//    let test = <@ let f x = x / 2 in let q = 2 in if f q = 3 then 1 else if q = 1 then 2 else 3 @>
//    ConvertToClojure test |> printf "%s\n"
//
//    let test = <@ let f x = x / 2 in let q = 2 in if f q = 3 then 1 elif q = 1 then 2 else 3 @>
//    ConvertToClojure test |> printf "%s\n"
//
//    /// For Loop Tests: 
//    /// for i in start..finish do => ForIntegerRangeLoop( var, start, finish, expr )
//    /// for i = start to finish do => ForIntegerRangeLoop( var, start, finish, expr )
//    ///         In Clojure ForIntegerRangeLoop( var, start, finish, expr ) would translate to => (for [var (range start finish) ] (expr) )
    let test = <@ for i = 1 to 10 do printfn "test" @>
    ConvertToClojure test |> printf "%s\n"
//
//    let test = <@ let finish = 10 in for i = 1 to (finish + 3) do printfn "test" @>
//    ConvertToClojure test |> printf "%s\n"
//
//    let test = <@ let start = 1 in for i = (start - 1) to 10 do printfn "test" @>
//    ConvertToClojure test |> printf "%s\n"
//
//    let test = <@ let start = 1 in let finish = 9 in for i = (start - 1) to (finish + 1) do printfn "test" @>
//    ConvertToClojure test |> printf "%s\n"
//
//    let test = <@ let start = 1 in let finish = 9 in for i = ( if start = 1 then 0 else 1) to (finish + 1) do printfn "test" @>
//    ConvertToClojure test |> printf "%s\n"

    ConvertToClojure <@ let start = 1 in let finish = 9 in for i = ( if start = 1 then 0 else 1) to (finish + 1) do printfn "test" @>
    |> printf "%s\n"
//
//    
//    let test = <@ for i in 1 .. 10 do printfn "test" @>
//    ConvertToClojure test |> printf "%s\n"
//
//    /// for i = start downtofinish do => Quotations cannot contain descending for loops
//    /// for i in start..inc..finish do => this actually becomes a whileLoop with some wrappers to create the sequence
//    /// for i in [list] do
//
//    /// While loop
//    /// always true
//    let test = <@ while true do printf "Hello" @>
//    ConvertToClojure test |> printf "%s\n"
//
//    ConvertToClojure <@  printf "Hello" @> |> printf "%s\n"
//
//    ConvertToClojure <@  printfn "Hello" @> |> printf "%s\n"

    printfn "\nA Complex Example"
    let test = <@ let f x = x * x in 
                  let g y z = y + z * 3 in 
                  let q = 3 in 
                  [1; 2; g (f 3) 3; 5; 6; q] @>
    ConvertToClojure test |> printf "%s\n"

    printfn "\n\n"
    0 // return an integer exit code
