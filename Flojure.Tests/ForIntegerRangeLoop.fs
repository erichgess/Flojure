﻿module ForIntegerRangeLoop

open NUnit.Framework
open FsUnit
open ConvertToClojure


[<TestFixture>] 
type ``Given a for loop on an integer range`` () =
    
    [<Test>] member this.
     ``when using "for i = 1 to 10 do", it returns a Clojure for`` () =
        ConvertToClojure <@ for i = 1 to 10 do printfn "test" @> 
        |> should equal """(for [i (range 1 (+ 10 1))] (println "test"))"""

    [<Test>] member this.
     ``when using "for i in 1 .. 10 do", it returns a Clojure for`` () =
        ConvertToClojure <@ for i in 1 .. 10 do printfn "test" @> 
        |> should equal """(for [i (range 1 (+ 10 1))] (println "test"))"""

    [<Test>] member this.
     ``when using an expression as the finish clause of the foor loop, it returns a Clojure for`` () =
        ConvertToClojure <@ let finish = 10 in for i = 1 to (finish + 3) do printfn "test" @> 
        |> should equal "(def finish 10)\n(for [i (range 1 (+ (+ finish 3) 1))] (println \"test\"))"



    [<Test>] member this.
     ``when using an expression as the start & finish clause of the foor loop, it returns a Clojure for`` () =
        ConvertToClojure <@ let start = 1 in let finish = 9 in for i = (start - 1) to (finish + 1) do printfn "test" @>
        |> should equal "(def start 1)\n(def finish 9)\n(for [i (range (- start 1) (+ (+ finish 1) 1))] (println \"test\"))"


    [<Test>] member this.
     ``when using an if expression as the start clause, it returns a Clojure for with an if in the start clause`` () =
        ConvertToClojure <@ let start = 1 in let finish = 9 in for i = ( if start = 1 then 0 else 1) to (finish + 1) do printfn "test" @>
        |> should equal "(def start 1)\n(def finish 9)\n(for [i (range (if (= start 1) 0 1) (+ (+ finish 1) 1))] (println \"test\"))"