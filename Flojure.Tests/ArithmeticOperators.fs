module ArithmeticOperators

open NUnit.Framework
open FsUnit
open ConvertToClojure

[<TestFixture>] 
type ``Testing converting standard arithmetic operators to Clojure`` () =
    [<Test>] member this.
     ``2 + 3 becomes (+ 2 3)`` () =
        ConvertToClojure <@  2 + 3 @>
        |> should equal """(+ 2 3)"""

    [<Test>] member this.
     ``2 - 3 becomes (- 2 3)`` () =
        ConvertToClojure <@ 2 - 3 @>
        |> should equal """(- 2 3)"""

    [<Test>] member this.
     ``2 * 3 becomes (* 2 3)`` () =
        ConvertToClojure <@  2 * 3 @>
        |> should equal """(* 2 3)"""

    [<Test>] member this.
     ``2 / 3 becomes (/ 2 3)`` () =
        ConvertToClojure <@  2 / 3 @>
        |> should equal """(/ 2 3)"""

    [<Test>] member this.
     ``2 % 3 becomes (mod 2 3)`` () =
        ConvertToClojure <@  2 % 3 @>
        |> should equal """(mod 2 3)"""

[<TestFixture>] 
type ``Testing converting standard conditional operators to Clojure`` () =
    [<Test>] member this.
     ``2 < 3 becomes (< 2 3)`` () =
        ConvertToClojure <@  2 < 3 @>
        |> should equal """(< 2 3)"""

    [<Test>] member this.
     ``2 <= 3 becomes (<= 2 3)`` () =
        ConvertToClojure <@ 2 <= 3 @>
        |> should equal """(<= 2 3)"""

    [<Test>] member this.
     ``2 > 3 becomes (> 2 3)`` () =
        ConvertToClojure <@  2 > 3 @>
        |> should equal """(> 2 3)"""

    [<Test>] member this.
     ``2 >= 3 becomes (>= 2 3)`` () =
        ConvertToClojure <@  2 >= 3 @>
        |> should equal """(>= 2 3)"""

    [<Test>] member this.
     ``2 = 3 becomes (= 2 3)`` () =
        ConvertToClojure <@  2 = 3 @>
        |> should equal """(= 2 3)"""

    [<Test>] member this.
     ``2 <> 3 becomes (not= 2 3)`` () =
        ConvertToClojure <@  2 <> 3 @>
        |> should equal """(not= 2 3)"""