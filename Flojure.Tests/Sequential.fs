module Sequential

open NUnit.Framework
open FsUnit
open ConvertToClojure

[<TestFixture>] 
type ``Converting sequential expressions in F#`` () =
    [<Test>] member this.
     ``printf "a"; printf "b" becomes (do (printf "a") ( printf "b"))`` () =
        ConvertToClojure <@  printf "a"; printf "b" @>
        |> should equal "(do (print \"a\") (print \"b\"))"

    [<Test>] member this.
     ``printf "a"; 2 + 3 becomes (do (printf "a") (+ 2 3))`` () =
        ConvertToClojure <@  printf "a"; 2 + 3 @>
        |> should equal "(do (print \"a\") (+ 2 3))"

    [<Test>] member this.
     ``let f x = printf "apply f"; x * x in printf "Hello"; f 3`` () =
        ConvertToClojure <@ let f x = printf "apply f"; x * x in printf "Hello"; f 3 @>
        |> should equal "(def f ( fn [x] (do (print \"apply f\") (* x x))))\n(do (print \"Hello\") (f 3))"