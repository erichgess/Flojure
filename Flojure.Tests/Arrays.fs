module Arrays

open NUnit.Framework
open FsUnit
open ConvertToClojure

[<TestFixture>] 
type ``Testing F# arrays mapping to Clojure vectors`` () =
    [<Test>] member this.
     ``[||] becomes []`` () =
        ConvertToClojure <@  [||] @>
        |> should equal """[]"""

    [<Test>] member this.
     ``[|1|] becomes [ 1]`` () =
        ConvertToClojure <@  [|1|] @>
        |> should equal """[ 1]"""

    [<Test>] member this.
     ``[|1; 2|] becomes [ 1 2]`` () =
        ConvertToClojure <@  [|1; 2|] @>
        |> should equal """[ 1 2]"""

    [<Test>] member this.
     ``let a = [|1; 2|] in a becomes (def a [ 1 2]) a`` () =
        ConvertToClojure <@  let a = [|1; 2|] in a @>
        |> should equal "(def a [ 1 2])\na"

    [<Test>] member this.
     ``[|1; 2|].[0] becomes (get [ 1 2] 0)`` () =
        ConvertToClojure <@ [|1; 2|].[0] @>
        |> should equal "(get [ 1 2] 0)"

    [<Test>] member this.
     ``let a = [|1; 2|] in a.[1] becomes (def a [ 1 2]) (get a 1)`` () =
        ConvertToClojure <@  let a = [|1; 2|] in a.[1] @>
        |> should equal "(def a [ 1 2])\n(get a 1)"

// The Length is commented out for now, because it will require handling the PropertyGet Quotation type.
//    [<Test>] member this.
//     ``[|1; 2|].Length becomes (count [ 1 2])`` () =
//        ConvertToClojure <@ [|1; 2|].Length @>
//        |> should equal "(count [ 1 2])"
//
//    [<Test>] member this.
//     ``let a = [|1; 2|] in a.Length becomes (def a [ 1 2]) (count a)`` () =
//        ConvertToClojure <@  let a = [|1; 2|] in a.Length @>
//        |> should equal "(def a [ 1 2])\n(count a)"