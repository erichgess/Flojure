module ForIntegerRangeLoop

open NUnit.Framework
open FsUnit
open ConvertToClojure


[<TestFixture>] 
type ``Given a a for loop on an integer range`` () =
    
    [<Test>] member this.
     ``when using "for i in 1 .. 2 do", it returns a Clojure for`` () =
        ConvertToClojure <@ for i = 1 to 10 do printfn "test" @> 
        |> should equal "(for [i ( range  1 ( op_Addition  10  1 ) )] ( PrintFormatLine unknown) )"

    [<Test>] member this.
     ``when using an expression as the finish clause of the foor loop, it returns a Clojure for`` () =
        ConvertToClojure <@ let finish = 10 in for i = 1 to (finish + 3) do printfn "test" @> 
        |> should equal "(for [i ( range  1 ( op_Addition  10  1 ) )] ( PrintFormatLine unknown) )"
