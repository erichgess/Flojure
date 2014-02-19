module WhileLoop

open NUnit.Framework
open FsUnit
open ConvertToClojure


[<TestFixture>] 
type ``Given a a while loop`` () =
    
    [<Test>] member this.
     ``when using true for the clause, then the clause is negated`` () =
        ConvertToClojure <@ while true do printf "Hello" @>
        |> should equal """(loop [] (if (not true) () (do (print "Hello") (recur))))"""

    [<Test>] member this.
     ``when using 1=1 for the clause`` () =
        ConvertToClojure <@ while 1=1 do printf "Hello" @> 
        |> should equal """(loop [] (if (not (op_Equality 1 1)) () (do (print "Hello") (recur))))"""