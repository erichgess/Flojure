module StandardIO

open NUnit.Framework
open FsUnit
open ConvertToClojure

[<TestFixture>] 
type ``Testing converting standard IO to Clojure`` () =
    
    [<Test>] member this.
     ``printf "hello" becomes (print "hello")`` () =
        ConvertToClojure <@  printf "Hello" @>
        |> should equal """(print "Hello")"""

    [<Test>] member this.
     ``printfn "hello" becomes (println "hello")`` () =
        ConvertToClojure <@  printfn "Hello" @>
        |> should equal """(println "Hello")"""