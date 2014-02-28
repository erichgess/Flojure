Flojure
====================

A library for translating F# into Clojure.

### How It Works
Flojure operates using F# Code Quotations.  Pass the ConvertToClojure method an F# Code Quotation and the function will output the equivalent Clojure code as a string.

### Isn't this absurd?
Yes.  However, it's been a great way for me to deepen my knowledge of how F# works while learning the basics of Clojure.

### Examples

#### For Loop Expression
F# Code

    ConvertToClojure <@ for i = 1 to 10 do printfn "test" @> 
    
Outputs

    (for [i ( range  1 ( +  10  1 ) )] ( println "test") )
    
    
#### A more complex example
F# Code

    ConvertToClojure <@ let start = 1 in let finish = 9 in for i = ( if start = 1 then 0 else 1) to (finish + 1) do printfn "test" @>
    
Outputs

    ( def start  1  )
    ( def finish  9  )
    (for [i ( range (if ( =  start  1 )  0  1 ) ( + ( +  finish  1 )  1 ) )] ( println "test") )"


#### Functions, currying, and lists
F# Code

    let test = <@ let f x = x * x in 
                  let g y z = y + z * 3 in 
                  let q = 3 in 
                  [1; 2; g (f 3) 3; 5; 6; q] @>

Outputs

    (def f ( fn [x] (* x x)) )
    (def g ( fn [y] ( fn [z] (+ y (* z 3))) ) )
    (def q 3)
    (cons 1 (cons 2 (cons ((g (f 3) )  3)  (cons 5 (cons 6 (cons q (list)))))))
