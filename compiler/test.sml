datatype fruit = Apple
datatype sizedFruit = Big of fruit | Small of fruit
(*val Big x = Big Apple;*)

fun foo(fruit) =
    let val Big x = getSizedFruit() in doSomethingWith(x) end

case getSizedFruit() of Big x => x | _ => ()
