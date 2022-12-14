(*
    monads consist of three main features:
        the monadic type
            a wrapper around a value
        the unit operation
            a type converter that converts a value to a monadic type
        the bind operation
            an operation that takes a monadic value unwrapps it and then
            inserts it into a function that returns a new monadic value
*)

module SomethingLogged =
    // monadic type 'm a'
    type TWithLogs<'T> = 
        { 
            something : 'T // 'a'
            logs : List<string>
        }
    
    // type contructor (unit operation)
    let createFromT<'T> (something: 'T)=
        // revieves value 'a'
        // wraps it into a monadic value of type 'm a'
        // in this case TWithLogs<'T>
        { something = something ; logs = [] }
    
open SomethingLogged


let square i =
    
    let result = i * i
    
    let log = [$"{i} squared => {result}"]
    
    { something = result ; logs = log }

let addOne i =
    
    let result = i + 1

    let log = [$"{i} plus one => {result}"]

    { something = result ; logs = log }


let concatination s1 s2 =
    let result = s1 + " " + s2

    let log = [$"string {s1} and {s2} have been modified to {result}"]

    { something = result ; logs = log }

let appendGreeting s =
    let result = s + " weclome to F#"

    let log = [$"string {s} has been modified to {result}"]

    { something = result ; logs = log }


// runner (bind operation)
let runWithLogs transform tLogged =
    // recieves monadic value of type 'm a'
    // and a function f with signature 'a' -> 'm b'

    let newThingWithLog = transform tLogged.something
    
    let newLogs = List.append tLogged.logs newThingWithLog.logs
    
    { something = newThingWithLog.something ; logs = newLogs }



let printWithLogs (t: TWithLogs<'T>) =
    t.logs |> List.iter (printfn "%s")
    printfn "%A \n" t.something



let addOneWithLogs = runWithLogs addOne
let squareWithLogs = runWithLogs square

let concatfn = concatination "hello"
let concatWithLogs = runWithLogs concatfn
let appendWithLogs = runWithLogs appendGreeting


createFromT 10
|> addOneWithLogs
|> squareWithLogs
|> printWithLogs

createFromT "world"
|> concatWithLogs
|> appendWithLogs
|> printWithLogs
