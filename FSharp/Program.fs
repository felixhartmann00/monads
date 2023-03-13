(*
    this monads consist of three main features:
        the monadic type
            a wrapper around a value
        the unit operation
            a type converter that converts a value to a monadic type
        the bind operation
            an operation that takes a monadic value unwrapps it and then
            inserts it into a function that returns a new monadic value
*)

module Monad =
    // monadic type 'm a'
    type TWithLogs<'T> = 
        { 
            value : 'T // 'a'
            logs : List<string>
        }
    
    // type contructor (unit operation)
    let createFromT<'T> (value: 'T)=
        // recieves value 'a'
        // wraps it into a monadic value of type 'm a'
        // in this case TWithLogs<'T>
        { value = value ; logs = [] }

    (*
        use of this monad is to extract the log combining function (runWithLogs)
        and still being able to chain functions with the pipe operator
    *)

    // runner (bind operation)
    let runWithLogs transform tLogged =
        // and a function f with signature 'a' -> 'm b'
        // recieves monadic value of type 'm a'

        let newThingWithLog = transform tLogged.value
        
        let newLogs = List.append tLogged.logs newThingWithLog.logs
        
        { value = newThingWithLog.value ; logs = newLogs }

open Monad


let square i =
    
    let result = i * i
    
    let log = [$"{i} squared => {result}"]
    
    { value = result ; logs = log }

let addOne i =
    
    let result = i + 1

    let log = [$"{i} plus one => {result}"]

    { value = result ; logs = log }

let subtractOne i =

    let result = i - 1

    let log = [$"{i} minus one => {result}"]

    { value = result ; logs = log }




let print (t: TWithLogs<'T>) =
    t.logs |> List.iter (printfn "%s")
    printfn "%A \n" t.value


let subtractOneWithLogs = runWithLogs subtractOne
let addOneWithLogs = runWithLogs addOne
let squareWithLogs = runWithLogs square


createFromT 10
|> addOneWithLogs
|> squareWithLogs
|> subtractOneWithLogs
|> print

