open System
open System.IO

type State = {
    ip : int
    a : int64
    b : int64
    c : int64
}

let lines = File.ReadAllLines "FSharp/17-chronospatial-computer-input.txt"

let parse (lines : string[]) =
    let program = (lines[4].Split(": ")[1]).Split(",")
                  |> Array.map Int32.Parse

    let state = {
        a = lines[0].Split(": ")[1] |> Int64.Parse
        b = lines[1].Split(": ")[1] |> Int64.Parse
        c = lines[2].Split(": ")[1] |> Int64.Parse
        ip = 0
    }

    program, state

let program, state = parse lines

let combo state = function
                  | x when x >= 0 && x <= 3 -> int64 x
                  | 4 -> state.a
                  | 5 -> state.b
                  | 6 -> state.c
                  | _ -> failwith "Invalid input"

let processOp (opCode, operand) state output =
    match opCode with
    | 0 -> { state with a = state.a / (pown 2L (combo state operand |> int)); ip = state.ip + 2 }, output
    | 1 -> { state with b = state.b ^^^ operand; ip = state.ip + 2 }, output
    | 2 -> { state with b = (combo state operand) % 8L; ip = state.ip + 2 }, output
    | 3 -> { state with ip = if state.a = 0 then state.ip + 2 else operand }, output
    | 4 -> { state with b = state.b ^^^ state.c; ip = state.ip + 2 }, output
    | 5 -> { state with ip = state.ip + 2 }, (combo state operand % 8L) :: output
    | 6 -> { state with b = state.a / (pown 2L (combo state operand |> int)); ip = state.ip + 2 }, output
    | 7 -> { state with c = state.a / (pown 2L (combo state operand |> int)); ip = state.ip + 2 }, output
    | _ -> failwith "Invalid input"

let rec runProgram state (program : int[]) output =
    if state.ip < 0 || state.ip > program.Length - 2
    then String.Join(',', output |> List.rev)
    else let state, output = processOp (program[state.ip], program[state.ip + 1]) state output
         runProgram state program output

let result1 = runProgram state program []