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

let processOp (opCode, operand) state =
    match opCode with
    | 0 -> { state with a = state.a / (pown 2L (combo state operand |> int)); ip = state.ip + 2 }, None
    | 1 -> { state with b = state.b ^^^ operand; ip = state.ip + 2 }, None
    | 2 -> { state with b = (combo state operand) % 8L; ip = state.ip + 2 }, None
    | 3 -> { state with ip = if state.a = 0 then state.ip + 2 else operand }, None
    | 4 -> { state with b = state.b ^^^ state.c; ip = state.ip + 2 }, None
    | 5 -> { state with ip = state.ip + 2 }, Some (combo state operand % 8L)
    | 6 -> { state with b = state.a / (pown 2L (combo state operand |> int)); ip = state.ip + 2 }, None
    | 7 -> { state with c = state.a / (pown 2L (combo state operand |> int)); ip = state.ip + 2 }, None
    | _ -> failwith "Invalid input"

let rec runProgram state (program : int[]) output =
    if state.ip < 0 || state.ip > program.Length - 2
    then output
    else let state, newOutput = processOp (program[state.ip], program[state.ip + 1]) state
         let output = match newOutput with
                      | None -> output
                      | Some n -> n :: output
         runProgram state program output

let result1 = String.Join(',', runProgram state program [] |> List.rev)

let rec runProgram2 state (program : int[]) outputCnt =
    if state.ip < 0 || state.ip > program.Length - 2
    then outputCnt = program.Length
    // else if List.length output > maxLength
    // then None
    else let state, newOutput = processOp (program[state.ip], program[state.ip + 1]) state
         match newOutput with
         | None -> runProgram2 state program outputCnt
         | Some n when outputCnt > program.Length - 1 -> false
         | Some n when n <> program[outputCnt] -> false
         | _ -> runProgram2 state program (outputCnt + 1)

// let expectedOutput = lines[4].Split(": ")[1]

let rec findResult2 state (program : int[]) =
    if runProgram2 state program 0
    then state.a
    else findResult2 { state with a = state.a + 1L } program
    // match runProgram state program [] maxLength with
    // | None -> findResult2 { state with a = state.a + 1L } program programRev maxLength
    // | Some o when o = programRev -> state.a
    // | _ -> findResult2 { state with a = state.a + 1L } program programRev maxLength

// let result2 = findResult2 state program

// let rec runProgram state (program : int[]) output maxLength =
//     if state.ip < 0 || state.ip > program.Length - 2
//     then Some output
//     else if List.length output > maxLength
//     then None
//     else let state, output = processOp (program[state.ip], program[state.ip + 1]) state output
//          runProgram state program output maxLength