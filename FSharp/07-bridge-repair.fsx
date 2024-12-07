open System
open System.IO

let parse (line : string) =
    let [| result; operands |] = line.Split ": "
    Int64.Parse result, operands.Split ' ' |> Array.map Int64.Parse |> List.ofArray

let isPossible result operands =
    let rec isPossible result operands acc =
        if acc > result then false
        else match operands with
             | [] -> acc = result
             | h :: t -> isPossible result t (acc + h) ||
                         isPossible result t (acc * h)

    isPossible result (List.skip 1 operands) (List.head operands)

let equations = File.ReadAllLines "FSharp/07-bridge-repair-input.txt"
                |> Array.map parse
                |> List.ofArray

let result1 = equations
              |> List.filter (fun (result, operands) -> isPossible result operands)
              |> List.sumBy fst
