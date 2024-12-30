open System
open System.IO

let lines = File.ReadAllLines "FSharp/19-linen-layout-input.txt"

let towels = lines[0].Split ", "
let designs = lines |> Array.skip 2


let isPossible (towels : string[]) (design : string) =
    let rec findSolution from (towels : string[]) (design : string) cache =
    let rec isPossible from (towels : string[]) (design : string) cache =
        if from < 30 then printfn $"isPossible called, from: {from}"
        let isMatch (str : string) from (sub : string) =
            if from + sub.Length <= str.Length
            then seq { 0..sub.Length - 1 }
                |> Seq.forall (fun i -> str[from + i] = sub[i])
            else false

        if from = design.Length
        then true
        else towels
             |> Array.exists (fun t -> isMatch design from t && isPossible (from + t.Length) towels design cache)
            //  |> Array.filter (fun t -> isMatch design from t)
            //  |> Array.exists (fun t -> isPossible (from + t.Length) towels design)

    isPossible 0 towels design

let result1 = designs
              |> Array.take 1
              |> Array.filter (isPossible 0 towels)
            //   |> Array.length