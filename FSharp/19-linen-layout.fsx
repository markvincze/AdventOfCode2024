open System
open System.IO

let lines = File.ReadAllLines "FSharp/19-linen-layout-input.txt"

let towels = lines[0].Split ", " |> List.ofArray
let designs = lines |> Array.skip 2

let isPossible (towels : string list) (design : string) =
    let isMatch (str : string) from (sub : string) =
        if from + sub.Length <= str.Length
        then seq { 0..sub.Length - 1 }
            |> Seq.forall (fun i -> str[from + i] = sub[i])
        else false

    let rec findSolution from (allTowels : string list) (towels : string list) (design : string) notPossible =
        // printfn $"findSolution called, from: {from}"
        match towels with
        | [] -> false, Set.add from notPossible
        | h :: t -> //printfn $"Testing {h}"
                    if isMatch design from h
                    then let result, notPossible = isPossible (from + h.Length) allTowels design notPossible
                         if result
                         then true, notPossible
                         else findSolution from allTowels t design notPossible
                    else findSolution from allTowels t design notPossible
    and isPossible from (towels : string list) (design : string) notPossible =
        // printfn $"isPossible called, from: {from}"
        // (false, notPossible)
        if (Set.contains from notPossible) then (false, notPossible)
        else if from = design.Length then (true, notPossible)
        else findSolution from towels towels design notPossible
            // towels
            //  |> Array.exists (fun t -> isMatch design from t && isPossible (from + t.Length) towels design cache)
            //  |> Array.filter (fun t -> isMatch design from t)
            //  |> Array.exists (fun t -> isPossible (from + t.Length) towels design)

    isPossible 0 towels design Set.empty<int> |> fst

let result1 = designs
            //   |> Array.take 1
              |> Array.filter (isPossible towels)
              |> Array.length