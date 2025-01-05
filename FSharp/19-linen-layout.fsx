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

    let rec findSolution from (allTowels : string list) (towels : string list) (design : string) (solCountCache : Map<int, int64>) acc =
        match towels with
        | [] -> acc, Map.add from acc solCountCache
        | h :: t -> let sc, solCountCache =
                        if isMatch design from h
                        then isPossible (from + h.Length) allTowels design solCountCache
                        else 0L, solCountCache
                    findSolution from allTowels t design solCountCache (acc + sc)

    and isPossible from (towels : string list) (design : string) (solCountCache : Map<int, int64>) =
        match Map.tryFind from solCountCache with
        | Some sc -> sc, solCountCache
        | None -> if from = design.Length then (1, solCountCache)
                  else findSolution from towels towels design solCountCache 0L

    isPossible 0 towels design Map.empty<int, int64> |> fst

let result1 = designs
              |> Array.filter (fun d -> isPossible towels d > 0)
              |> Array.length

let result2 = designs
              |> Array.sumBy (isPossible towels >> int64)