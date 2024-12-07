open System
open System.IO

let parse (line : string) = let [| num1; num2 |] = line.Split("   ")
                            (Int32.Parse num1, Int32.Parse num2)

let lines = File.ReadAllLines "FSharp/01-historian-hysteria-input.txt"
            |> Array.map parse
            |> List.ofArray

let ids1 = lines |> List.map fst |> List.sort
let ids2 = lines |> List.map snd |> List.sort

let result1 = List.zip ids1 ids2
              |> List.sumBy (fun (a, b) -> a - b |> abs)

let result2 =
    ids1
    |> List.sumBy (fun x -> x * (List.filter (fun y -> y = x) ids2 |> List.length))
