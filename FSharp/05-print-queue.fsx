open System
open System.IO

let parseRule (line : string) = let [|a; b|] = line.Split '|'
                                a |> Int32.Parse, b |> Int32.Parse

let parseUpdate (line : string) = line.Split ',' |> Array.map Int32.Parse |> List.ofArray

let lines = File.ReadAllLines "FSharp/05-print-queue-input.txt" |> List.ofArray
let ruleLines, updateLines = List.splitAt (List.findIndex (fun (line : string) -> System.String.IsNullOrWhiteSpace(line)) lines) lines

let rules = ruleLines |> List.map parseRule
let updates = updateLines |> List.skip 1 |> List.map parseUpdate

let isCorrect rules update =
    let rec doesSatisfyRule update didContainSecond (first, second) =
        match update with
        | [] -> true
        | h :: t -> if h = first then not didContainSecond
                    else if h = second then doesSatisfyRule t true (first, second)
                    else doesSatisfyRule t didContainSecond (first, second)

    List.forall (doesSatisfyRule update false) rules

let middle update = List.item (List.length update / 2) update

let result1 = updates
              |> List.filter (isCorrect rules)
              |> List.sumBy middle

let incorrectUpdates = updates |> List.filter (isCorrect rules >> not)

let getCorrectMiddle2 rules update =
    List.sortWith
        (fun a b -> if List.contains (a, b) rules then -1
                    else if List.contains (b, a) rules then 1
                    else 0)
        update
    |> middle

let result2 = 
    incorrectUpdates
    |> List.sumBy (getCorrectMiddle2 rules)
