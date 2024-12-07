open System
open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllText "FSharp/03-mull-it-over-input.txt"

let regex1 = new Regex(@"mul\((\d{1,3}),(\d{1,3})\)")

let matches1 = regex1.Matches input

let result1 = matches1 
              |> Seq.sumBy (fun m -> (m.Groups[1].Value |> Int32.Parse) * (m.Groups[2].Value |> Int32.Parse))

let regex2 = new Regex(@"mul\((\d{1,3}),(\d{1,3})\)|(do\(\))|(don't\(\))")

let matches2 = regex2.Matches input

let rec proc (ms : Match list) enabled acc =
    match ms with
    | [] -> acc
    | h :: t -> match h.Value with
                | "do()" -> proc t true acc
                | "don't()" -> proc t false acc
                | _ -> if enabled
                       then let nextVal = (h.Groups[1].Value |> Int32.Parse) * (h.Groups[2].Value |> Int32.Parse)
                            proc t true (acc + nextVal)
                       else proc t false acc

let result2 = proc (List.ofSeq matches2) true 0
