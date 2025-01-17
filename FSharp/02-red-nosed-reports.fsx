open System
open System.IO

let parse (line : string) = line.Split ' ' |> Array.map Int32.Parse |> List.ofArray

let reports = File.ReadAllLines "FSharp/02-red-nosed-reports-input.txt"
              |> Array.map parse
              |> List.ofArray

let isSafePair a b inc = if inc
                         then b - a >= 1 && b - a <= 3
                         else a - b >= 1 && a - b <= 3

let isSafe report =
    let rec isSafe report inc =
        match report with
        | h1 :: (h2 :: t) -> isSafePair h1 h2 inc && isSafe (h2 :: t) inc
        | _ -> true

    isSafe report (report[0] < report[1])

let result1 = reports
              |> List.filter isSafe
              |> List.length

let isSafe2 report =
    let rec isSafe2 report inc dampenerLeft =
        match report with
        | [ _ ] -> true
        | h1 :: (h2 :: t) -> isSafePair h1 h2 inc && isSafe2 (h2 :: t) inc dampenerLeft ||
                             (dampenerLeft && isSafe2 (h1 :: t) inc false)

    isSafe2 report (report[0] < report[1]) true ||
    isSafe2 (List.skip 1 report) (report[1] < report[2]) false

let isSafe3 report =
    isSafe report ||
    [0..(List.length report - 1)]
    |> List.exists (fun i -> isSafe (List.removeAt i report))

// This approach is not working for some reason
let result2 = reports
              |> List.filter isSafe2
              |> List.length

let result3 = reports
              |> List.filter isSafe3
              |> List.length

// 22 21 25 27 28

for i in 0 .. List.length reports - 1 do
    if isSafe2 reports[i] <> isSafe3 reports[i]
    then printfn $"{reports[i]}: {isSafe2 reports[i]} {isSafe3 reports[i]}"
