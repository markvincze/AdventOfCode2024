open System
open System.IO

let lines = File.ReadAllLines "FSharp/18-ram-run-input.txt"

let byteCount = 1024
let width = 71
let height = 71

let getCorruptedN n (lines : string[]) =
    lines
    |> Array.take n
    |> Array.map
           (fun s -> let [|x; y|] = s.Split ','
                     (Int32.Parse x, Int32.Parse y))
    |> Set.ofArray

let possibleNextSteps w h visited corrupted (x, y) =
    let canStep (x, y) = not <| Set.contains (x, y) visited && not <| Set.contains (x, y) corrupted
    seq {
        if x > 0 && canStep (x - 1, y) then yield (x - 1, y)
        if y > 0 && canStep (x, y - 1) then yield (x, y - 1)
        if x < w - 1 && canStep (x + 1, y) then yield (x + 1, y)
        if y < h - 1 && canStep (x, y + 1) then yield (x, y + 1)
    }

let rec findShortestPath w h q visited corrupted stepCount =
    if Set.isEmpty q then None
    else if q |> Set.contains (w - 1, h - 1) then Some stepCount
    else let newQueue = q
                        |> Seq.collect (possibleNextSteps w h visited corrupted)
                        |> Set.ofSeq
         findShortestPath w h newQueue (Set.union visited q) corrupted (stepCount + 1)

let result1 = findShortestPath width height (Set.singleton (0, 0)) Set.empty<int * int> (getCorruptedN 1024 lines) 0

// let result2 =
//     seq { 1024..lines.Length }
//     |> Seq.find (fun n -> findShortestPath width height (Set.singleton (0, 0)) Set.empty<int * int> (getCorruptedN n lines) 0 = None)