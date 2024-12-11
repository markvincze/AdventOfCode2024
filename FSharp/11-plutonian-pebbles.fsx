open System
open System.IO
open System.Collections.Generic

let stones = (File.ReadAllText "FSharp/11-plutonian-pebbles-input.txt").Split ' '
             |> Array.map Int64.Parse
             |> LinkedList<int64>

let digitCount n = floor((log10 (float n)) + 1.0) |> int

let hasEvenDigits n = (digitCount n) % 2 = 0

let splitDigits n = let dc = digitCount n
                    let div = pown 10L (dc / 2)
                    (n / div), (n % div)

let blink (stones : LinkedList<int64>) (stone : LinkedListNode<int64>) =
    match stone.Value with
    | 0L -> stone.Value <- 1
            stone.Next
    | n when hasEvenDigits n -> let (a, b) = splitDigits n
                                stones.AddBefore(stone, a) |> ignore
                                let node = stones.AddBefore(stone, b)
                                stones.Remove stone
                                node.Next
    | n -> stone.Value <- (stone.Value) * 2024L
           stone.Next

let rec blinkAll (stones : LinkedList<int64>) (stone : LinkedListNode<int64>) =
    let next = blink stones stone
    match next with
    | null -> ()
    | next -> blinkAll stones next

let rec blinkAllN (stones : LinkedList<int64>) n =
    // printfn "blinkAllN called with %d, length: %d, unique length: %d" n (stones.Count) (stones |> Set.ofSeq |> Set.count)
    match n with
    | 0 -> ()
    | n -> blinkAll stones stones.First
           blinkAllN stones (n - 1)

let printStones (stones : LinkedList<int64>) =
    stones |> Seq.iter (fun s -> printf "%d " s)
    printfn ""

blinkAllN stones 25

let result1 = stones.Count

let stonesMap = (File.ReadAllText "FSharp/11-plutonian-pebbles-input.txt").Split ' '
                |> Array.map Int64.Parse
                |> Array.map (fun n -> n, 1L)
                |> Map.ofArray

let setOrAdd k v m =
    match Map.tryFind k m with
    | None -> Map.add k v m
    | Some ev -> Map.add k (ev + v) m

let blinkMap newMap n count=
    match n with
    | 0L -> setOrAdd 1L count newMap
    | n when hasEvenDigits n -> let (a, b) = splitDigits n
                                let newMap = setOrAdd a count newMap
                                setOrAdd b count newMap
    | n -> setOrAdd (n * 2024L) count newMap

let rec blinkMapN n (stoneMap : Map<int64, int64>) =
    match n with
    | 0 -> stoneMap
    | n -> Map.fold
               blinkMap
               Map.empty<int64, int64>
               stoneMap
           |> blinkMapN (n - 1)

let m25 = blinkMapN 25 stonesMap |> Map.values |> Seq.sum

let result2 = blinkMapN 75 stonesMap |> Map.values |> Seq.sum