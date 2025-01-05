open System
open System.IO

let lines = File.ReadAllLines "FSharp/20-race-condition-input.txt"

let maze = Array2D.init
               lines[0].Length
               lines.Length
               (fun x y -> match lines[y][x] with
                           | '#' -> true
                           | _ -> false) 

let findCharPos (lines : string[]) c =
    seq {
        for x in 0..lines[0].Length - 1 do
            for y in 0..lines.Length - 1 do
                yield (x, y)
    }
    |> Seq.find (fun (x, y) -> lines[y][x] = c)


let sx, sy = findCharPos lines 'S'
let ex, ey = findCharPos lines 'E'

let steps (maze : bool[,]) disableCollision (x, y) =
    seq {
        if x > 0 && (disableCollision || not maze[x - 1, y]) then yield (x - 1, y)
        if y > 0 && (disableCollision || not maze[x, y - 1]) then yield (x, y - 1)
        if x < Array2D.length1 maze - 1 && (disableCollision || not maze[x + 1, y]) then yield (x + 1, y)
        if y < Array2D.length2 maze - 1 && (disableCollision || not maze[x, y + 1]) then yield (x, y + 1)
    }

let rec findDistance maze q d visited (ex, ey) =
    if Set.contains (ex, ey) q then d
    else let newQ = Seq.collect (steps maze false) q
                    |> Seq.except visited
                    |> Set.ofSeq
         findDistance maze newQ (d + 1) (Set.union visited q) (ex, ey)

let minDist = findDistance maze (Set.singleton (sx, sy)) 0 Set.empty<int * int> (ex, ey)

let rec cheatRouteCount maze q d visited cheatLeft cs ce maxDist (ex, ey) =
    if d > maxDist then []//, Set.empty<(int * int) * (int * int)>
    else if Set.contains (ex, ey) q then [ (d, Option.get cs, Option.get ce) ]  //Set.exists (fun (n, _, _) -> n = (ex, ey)) q
    else match cheatLeft with
         | 0 -> let newQueue = Seq.collect (steps maze false) q
                              |> Seq.except visited |> Set.ofSeq
                cheatRouteCount maze newQueue (d + 1) (Set.union visited q) 0 cs ce maxDist (ex, ey)
         | 2 -> let newQueueWithoutCheat = Seq.collect (steps maze false) q
                                           |> Seq.except visited |> Set.ofSeq
                let resultWithoutCheat = cheatRouteCount maze newQueueWithoutCheat (d + 1) (Set.union visited q) 2 cs ce maxDist (ex, ey)
                let resultWithCheatStarted =
                    q
                    |> Seq.collect
                           (fun pos -> let ns = steps maze true pos |> Set.ofSeq
                                       cheatRouteCount maze ns (d + 1) (Set.add pos visited) 1 (Some pos) ce maxDist (ex, ey))
                    |> List.ofSeq
                List.append resultWithoutCheat resultWithCheatStarted
         | 1 -> let newQueue = Seq.collect (steps maze false) q
                              |> Seq.except visited
                newQueue
                |> Seq.collect (fun n ->
                    cheatRouteCount maze (Set.singleton n) (d + 1) (Set.union visited q) 0 cs (Some n) maxDist (ex, ey))
                |> List.ofSeq

// let cheatResults = cheatRouteCount maze (Set.singleton (sx, sy)) 0 Set.empty<int * int> 2 None None (minDist - 1) (ex, ey)
// let result1 = cheatResults
//               |> List.groupBy (fun (d, _, _) -> d)
//               |> List.map (fun (d, cs) -> (minDist - d, List.length cs))
//               |> List.sortBy fst

let cheatResults = cheatRouteCount maze (Set.singleton (sx, sy)) 0 Set.empty<int * int> 2 None None (minDist - 100) (ex, ey)
                   |> List.length
// let result1 = cheatResults
//               |> List.groupBy (fun (d, _, _) -> d)
//               |> List.map (fun (d, cs) -> (minDist - d, List.length cs))
//               |> List.sortBy fst