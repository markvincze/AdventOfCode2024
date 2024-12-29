open System
open System.IO

type Dir = Up | Right | Down | Left

let lines = File.ReadAllLines "FSharp/16-reindeer-maze-input.txt"

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

let clockwise = function
                | Up -> Right
                | Right -> Down
                | Down -> Left
                | Left -> Up

let counterClockwise = function
                       | Up -> Left
                       | Right -> Up
                       | Down -> Right
                       | Left -> Down

let move dir (x, y) = match dir with
                      | Up -> (x, y - 1)
                      | Right -> (x + 1, y)
                      | Down -> (x, y + 1)
                      | Left -> (x - 1, y)

let canMove maze (x, y) dir =
    let (x, y) = move dir (x, y)
    x >= 0 && y >= 0 && x < Array2D.length1 maze && y < Array2D.length2 maze && not maze[x, y]

let tryAddStep ((x, y), dir) score q d =
    match Map.tryFind ((x, y), dir) d with
    | None -> Set.add ((x, y), dir) q, Map.add ((x, y), dir) score d
    | Some x when x > score -> Set.add ((x, y), dir) q, Map.add ((x, y), dir) score d
    | _ -> q, d

let rec findDist (maze : bool[,]) q (d : Map<(int * int) * Dir, int>) (ex, ey) =
    let (x, y), dir = q |> Seq.minBy (fun n -> d[n])
    let score = d[(x, y), dir]
    if (x, y) = (ex, ey)
    then score
    else let q, d = if canMove maze (x, y) dir
                    then tryAddStep (move dir (x, y), dir) (score + 1) q d
                    else q, d
         let q, d = tryAddStep ((x, y), clockwise dir) (score + 1000) q d
         let q, d = tryAddStep ((x, y), counterClockwise dir) (score + 1000) q d
         findDist maze (Set.remove ((x, y), dir) q) d (ex, ey)

let q = Set.singleton ((sx, sy), Right)
let d = Map.empty<(int * int) * Dir, int> |> Map.add ((sx, sy), Right) 0

let result1 = findDist maze q d (ex, ey)

let tryAddStep2 ((x, y), dir) score visited q d =
    match Map.tryFind ((x, y), dir) d with
    | None -> Set.add ((x, y), dir, visited) q, Map.add ((x, y), dir) score d
    | Some s when s >= score -> Set.add ((x, y), dir, visited) q, Map.add ((x, y), dir) score d
    | _ -> q, d

let rec findDist2 (maze : bool[,]) q (d : Map<(int * int) * Dir, int>) (ex, ey) bestScore (visitedForBest : Set<int * int>) =
    let (x, y), dir, visited = q |> Seq.minBy (fun (pos, dir, _) -> d[(pos, dir)])
    let score = d[(x, y), dir]
    if Option.isSome bestScore && Option.get bestScore < score
    then printfn $"Score higher than best, bestScore: {bestScore}, score: {score}"
         visitedForBest
    else if (x, y) = (ex, ey)
         then printfn "Reached the end"
              findDist2 maze (Set.remove ((x, y), dir, visited) q) d (ex, ey) (Some score) (Set.union visitedForBest (Set.add (x, y) visited))
         else let newVisited = Set.add (x, y) visited
              let q, d = if canMove maze (x, y) dir
                         then tryAddStep2 (move dir (x, y), dir) (score + 1) newVisited q d
                         else q, d
              let q, d = tryAddStep2 ((x, y), clockwise dir) (score + 1000) newVisited q d
              let q, d = tryAddStep2 ((x, y), counterClockwise dir) (score + 1000) newVisited q d
              findDist2 maze (Set.remove ((x, y), dir, visited) q) d (ex, ey) bestScore newVisited

let q2 = Set.singleton ((sx, sy), Right, Set.empty<int * int>)
let d2 = Map.empty<(int * int) * Dir, int> |> Map.add ((sx, sy), Right) 0

let result2 = findDist2 maze q2 d2 (ex, ey) None Set.empty<int * int> |> Set.count

let print (maze : bool[,]) (visited : Set<int * int>) =
    for y in 0..lines.Length - 1 do
        for x in 0..lines[0].Length - 1 do
            if Set.contains (x, y) visited then printf "O"
            else if maze[x, y] then printf "#"
            else printf "."
        printfn ""
    printfn ""
