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