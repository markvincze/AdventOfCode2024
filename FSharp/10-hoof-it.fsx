open System
open System.IO

let lines = File.ReadAllLines "FSharp/10-hoof-it-input.txt"

let tmap = Array2D.init
               lines[0].Length
               lines.Length
               (fun x y -> lines[y][x] |> string |> Int32.Parse)

let trailheads = [ for x in 0..Array2D.length1 tmap - 1 do 
                       for y in 0..Array2D.length2 tmap - 1 do
                           yield (x, y) ]
                 |> List.filter (fun (x, y) -> tmap[x, y] = 0)

let width = Array2D.length1 tmap
let height = Array2D.length2 tmap

let neighbors (x, y) =
    [
        if x > 0 then yield (x - 1, y)
        if x < width - 1 then yield (x + 1, y)
        if y > 0 then yield (x, y - 1)
        if y < height - 1 then yield (x, y + 1)
    ]

let score (tmap : int[,]) startPos =
    let rec score (tmap : int[,]) queue visited acc =
        match queue with
        | [] -> acc
        | (x, y) :: t -> if Set.contains (x, y) visited then score tmap t visited acc
                         else if tmap[x, y] = 9 then score tmap t (Set.add (x, y) visited) (acc + 1)
                         else let newQs = neighbors (x, y)
                                          |> List.filter (fun p -> not <| Set.contains p visited)
                                          |> List.filter (fun (xn, yn) -> tmap[xn, yn] = tmap[x, y] + 1)
                              score tmap (List.append t newQs) (Set.add (x, y) visited) acc

    score tmap [ startPos ] Set.empty<int * int> 0

let result1 = trailheads
              |> List.sumBy (fun p -> score tmap p)

let rating (tmap : int[,]) startPos =
    let rec rating (tmap : int[,]) queue acc =
        match queue with
        | [] -> acc
        | (x, y) :: t -> if tmap[x, y] = 9 then rating tmap t  (acc + 1)
                         else let newQs = neighbors (x, y)
                                          |> List.filter (fun (xn, yn) -> tmap[xn, yn] = tmap[x, y] + 1)
                              rating tmap (List.append t newQs) acc

    rating tmap [ startPos ] 0

let result2 = trailheads
              |> List.sumBy (fun p -> rating tmap p)