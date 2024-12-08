open System
open System.IO

let lines = File.ReadAllLines "FSharp/08-resonant-collinearity-input.txt"

let tiles = Array2D.init
                lines[0].Length
                lines.Length
                (fun x y -> lines[y][x])

let width = Array2D.length1 tiles
let height = Array2D.length2 tiles

let pos = seq { for x in 0..(Array2D.length1 tiles - 1) do 
                    for y in 0..(Array2D.length2 tiles - 1) do
                        yield (x, y) }
          |> Seq.filter (fun (x, y) -> tiles[x, y] <> '.')
          |> Seq.map (fun (x, y) -> tiles[x, y], (x, y))
          |> Seq.groupBy fst
          |> Seq.map (fun (c, ts) -> c, ts |> Seq.map snd |> List.ofSeq)
          |> Map.ofSeq

let rec allPairs items =
    match items with
    | [] | [ _ ] -> []
    | [ a; b ] -> [ (a, b) ]
    | h :: t ->  (t |> List.map (fun i -> h, i)) @ allPairs t

let inBounds width height (x, y) =
    x >= 0 && x < width && y >= 0 && y < height

let antinodes width height (x1, y1) (x2, y2) =
    let a1 = (2 * x2 - x1), (2 * y2 - y1)
    let a2 = (2 * x1 - x2), (2 * y1 - y2)
    [
        if inBounds width height a1 then yield a1
        if inBounds width height a2 then yield a2
    ]

let result1 = pos
              |> Map.values
              |> List.ofSeq
              |> List.collect (fun ts -> allPairs ts |> List.collect (fun (p1, p2) -> antinodes width height p1 p2))
              |> Set.ofList
              |> Set.count

let antinodes2 width height (x1, y1) (x2, y2) =
    let xd = x2 - x1
    let yd = y2 - y1
    let as1 = Seq.unfold
                  (fun (x, y) -> let nx, ny = x + xd, y + yd
                                 if inBounds width height (nx, ny) then Some ((nx, ny), (nx, ny))
                                 else None)
                  (x2, y2)
              |> Seq.append [ (x2, y2) ]

    let as2 = Seq.unfold
                  (fun (x, y) -> let nx, ny = x - xd, y - yd
                                 if inBounds width height (nx, ny) then Some ((nx, ny), (nx, ny))
                                 else None)
                  (x1, y1)
              |> Seq.append [ (x1, y1) ]

    Seq.append as1 as2
    |> List.ofSeq

let result2 = pos
              |> Map.values
              |> List.ofSeq
              |> List.collect (fun ts -> allPairs ts |> List.collect (fun (p1, p2) -> antinodes2 width height p1 p2))
              |> Set.ofList
              |> Set.count