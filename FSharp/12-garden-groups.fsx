open System
open System.IO

let lines = File.ReadAllLines "FSharp/12-garden-groups-input.txt"

let garden = Array2D.init
                 lines[0].Length
                 lines.Length
                 (fun x y -> lines[y][x])

type Pos = int * int
type Region = char * Pos list

let width = Array2D.length1 garden
let height = Array2D.length2 garden

let neighbors (x, y) =
    [
        if x > 0 then yield (x - 1, y)
        if x < width - 1 then yield (x + 1, y)
        if y > 0 then yield (x, y - 1)
        if y < height - 1 then yield (x, y + 1)
    ]

let findRegion (garden: char[,]) (x, y) =
    let rec findRegion (garden: char[,]) queue region =
        match queue with
        | [] -> region
        | (x, y) :: t -> if (Set.contains (x, y) region) then findRegion garden t region
                         else let ns = neighbors (x, y)
                                       |> List.filter (fun (nx, ny) -> garden[nx, ny] = garden[x, y])
                              findRegion garden (List.append t ns) (Set.add (x, y) region)
    findRegion garden [(x, y)] Set.empty<int * int>

let findAllRegions (garden: char[,]) =
    seq {
        for x in 0..width-1 do
            for y in 0..height-1 do
                yield x,y }
    |> Seq.fold
        (fun regions (x, y) ->
            if List.exists (fun (c, ps) -> Set.contains (x, y) ps) regions then regions
            else (garden[x, y], findRegion garden (x, y)) :: regions)
        []

let regions = findAllRegions garden

let area region = region |> Set.count

let perimeter (garden : char[,]) region =
    region
    |> Seq.sumBy (fun (x, y) -> let plant = garden[x, y]
                                let mutable cnt = 0
                                if x = 0 || garden[x - 1, y] <> plant then cnt <- cnt + 1
                                if y = 0 || garden[x, y - 1] <> plant then cnt <- cnt + 1
                                if x = width - 1 || garden[x + 1, y] <> plant then cnt <- cnt + 1
                                if y = height - 1 || garden[x, y + 1] <> plant then cnt <- cnt + 1
                                cnt)

let price (garden : char[,]) region = area region * perimeter garden region

let result1 = regions
              |> List.map snd
              |> List.sumBy (price garden)

type EdgeDirection = Vertical | Horizontal

type EdgeSide = Left | Right | Top | Bottom

let rec edgeCount coords prev acc =
    match coords with
    | [] -> acc
    | h :: t -> edgeCount t h (if h - prev = 1 then acc else acc + 1)

let perimeterBulk (garden : char[,]) region =
    let edgePieces =
        region
        |> Seq.collect
            (fun (x, y) -> let plant = garden[x, y]
                           seq {
                               if x = 0 || garden[x - 1, y] <> plant then yield Vertical, Left, (x, y)
                               if y = 0 || garden[x, y - 1] <> plant then yield Horizontal, Top, (x, y)
                               if x = width - 1 || garden[x + 1, y] <> plant then yield Vertical, Right, (x + 1, y)
                               if y = height - 1 || garden[x, y + 1] <> plant then yield Horizontal, Bottom, (x, y + 1)
                           })
        |> List.ofSeq

    let horizontals = edgePieces
                      |> List.filter (fun (d, _, _) -> d = Horizontal)
                      |> List.map (fun (_, s, c) -> s, c)

    let groups = horizontals |> List.groupBy (fun (s, (_, y)) -> s, y) 

    let horizontalEdgeCount = groups
                              |> List.sumBy (fun (_, coords) -> let xs = coords |> List.map (snd >> fst) |> List.sort
                                                                edgeCount xs Int32.MinValue 0)

    let verticals = edgePieces
                    |> List.filter (fun (d, _, _) -> d = Vertical)
                    |> List.map (fun (_, s, c) -> s, c)

    let groups = verticals |> List.groupBy (fun (s, (x, _)) -> s, x)

    let verticalEdgeCount = groups
                            |> List.sumBy (fun (_, coords) -> let ys = coords |> List.map (snd >> snd) |> List.sort
                                                              edgeCount ys Int32.MinValue 0)

    horizontalEdgeCount + verticalEdgeCount

let priceBulk (garden : char[,]) region = area region * perimeterBulk garden region

let result2 = regions
              |> List.map snd
              |> List.sumBy (priceBulk garden)