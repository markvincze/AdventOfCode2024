open System
open System.IO

let lines = File.ReadAllLines "FSharp/15-warehouse-woes-input.txt"

let mapLines = lines |> Array.takeWhile (fun l -> not <| String.IsNullOrWhiteSpace l)

type Tile = Empty | Box | Wall

let tiles = Array2D.init
                mapLines[0].Length
                mapLines.Length
                (fun x y -> match mapLines[y][x] with
                            | '.' | '@' -> Empty
                            | 'O' -> Box
                            | '#' -> Wall
                            | _ -> failwith "Invalid input")

let (rx, ry) = seq { for x in 0..mapLines[0].Length - 1 do
                       for y in 0..mapLines.Length - 1 do
                           (x, y) }
               |> Seq.find (fun (x, y) -> mapLines[y][x] = '@')

type Dir = Up | Right | Down | Left

let steps = String.Join("", lines |> Array.skip (mapLines.Length + 1))
            |> Seq.map (fun c -> match c with
                                 | '<' -> Left
                                 | '^' -> Up
                                 | '>' -> Right
                                 | 'v' -> Down
                                 | _ -> failwith "Invalid input")
            |> List.ofSeq

let step dir (x, y) = match dir with
                      | Up -> (x, y - 1)
                      | Right -> (x + 1, y)
                      | Down -> (x, y + 1)
                      | Left -> (x - 1, y)

let rec pushBoxes dir (x, y) (tiles : Tile[,]) =
    match tiles[x, y] with
    | Box -> pushBoxes dir (step dir (x, y)) tiles
    | Empty -> tiles[x, y] <- Box
               true
    | Wall -> false

let move dir (x, y) (tiles : Tile[,]) =
    let nx, ny = step dir (x, y)
    match tiles[nx, ny] with
    | Empty -> (nx, ny)
    | Wall -> (x, y)
    | Box -> if pushBoxes dir (nx, ny) tiles
             then tiles[nx, ny] <- Empty
                  (nx, ny)
             else (x, y)

let print tiles (rx, ry) =
    for y in 0..Array2D.length2 tiles - 1 do
        for x in 0..Array2D.length1 tiles - 1 do
            let c = if (rx, ry) = (x, y) then '@'
                    else match tiles[x, y] with
                         | Empty -> '.'
                         | Wall -> '#'
                         | Box -> 'O'
            printf "%c" c
        printfn ""
    printfn ""

let rec executeSteps tiles (rx, ry) steps =
    // print tiles (rx, ry)
    match steps with
    | [] -> (rx, ry)
    | h :: t -> let nx, ny = move h (rx, ry) tiles
                executeSteps tiles (nx, ny) t

let fx, fy = executeSteps tiles (rx, ry) steps

// print tiles (fx, fy)

let result1 = seq { for x in 0..Array2D.length1 tiles - 1 do
                        for y in 0..Array2D.length2 tiles - 1 do
                            (x, y) }
              |> Seq.sumBy (fun (x, y) -> match tiles[x, y] with
                                          | Box -> y * 100 + x
                                          | _ -> 0)