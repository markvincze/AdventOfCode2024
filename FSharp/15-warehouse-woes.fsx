open System
open System.IO

let lines = File.ReadAllLines "FSharp/15-warehouse-woes-input.txt"

let mapLines = lines |> Array.takeWhile (fun l -> not <| String.IsNullOrWhiteSpace l)

type Tile = Empty | Box | Wall | BoxLeft | BoxRight

let print tiles (rx, ry) =
    for y in 0..Array2D.length2 tiles - 1 do
        for x in 0..Array2D.length1 tiles - 1 do
            let c = if (rx, ry) = (x, y) then '@'
                    else match tiles[x, y] with
                         | Empty -> '.'
                         | Wall -> '#'
                         | Box -> 'O'
                         | BoxLeft -> '['
                         | BoxRight -> ']'
            printf "%c" c
        printfn ""
    printfn ""

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

let rec executeSteps tiles (rx, ry) steps =
    // print tiles (rx, ry)
    match steps with
    | [] -> (rx, ry)
    | h :: t -> let nx, ny = move h (rx, ry) tiles
                executeSteps tiles (nx, ny) t

// let fx, fy = executeSteps tiles (rx, ry) steps

// // print tiles (fx, fy)

// let result1 = seq { for x in 0..Array2D.length1 tiles - 1 do
//                         for y in 0..Array2D.length2 tiles - 1 do
//                             (x, y) }
//               |> Seq.sumBy (fun (x, y) -> match tiles[x, y] with
//                                           | Box -> y * 100 + x
//                                           | _ -> 0)

// Part 2

let tiles2 = Array2D.init
                (mapLines[0].Length * 2)
                mapLines.Length
                (fun x y -> match mapLines[y][x / 2] with
                            | '.' | '@' -> Empty
                            | 'O' when x % 2 = 0 -> BoxLeft
                            | 'O' when x % 2 = 1 -> BoxRight
                            | '#' -> Wall
                            | _ -> failwith "Invalid input")

let (rx2, ry2) = (rx * 2, ry)

// print tiles2 (rx2, ry2)

let rec canPushBoxes dir (x, y) (tiles : Tile [,]) =
    // printfn "canPushBoxes called"
    match tiles[x, y] with
    | Empty -> true
    | Wall -> false
    | _ when dir = Left || dir = Right -> canPushBoxes dir ((x, y) |> step dir |> step dir) tiles
    | _ -> let nx, ny = step dir (x, y)
           let nx2, ny2 = match tiles[x, y] with
                          | BoxLeft -> (nx + 1, ny)
                          | BoxRight -> (nx - 1, ny)
                          | _ -> failwith "Invalid input"
           canPushBoxes dir (nx, ny) tiles &&
           canPushBoxes dir (nx2, ny2) tiles

let pushBoxes2 dir (x, y) (tiles : Tile [,]) =
    printfn "pushBoxes2 called"
    let rec pushBoxes2 dir (x, y) prev (tiles : Tile [,]) =
        let prevN = tiles[x, y]
        match dir with
        | Left | Right -> match prevN with
                          | BoxLeft | BoxRight -> tiles[x, y] <- prev
                                                  pushBoxes2 dir (step dir (x, y)) prevN tiles
                          | Empty -> tiles[x, y] <- prev
        | Up | Down -> match prevN with
                       | BoxLeft | BoxRight -> tiles[x, y] <- prev
                                               let nx, ny = step dir (x, y)
                                               let nx2, ny2 = match prevN with
                                                              | BoxLeft -> (nx + 1, ny)
                                                              | BoxRight -> (nx - 1, ny)
                                                              | _ -> failwith "Invalid input 1"
                                               pushBoxes2 dir (nx, ny) prevN tiles
                                               if tiles[nx, y] <> Empty
                                               then pushBoxes2 dir (nx2, ny2) Empty tiles
                       | Empty -> tiles[x, y] <- prev

    match dir with
    | Left | Right -> pushBoxes2 dir (x, y) Empty tiles
    | Up | Down -> let nx, ny = match tiles[x, y] with
                                | BoxLeft -> x + 1, y
                                | BoxRight -> x - 1, y
                                | _ -> failwith "Invalid input 2"
                   pushBoxes2 dir (x, y) Empty tiles
                   pushBoxes2 dir (nx, ny) Empty tiles

let move2 dir (x, y) (tiles : Tile[,]) =
    printfn "move2 called"
    let nx, ny = step dir (x, y)
    match tiles[nx, ny] with
    | Empty -> (nx, ny)
    | Wall -> (x, y)
    | BoxLeft | BoxRight -> if canPushBoxes dir (nx, ny) tiles
                            then pushBoxes2 dir (nx, ny) tiles
                                 (nx, ny)
                            else (x, y)

let rec executeSteps2 tiles (rx, ry) steps =
    match steps with
    | [] -> (rx, ry)
    | h :: t -> printfn $"executeStep2 called. Step {h}"
                print tiles (rx, ry)
                let nx, ny = move2 h (rx, ry) tiles
                executeSteps2 tiles (nx, ny) t

let fx2, fy2 = executeSteps2 tiles2 (rx2, ry2) (steps |> List.take 40)

// print tiles (fx, fy)

let result2 = seq { for x in 0..Array2D.length1 tiles2 - 1 do
                        for y in 0..Array2D.length2 tiles2 - 1 do
                            (x, y) }
              |> Seq.sumBy (fun (x, y) -> match tiles2[x, y] with
                                          | BoxLeft -> y * 100 + x
                                          | _ -> 0)
