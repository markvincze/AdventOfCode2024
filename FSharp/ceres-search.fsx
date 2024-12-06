open System
open System.IO

let lines = File.ReadAllLines "FSharp/ceres-search-input.txt"

let letters = Array2D.init
                  (lines.[0].Length)
                  lines.Length
                  (fun x y -> lines[y][x])

let width = Array2D.length1 letters
let height = Array2D.length2 letters

let xmasCount x y (letters : char[,]) width height =
    [
        (x < width - 3 && y >= 3 &&
            ((letters[x, y] = 'X' && letters[x+1, y-1] = 'M' && letters[x+2, y-2] = 'A' && letters[x+3, y-3] = 'S') ||
            (letters[x, y] = 'S' && letters[x+1, y-1] = 'A' && letters[x+2, y-2] = 'M' && letters[x+3, y-3] = 'X')))
        (x < width - 3 &&
            ((letters[x, y] = 'X' && letters[x+1, y] = 'M' && letters[x+2, y] = 'A' && letters[x+3, y] = 'S') ||
            (letters[x, y] = 'S' && letters[x+1, y] = 'A' && letters[x+2, y] = 'M' && letters[x+3, y] = 'X')))
        (x < width - 3 && y < height - 3 &&
            ((letters[x, y] = 'X' && letters[x+1, y+1] = 'M' && letters[x+2, y+2] = 'A' && letters[x+3, y+3] = 'S') ||
            (letters[x, y] = 'S' && letters[x+1, y+1] = 'A' && letters[x+2, y+2] = 'M' && letters[x+3, y+3] = 'X')))
        (y < height - 3 &&
            ((letters[x, y] = 'X' && letters[x, y+1] = 'M' && letters[x, y+2] = 'A' && letters[x, y+3] = 'S') ||
            (letters[x, y] = 'S' && letters[x, y+1] = 'A' && letters[x, y+2] = 'M' && letters[x, y+3] = 'X')))
    ] |> List.filter id |> List.length

let result1 = seq {
                  for x in 0..width-1 do
                      for y in 0..height-1 do
                          yield x,y }
              |> Seq.sumBy (fun (x, y) -> xmasCount x y letters width height)

let xmasCount2 x y (letters : char[,]) width height =
    if
        x < width - 2 && y < height - 2 && letters[x+1, y+1] = 'A' &&
        ((letters[x, y] = 'M' && letters[x+2, y+2] = 'S') || (letters[x, y] = 'S' && letters[x+2, y+2] = 'M')) &&
        ((letters[x, y+2] = 'M' && letters[x+2, y] = 'S') || (letters[x, y+2] = 'S' && letters[x+2, y] = 'M'))
    then 1
    else 0

let result2 = seq {
                  for x in 0..width-1 do
                      for y in 0..height-1 do
                          yield x,y }
              |> Seq.sumBy (fun (x, y) -> xmasCount2 x y letters width height)