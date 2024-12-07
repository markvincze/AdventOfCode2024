open System
open System.IO

let lines = File.ReadAllLines "FSharp/guard-galliant-input.txt"

let tiles = Array2D.init
                lines[0].Length
                lines.Length
                (fun x y -> lines[y][x] = '#')

let pos = seq { for x in 0..(lines[0].Length - 1) do 
                    for y in 0..(lines.Length - 1) do
                        yield (x, y)}
          |> Seq.find (fun (x, y) -> lines[y][x] = '^')

type Dir = Up | Right | Down | Left

let turnRight = function
                | Up -> Right
                | Right -> Down
                | Down -> Left
                | Left -> Up

let nextPos (x, y) = function
                     | Up -> (x, y - 1)
                     | Right -> (x + 1, y)
                     | Down -> (x, y + 1)
                     | Left -> (x - 1, y)

let rec moveUntilOut tiles (x, y) dir visited =
    if x < 0 || x >= Array2D.length1 tiles || y < 0 || y >= Array2D.length2 tiles
    then visited
    else let nextX, nextY = nextPos (x, y) dir
         if nextX >= 0 && nextX < Array2D.length1 tiles && nextY >= 0 && nextY < Array2D.length2 tiles && tiles[nextX, nextY]
         then moveUntilOut tiles (x, y) (turnRight dir) (Set.add (x, y) visited)
         else moveUntilOut tiles (nextX, nextY) dir (Set.add (x, y) visited)

let result1 = moveUntilOut tiles pos Up Set.empty<int*int>
              |> Set.count

let endsInLoop tiles (x, y) dir =
    let rec endsInLoop tiles (x, y) dir visited =
        if Set.contains ((x, y), dir) visited
        then true
        else if x < 0 || x >= Array2D.length1 tiles || y < 0 || y >= Array2D.length2 tiles
        then false
        else let nextX, nextY = nextPos (x, y) dir
             if nextX >= 0 && nextX < Array2D.length1 tiles && nextY >= 0 && nextY < Array2D.length2 tiles && tiles[nextX, nextY]
             then endsInLoop tiles (x, y) (turnRight dir) (Set.add ((x, y), dir) visited)
             else endsInLoop tiles (nextX, nextY) dir (Set.add ((x, y), dir) visited)

    endsInLoop tiles (x, y) dir Set.empty<(int * int) * Dir>

let countObstructions tiles (x, y) dir =
    let rec countObstructions tiles (origX, origY) (x, y) dir visited cnt =
        let nextX, nextY = nextPos (x, y) dir
        if nextX < 0 || nextX >= Array2D.length1 tiles || nextY < 0 || nextY >= Array2D.length2 tiles
        then cnt
        else if tiles[nextX, nextY]
        then countObstructions tiles (origX, origY) (x, y) (turnRight dir) (Set.add (x, y) visited) cnt
        else if (not <| Set.contains (nextX, nextY) visited)
        then tiles[nextX, nextY] <- true
             let loops = endsInLoop tiles (origX, origY) Up 
             tiles[nextX, nextY] <- false
             countObstructions tiles (origX, origY) (nextX, nextY) dir (Set.add (x, y) visited) (if loops then cnt + 1 else cnt)
        else 
             countObstructions tiles (origX, origY) (nextX, nextY) dir (Set.add (x, y) visited) cnt

    countObstructions tiles (x, y) (x, y) dir Set.empty<int*int> 0

let result2 = countObstructions tiles pos Up
