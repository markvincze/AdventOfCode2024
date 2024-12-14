open System
open System.IO
open System.Text.RegularExpressions

type ClawMachine = {
    sx : int64
    sy : int64
    ax : int
    ay : int
    bx : int
    by : int
}

let parse (lines : string[]) = 
    let buttonRegex = Regex @"Button .: X\+(\d+), Y\+(\d+)"
    let matchA = buttonRegex.Match lines[0]
    let matchB = buttonRegex.Match lines[1]
    let matchPrize = (Regex @"Prize: X=(\d+), Y=(\d+)").Match lines[2]

    {
        // Part 1
        // sx = (matchPrize.Groups[1].Value |> Int64.Parse)
        // sy = (matchPrize.Groups[2].Value |> Int64.Parse)
        // Part 2
        sx = (matchPrize.Groups[1].Value |> Int64.Parse) + 10000000000000L
        sy = (matchPrize.Groups[2].Value |> Int64.Parse) + 10000000000000L
        ax = matchA.Groups[1].Value |> Int32.Parse
        ay = matchA.Groups[2].Value |> Int32.Parse
        bx = matchB.Groups[1].Value |> Int32.Parse
        by = matchB.Groups[2].Value |> Int32.Parse
    }

let lines = File.ReadAllLines "FSharp/13-claw-contraption-input.txt"

let machines = lines
               |> Array.chunkBySize 4
               |> Array.map parse

let sol (sx : int64) (sy : int64) ax bx ay by =
    let maxA = min (sx / ax) (sy / ay)
    let maxB = min (sx / bx) (sy / by)

    let sols = 
        seq {
            for a in 0L..maxA do
                let minB = max
                               ((sx - (a * ax)) / bx - 1L)
                               ((sy - (a * ay)) / by - 1L)
                let maxB = min
                               ((sx - (a * ax)) / bx + 1L)
                               ((sy - (a * ay)) / by + 1L)
                for b in minB..maxB do
                    (a, b)
        }
        |> Seq.filter (fun (a, b) -> a * ax + b * bx = sx && a * ay + b * by = sy)
        |> List.ofSeq

    if sols |> List.length > 1
    then failwith $"More than 1 solution. sx: {sx}, sy: {sy}, ax: {ax}, ay: {ay}, bx: {bx}, by: {by}"

    match sols with
    | [ s ] -> Some s
    | _ -> None


let sol2 (sx : int64) (sy : int64) ax bx ay by =
    let ap = (float sx - ((float sy * float bx) / float by)) / (float ax - ((float ay * float bx) / float by)) |> round |> int64
    let bp = (float sx - (float ap * float ax)) / float bx |> round |> int64

    if ap * ax + bp * bx = sx && ap * ay + bp * by = sy
    then Some (int64 ap, int64 bp)
    else None

let sols = machines
           |> Array.map (fun m -> sol2 m.sx m.sy m.ax m.bx m.ay m.by)

let result = sols
              |> Array.choose id
              |> Array.sumBy (fun (a, b) -> a * 3L + b)
