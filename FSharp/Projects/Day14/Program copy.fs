open System
open System.IO
open System.Text.RegularExpressions

type Robot = {
    Pos : int * int
    Vel : int * int
}

// p=0,4 v=3,-3
let parse line =
    let rm = (new Regex @"p=([\d-]+),([\d-]+) v=([\d-]+),([\d-]+)").Match line

    {
        Pos = Int32.Parse rm.Groups[1].Value, Int32.Parse rm.Groups[2].Value
        Vel = Int32.Parse rm.Groups[3].Value, Int32.Parse rm.Groups[4].Value
    }

let robots = File.ReadAllLines "14-restroom-redoubt-input.txt"
             |> Array.map parse

// let width = 11
// let height = 7

let width = 101
let height = 103

let afterN robot n =
    let (px, py) = robot.Pos
    let (vx, vy) = robot.Vel
    let x = (px + n * vx) % width
    let y = (py + n * vy) % height

    (if x >= 0 then x else (width + x)),
    (if y >= 0 then y else (height + y))

let r = { Pos = (2, 4); Vel = (2, -3) }

let mutable robotPs = robots
                      |> Array.map (fun r -> afterN r 100)
 
let safetyFactor ps =
    let rec safetyFactor ps q1 q2 q3 q4 =
        match ps with
        | [] -> q4 * q1 * q2 * q3
        | (x, y) :: t -> if x < width / 2 && y < height / 2 then safetyFactor t (q1 + 1) q2 q3 q4
                         else if x > width / 2 && y < height / 2 then safetyFactor t q1 (q2 + 1) q3 q4
                         else if x < width / 2 && y > height / 2 then safetyFactor t q1 q2 (q3 + 1) q4
                         else if x > width / 2 && y > height / 2 then safetyFactor t q1 q2 q3 (q4 + 1)
                         else safetyFactor t q1 q2 q3 q4 

    safetyFactor ps 0 0 0 0

let result1 = robotPs |> List.ofArray |> safetyFactor

let print ps =
    for y in 0..height - 1 do
        for x in 0..width - 1 do
            if ps |> Array.exists (fun (rx, ry) -> rx = x && ry = y)
            then printf "X"
            else printf "."
        printfn ""

Console.Clear()

// print robotPs

for i in 0..100 do
    Console.Clear()
    let ps = robots
             |> Array.map (fun r -> afterN r i)
    print robotPs
    Console.ReadKey |> ignore
