open System
open System.IO
open System.Collections.Generic

let diskMap = File.ReadAllText "FSharp/09-disk-fragmenter-input.txt"
            |> Seq.map (string >> Int32.Parse)
            |> List.ofSeq

let disk = Array.create (diskMap |> List.sum) -1

let rec loadDisk diskMap disk isFile id i =
    match diskMap with
    | [] -> ()
    | h :: t -> Array.fill disk i h (if isFile then id else -1)
                loadDisk t disk (not isFile) (if isFile then (id + 1) else id) (i + h)

let printDisk disk =
    printfn "%s" (String.Join("", disk |> Array.map (fun i -> if i = -1 then "." else i.ToString())))

loadDisk diskMap disk true 0 0

let rec defrag (disk : int array) i1 i2 =
    if i1 > i2 then ()
    else if disk[i1] = -1 && disk[i2] <> -1
    then disk[i1] <- disk[i2]
         disk[i2] <- -1 
         defrag disk (i1 + 1) (i2 - 1)
    else if disk[i1] <> -1 then defrag disk (i1 + 1) i2
    else if disk[i2] = -1 then defrag disk i1 (i2 - 1)

defrag disk 0 (Array.length disk - 1)

let result1 = disk
              |> Seq.takeWhile (fun b -> b <> -1)
              |> Seq.indexed
              |> Seq.sumBy (fun (idx, id) -> (int64 idx) * (int64 id))

type Entry =
    | File of int * int
    | Free of int

let rec printEntries entries =
    match entries with
    | [] -> printfn ""
            ()
    | h :: t -> match h with
                | Free s -> printf "%s" (String.Join("", Seq.replicate s "."))
                | File (s, id) -> printf "%s" (String.Join("", Seq.replicate s (string id)))
                printEntries t

let entries = diskMap
              |> Seq.indexed
              |> Seq.map (fun (idx, n) -> if idx % 2 = 0
                                          then File (n, idx / 2)
                                          else Free n)
              |> LinkedList

let rec tryMoveLeft (entries : LinkedList<Entry>) (item : LinkedListNode<Entry>) (from : LinkedListNode<Entry>) =
    if item = from then item.Previous
    else let (File (fileSize, _)) = item.Value
         match from.Value with
         | Free s when s >= fileSize ->
            entries.AddBefore(from, item.Value) |> ignore
            if s > fileSize then entries.AddBefore(from, Free (s - fileSize)) |> ignore
            let newSpace = entries.AddBefore(item, Free fileSize)
            entries.Remove from
            entries.Remove item
            newSpace.Previous
         | _ -> tryMoveLeft entries item from.Next

let rec defrag2 (entries : LinkedList<Entry>) (item : LinkedListNode<Entry>) =
    if item = entries.First then ()
    else match item.Value with
         | Free _ -> defrag2 entries item.Previous
         | File _ -> let nextItem = tryMoveLeft entries item entries.First
                     defrag2 entries nextItem

defrag2 entries entries.Last

let rec checksum entries (idx : int32) (acc : int64) =
    match entries with
    | [] -> acc
    | h :: t -> match h with
                | Free s -> checksum t (idx + s) acc
                | File (s, id) -> let fileChecksum = [ idx..(idx + s - 1) ]
                                                     |> List.sumBy (fun i -> (int64 i) * (int64 id))
                                  checksum t (idx + s) (acc + (int64 fileChecksum))

let result2 = checksum (entries |> List.ofSeq) 0 0L