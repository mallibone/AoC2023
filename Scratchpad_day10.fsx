#time
#r "nuget: FSharp.Collections.ParallelSeq, 1.2.0"
open System.IO
open System.Text.RegularExpressions
open System
open System.Linq
open System.Collections.Generic
open FSharp.Collections.ParallelSeq
open System.Collections.Concurrent

let getTestInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename)
    // File.ReadAllText(filename)

let getInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename)
    // File.ReadAllText(filename)

let parseInput (inputLines:string) =
    inputLines.ToCharArray() |> Array.map (fun c -> c.ToString())

type Coords = { X:int; Y:int}
type Map = {Map:string array array; Waypoints:Coords list list; Start:Coords; Steps:int}

let findStart (map:string array array) =
    let maxX = map[0] |> Array.length
    let maxY = map |> Array.length
    let startCoords =
        [|
            for y in 0..maxY-1 do
                for x in 0..maxX-1 do
                    if map[y][x] = "S" then
                        yield {X = x; Y =y}
        |] |> Array.head
    {Map = map; Waypoints = [[startCoords]]; Start = startCoords; Steps = 0}

type NeighbourCandidates = {Left:string list; Right:string list; Top:string list; Bottom:string list}
let validPipeCandidates =
    Map ["-", {Left = ["-";"F";"L"]; Right = ["-";"J";"7"]; Top = []; Bottom = []};
                    "|", {Left = []; Right = []; Top = ["|";"F";"7"]; Bottom = ["|";"J";"L"]};
                    "J", {Left = ["-";"F";"L"]; Right = []; Top = ["|";"F";"7"]; Bottom = []};
                    "L", {Left = []; Right = ["-";"J";"7"]; Top = ["|";"F";"7"]; Bottom = []};
                    "7", {Left = ["-";"F";"L"]; Right = []; Top = []; Bottom = ["|";"J";"L"]};
                    "F", {Left = []; Right = ["-";"J";"7"]; Top = []; Bottom = ["|";"J";"L"]};
                    "S", {Left = ["-";"F";"L"]; Right = ["-";"J";"7"]; Top = ["|";"F";"7"]; Bottom = ["|";"J";"L"]};]
                    // ".", {Left = []; Right = []; Top = []; Bottom = []}];

let nextWaypoints maxX maxY (map: string array array) (coords:Coords) =
    let currentPipe = map[coords.Y][coords.X]
    let validateLeft = coords.X - 1 >= 0 && validPipeCandidates[currentPipe].Left |> List.contains (map[coords.Y][coords.X - 1])
    let validateRight = (coords.X + 1 < maxX && (validPipeCandidates[currentPipe].Right |> List.contains (map[coords.Y][coords.X + 1])))
    let validateBottom = (coords.Y + 1 < maxY && (validPipeCandidates[currentPipe].Bottom |> List.contains (map[coords.Y + 1][coords.X])))
    let validateTop = (coords.Y - 1 >= 0 && (validPipeCandidates[currentPipe].Top |> List.contains (map[coords.Y - 1][coords.X])))

    let nextCoords =
        [
            if validateLeft then Some {X = coords.X - 1; Y = coords.Y} else None
            if validateRight then Some {X = coords.X + 1; Y = coords.Y} else None
            if validateTop then Some {X = coords.X; Y = coords.Y - 1} else None
            if validateBottom then Some {X = coords.X; Y = coords.Y + 1} else None
        ] |> List.choose id
    
    // printfn "Next coords: %A" nextCoords
    // printfn "Bottom candidates: %A %A %A" validPipeCandidates[currentPipe].Bottom (coords.Y + 1 < maxY)
    // printfn "Current pipe: %A; bottom %A" currentPipe (map[coords.Y + 1][coords.X])

    nextCoords

let rec findFurthestPoint map =
    // printfn "Map: %A" map
    let maxX = map.Map[0] |> Array.length
    let maxY = map.Map |> Array.length
    let nextWaypoints' = (nextWaypoints maxX maxY map.Map)

    let getWaypoints ((head:Coords), (prevCoords:Coords list)) = 
        // printfn "Head: %A, %A" head prevCoords
        (nextWaypoints' head) 
        |> List.filter(fun newCoords -> not (prevCoords |> Seq.contains newCoords))
        |> List.map (fun newCoords -> newCoords::prevCoords)

    let neighbourWaypoints = 
        map.Waypoints 
        |> List.map (fun x -> ((x |> List.head), x))
        |> List.collect getWaypoints 

    if neighbourWaypoints |> Seq.map (fun h -> h |> Seq.head) |> Seq.groupBy id |> Seq.exists (fun (k,v) -> v |> Seq.length > 1) then
        neighbourWaypoints, map.Steps + 1
    else
        findFurthestPoint {map with Waypoints = neighbourWaypoints; Steps = map.Steps + 1}

// part 1
getInput 10
// getTestInput 10
|> Array.map parseInput
|> findStart
|> findFurthestPoint
|> snd


// need to remember history of steps
// if we come back to a point we previously were -> dead end
// if two paths meet -> solution end point

// part 2
// getInput 10
// getTestInput 10

