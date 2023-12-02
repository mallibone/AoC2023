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


type CubeCount = { Name:string; Count:int }
let ParseGames (input:string[]) =
    input
    |> Seq.map (fun line -> line.Split(": ") |> fun l -> [(int (l[0].Replace("Game ", ""))), l[1]] |> Map.ofList)

let ToCubeCount (input:string[]) =
    input |> Array.map (fun cubeString -> cubeString.Split(" ") |> (fun cubeStringParts -> { Name = cubeStringParts[1]; Count = int cubeStringParts[0] }))

let ParseSets (input:Map<int, string>) =
    let parsedSets = 
        input
        |> Map.values 
        |> Seq.head 
        |> (fun line -> line.Split("; ") |> Array.collect(fun set -> set.Split(", ") |> ToCubeCount) )
    [input.Keys.First(), parsedSets] |> Map.ofList

// part 1
let IsWithinLimit cube = 
    match cube.Name with
    | "red" -> cube.Count <= 12
    | "green" -> cube.Count <= 13
    | "blue" -> cube.Count <= 14
    | _ -> false

let AreAllSetsValid (input:Map<int, CubeCount[]>) =
    input
    |> Map.values
    |> Seq.forall (fun cubes-> cubes |> Seq.forall IsWithinLimit)

getInput 2
// getTestInput 2
|> ParseGames
|> Seq.map ParseSets
|> Seq.filter AreAllSetsValid
|> Seq.sumBy (fun set -> set |> Map.keys |> Seq.head)


// part 2
let GetMinCubesRequired (input:Map<int, CubeCount[]>) =
    let cubes = input |> Map.values |> Seq.head

    ["red"; "green"; "blue"]
    |> List.map (fun color -> cubes |> Seq.filter (fun cube -> cube.Name = color) |> Seq.maxBy (fun cube -> cube.Count))

getInput 2
// getTestInput 2
|> ParseGames
|> Seq.map ParseSets
|> Seq.map GetMinCubesRequired
|> Seq.map (fun cubes -> cubes |> Seq.map (fun cube -> cube.Count))
|> Seq.map (Seq.fold (*) 1)
|> Seq.sum
