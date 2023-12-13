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

let getInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename)

// day 5 common
type AlamacMap = { Destinations: (int64*int64) list; Sources: (int64*int64) list }
type Alamac = { Seeds: int64 list; Maps: AlamacMap list }

let updateAlamacMap acc (alamacInput:int64 array) = 
    { acc with 
        Destinations = acc.Destinations@[alamacInput[0],(alamacInput[0] + alamacInput[2] - 1L)]; 
        Sources = acc.Sources@[alamacInput[1],(alamacInput[1] + alamacInput[2] - 1L)] }

let rec mapParser (partialInput:string array) (maps:AlamacMap list) =
    match partialInput with
    | [||] -> maps
    | _ ->
        let newMap = 
            partialInput 
            |> Array.takeWhile (fun s -> s <> "")
            |> Array.map (fun s -> s.Split(" ") |> Array.map int64)
            |> Array.fold updateAlamacMap { Destinations = []; Sources = [] }
        mapParser (partialInput |> Seq.skipWhile (fun s -> s <> "") |> Seq.skip 2 |> Seq.toArray) (maps@[newMap])

let containsSource (sources:(int64*int64) list) (index:int64) =
    sources |> Seq.exists (fun (x,y) -> x <= index && index <= y)

let findDestIndex mapping i = 
    mapping.Sources 
    |> Seq.findIndex (fun (low,up) -> low <= i && i <= up) 
    |> fun indx -> i - (fst mapping.Sources[indx]) + (fst mapping.Destinations[indx])

let findIndex (alamac:Alamac) (index:int64) =
    alamac.Maps 
    |> Seq.fold (fun (i:int64) mapping -> if (containsSource mapping.Sources i) then findDestIndex mapping i else i) index

let mapSeeds (alamac:Alamac) =
    alamac.Seeds
    |> PSeq.map (findIndex alamac)

// part 1

let parseInput (input:string array) =
    let seeds = input |> Seq.head |> fun s -> ((s.Split(": "))[1]).Split(" ") |> Seq.map int64 |> Seq.toList
    { Seeds = seeds; Maps = (mapParser (input |> Seq.skip 3 |> Seq.toArray) [])}

getInput 5
// getTestInput 5
|> parseInput
|> mapSeeds
|> Seq.min

// part 2
let parsePairSeedInput (input:string array) =
    let rec parsePairSeedInput result (input:int64 list) =
        match input with
        | [] -> result
        | _ -> parsePairSeedInput (result@[input |> List.take 2]) (input |> Seq.skip 2 |> Seq.toList)

    let seeds = 
        input 
        |> Seq.head |> fun s -> ((s.Split(": "))[1]).Split(" ") |> Seq.map int64 |> Seq.toList
        |> parsePairSeedInput []
        |> List.collect (fun [start;count]->[start .. start + count-1L])
    { Seeds = seeds; Maps = (mapParser (input |> Seq.skip 3 |> Seq.toArray) [])}
getInput 5
// getTestInput 5
|> parsePairSeedInput
|> mapSeeds
|> PSeq.min
