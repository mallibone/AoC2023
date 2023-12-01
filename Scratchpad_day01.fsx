#time
#r "nuget: FSharp.Collections.ParallelSeq, 1.2.0"
open System.IO
open System.Text.RegularExpressions
open System
open System.Linq
open System.Collections.Generic
open FSharp.Collections.ParallelSeq
open System.Collections.Concurrent

let getTestInput (day:string) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename)
    // File.ReadAllText(filename)

let getInput (day:string) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename)
    // File.ReadAllText(filename)

// part 1
getInput "1" 
// getTestInput "1a"
|> Seq.map (Seq.toList >> (List.filter Char.IsDigit) >> (fun l -> String.Join("", [(List.head l); (List.last l)])))
|> Seq.map int
|> Seq.sum

// part 2

// Map from words to numbers
let wordToNumberMap = 
    Map [
        "one", "1"; "two", "2"; "three", "3";
        "four", "4"; "five", "5"; "six", "6";
        "seven", "7"; "eight", "8"; "nine", "9";
        "1", "1"; "2", "2"; "3", "3";
        "4", "4"; "5", "5"; "6", "6";
        "7", "7"; "8", "8"; "9", "9"
    ]

let rec parseNumbers (state:list<string>) (input: string) =
    match input with
    | "" -> state |> List.rev
    | i ->
        let digit = wordToNumberMap |> Map.keys |> Seq.tryFind i.StartsWith

        match digit with
        | None -> parseNumbers state (i.Substring(1))
        | Some(d) -> parseNumbers (wordToNumberMap[d] :: state) (i.Substring(1))

getInput "1" 
|> Seq.map (parseNumbers [])
|> Seq.map (fun l -> String.Join("", [(List.head l); (List.last l)]))
|> Seq.map int
|> Seq.sum
