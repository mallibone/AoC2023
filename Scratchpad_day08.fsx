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


let (|Match|_|) (pat: string) (inp: string) =
    let m = Regex.Match(inp, pat) in

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let parseNodes inputLine =
    inputLine |> function
        | Match "([A-Z]{3}) = \(([A-Z]{3}), ([A-Z]{3})\)" [ node; path1; path2] -> (node, [|path1; path2|])

let parseInput (input:string array) =
    let path = input |> Array.head |> Seq.map (fun c -> if c = 'L' then 0 else 1) |> Seq.toArray
    let nodeMap = input |> Array.skip 2 |> Array.map parseNodes |> Map.ofArray
    (path, nodeMap)

let findGoal ((steps:int array), (nodeMap:Map<string,string array>)) = 
    let rec findGoalRec totalCount currentNode count (steps:int array) (nodeMap:Map<string,string array>) =
        printfn "currentNode: %s, count: %d" currentNode count
        match currentNode with
        | "ZZZ" -> totalCount
        | _ -> 
            let nextNode = (nodeMap[currentNode])[steps[count]]
            // let nextNode = if paths.[0] = "ZZZ" then paths.[1] else paths.[0]
            let nextCount = (count + 1) % (steps.Length)
            findGoalRec (totalCount+1) nextNode nextCount steps nodeMap
    findGoalRec 0 "AAA" 0 steps nodeMap


// part 1
// getInput 8
getTestInput 8
|> parseInput
|> findGoal "AAA" 0

// part 2
// getInput 8
// getTestInput 8

