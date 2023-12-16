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


let parseInput (inputLine:string) =
    inputLine.Split(" ") |> Array.map int64

    



// part 1
let predictNextNumber series =
    let rec predictNextNumber' (tailNumbers:int64 list) (series: int64 seq) =
        let diffs = series |> Seq.windowed 2 |> Seq.map (fun x -> x.[1] - x.[0])
        if (diffs |> Seq.forall (fun d -> d = 0L)) then
            tailNumbers |> List.fold (fun state n -> (state + n)) (series |> Seq.last)
        else
            predictNextNumber' ((series |> Seq.last) :: tailNumbers) diffs
    predictNextNumber' [] series

getInput 9
// getTestInput 9
|> Array.map parseInput
|> Array.map predictNextNumber
|> Seq.sum

// part 2
let predictPrevNumber series =
    let rec predictNextNumber' (headNumbers:int64 list) (series: int64 seq) =
        let diffs = series |> Seq.windowed 2 |> Seq.map (fun x -> x.[1] - x.[0])
        if (diffs |> Seq.forall (fun d -> d = 0L)) then
            headNumbers |> List.fold (fun state n -> (n - state)) (series |> Seq.head)
        else
            predictNextNumber' ((series |> Seq.head) :: headNumbers) diffs
    predictNextNumber' [] series

getInput 9
// getTestInput 9
|> Array.map parseInput
|> Array.map predictPrevNumber
|> Seq.sum

