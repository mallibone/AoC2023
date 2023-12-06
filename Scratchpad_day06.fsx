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

// common
type Race = {Duration:int64; WinningDistance:int64}

let findWinningButtonHolds race =
    [0L .. race.Duration]
    |> Seq.map (fun i -> i * (race.Duration-i))
    |> Seq.filter (fun i -> i > race.WinningDistance)
    |> Seq.length

// part 1
let parseInputPartI inputLine =
    let regex = new Regex(@"\d+")
    regex.Matches(inputLine).ToArray() |> Seq.map (fun (m:Match) -> m.Value) |> Seq.map int64 |> Seq.toArray

// getInput 6
getTestInput 6
|> Array.map parseInputPartI
|> fun(input) -> Array.zip input[0] input[1]
|> Array.map (fun (duration, distance) -> {Duration = duration; WinningDistance = distance})
|> Array.map findWinningButtonHolds
|> Array.fold (*) 1


// part 2
let parseInputPartII inputLine =
    let regex = new Regex(@"\d+")
    regex.Matches(inputLine).ToArray() |> Seq.map (fun (m:Match) -> m.Value) |> fun v -> String.Join("", v) |> int64

getInput 6
// getTestInput 6
|> Array.map parseInputPartII
|> fun i -> { Duration = i[0]; WinningDistance = i[1] }
|> findWinningButtonHolds

