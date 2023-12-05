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


// let (|Match|_|) (pat: string) (inp: string) =
//     let m = Regex.Match(inp, pat) in

//     if m.Success then
//         Some(List.tail [ for g in m.Groups -> g.Value ])
//     else
//         None

// let parseInput inputLine =
//     inputLine |> function
//         | Match "Blueprint (.*): Each ore robot costs (.*) ore. Each clay robot costs (.*) ore. Each obsidian robot costs (.*) ore and (.*) clay. Each geode robot costs (.*) ore and (.*) obsidian." [ blueprintId; oreRobotCost; clayRobotCost; obsidianRobotOreCost; obsidianRobotClayCost; geodeRobotOreCost; geodeRobotObsidianCost; ] ->
//             {Id = int blueprintId; OreRobotOreCost = int oreRobotCost; ClayRobotOreCost = int clayRobotCost; ObsidianRobotOreCost = int obsidianRobotOreCost; ObsidianRobotClayCost = int obsidianRobotClayCost; GeodeRobotObsidianCost = int geodeRobotObsidianCost; GeodeRobotOreCost = int geodeRobotOreCost}

// part 1
type Card = { WinningNumbers : int seq; Numbers : int seq }

let parseNumbers (input:string) = input.Split(" ") |> Array.filter (fun s -> s <> "") |> Array.map int
let parseGames (input:string) =
    input.Split(": ")[1] |> _.Split(" | ") |> fun x -> { WinningNumbers = x[0] |> parseNumbers; Numbers = x[1] |> parseNumbers }

let findWinningNumbers (card:Card) =
    card.WinningNumbers |> Seq.filter (fun n -> card.Numbers |> Seq.exists ((=) n)) |> Seq.length

getInput 4
// getTestInput 4
|> Array.map parseGames
|> Array.map findWinningNumbers
|> Array.map (fun x -> Math.Pow(2, float (x - 1)) |> int)
|> Array.sum

// part 2
let addOrUpdate (state:Dictionary<int,int>) indexvalue mult =
    if state.ContainsKey(indexvalue) then
        state[indexvalue] <- state[indexvalue] + mult
    else
        state.Add(indexvalue, mult)
let getCopies (state:Dictionary<int,int>) (index:int, card:Card) =
    addOrUpdate state index 1
    card.WinningNumbers 
    // |> Seq.mapi (fun i v -> i, v) 
    |> Seq.filter(fun  v -> card.Numbers |> Seq.exists ((=) v)) 
    // |> Seq.map (fun (i, _) -> i)
    |> Seq.iteri (fun i _ -> addOrUpdate state (index+i+1) state[index])

    state
    
getInput 4
// getTestInput 4
|> Array.map parseGames
|> Array.mapi (fun i v -> i, v)
|> Array.fold getCopies (new Dictionary<int,int>())
|> fun d -> d.Values
|> Seq.sum
