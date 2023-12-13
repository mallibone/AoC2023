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



// part 1
let cardMap = 
    [
        ('A', 14L);
        ('K', 13L);
        ('Q', 12L);
        ('J', 11L);
        ('T', 10L);
        ('9', 9L);
        ('8', 8L);
        ('7', 7L);
        ('6', 6L);
        ('5', 5L);
        ('4', 4L);
        ('3', 3L);
        ('2', 2L);
        ('1', 1L)
    ] |> Map.ofSeq

let countOccurrences (cards: string) =
    let counter = Dictionary<char, int>()
    for card in cards do
        if counter.ContainsKey(card) then
            counter[card] <- counter[card] + 1
        else
            counter.Add(card, 1)
    counter.Values |> Seq.sortDescending |> Seq.toList

let getType (cards:string) =
    match (countOccurrences cards) with
    | [5] -> 6L
    | [4;_] -> 5L
    | [3;2] -> 4L
    | [3;1;1] -> 3L
    | [2;2;1] -> 2L
    | [2;1;1;1] -> 1L
    | [1;1;1;1;1] -> 0L
    | _ -> 0L

let parseCards (cards:string) : int64 =
    let handType = getType cards
    cards
    |> Seq.map (fun c -> cardMap[c])
    |> Seq.fold (fun hand card -> (hand * 100L + card)) handType

let parseHand (inputRow:string) =
    inputRow.Split(" ") |> (fun [|cards; bid|] -> (parseCards cards, int bid, cards))
// part 1
getInput 7
// getTestInput 7
|> Seq.map parseHand
|> Seq.sortBy (fun (hand, _, _) -> hand)
|> Seq.mapi (fun i (hand, bid, cards) ->  (i+1) * bid)
|> Seq.sum

// part 2
let cardMapII = 
    [
        ('A', 14L);
        ('K', 13L);
        ('Q', 12L);
        ('J', 1L);
        ('T', 10L);
        ('9', 9L);
        ('8', 8L);
        ('7', 7L);
        ('6', 6L);
        ('5', 5L);
        ('4', 4L);
        ('3', 3L);
        ('2', 2L);
        ('1', 1L)
    ] |> Map.ofSeq

let countOccurrencesWithJoker (cards: string) =
    let rec recOccurenceCounter (cards:char list) (jokerCount:int) (counter:Dictionary<char, int>) =
        match cards with
        | [] -> 
            match jokerCount with
            | 0 -> counter.Values |> Seq.sortDescending |> Seq.toList
            | 5 -> [5]
            | _ ->
                let highestCount = counter |> Seq.maxBy (fun kvp -> kvp.Value) |> fun kvp -> kvp.Key
                counter[highestCount] <- counter[highestCount] + jokerCount
                counter.Values |> Seq.sortDescending |> Seq.toList
        | card::rest ->
            if card = 'J' then
                recOccurenceCounter rest (jokerCount + 1) counter
            elif counter.ContainsKey(card) then
                counter[card] <- counter[card] + 1
                recOccurenceCounter rest jokerCount counter
            else
                counter.Add(card, 1)
                recOccurenceCounter rest jokerCount counter
    recOccurenceCounter (cards |> Seq.toList) 0 (Dictionary<char, int>())

let getTypeII occurences = 
    match occurences with
    | [5] -> 6L
    | [4;_] -> 5L
    | [3;2] -> 4L
    | [3;1;1] -> 3L
    | [2;2;1] -> 2L
    | [2;1;1;1] -> 1L
    | [1;1;1;1;1] -> 0L
    | _ -> 0L

let parseCardsII (cards:string) : int64 =
    let handType = getTypeII (countOccurrencesWithJoker cards)
    cards
    |> Seq.map (fun c -> cardMapII[c])
    |> Seq.fold (fun hand card -> (hand * 100L + card)) handType

let parseHandII (inputRow:string) =
    inputRow.Split(" ") |> (fun [|cards; bid|] -> (parseCardsII cards, int bid, cards))

getInput 7
// getTestInput 7
|> Seq.map parseHandII
|> Seq.sortBy (fun (hand, _, _) -> hand)
|> Seq.mapi (fun i (hand, bid, cards) ->  (i+1) * bid)
|> Seq.sum
