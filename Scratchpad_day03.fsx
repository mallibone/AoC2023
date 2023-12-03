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

// part 1

type NumberFinderState = { Numbers:list<string*int>; X:int;Y:int;Width:int;Height:int }

let rec readNumber (state:NumberFinderState) (grid:char array array) =
    let x = state.X
    let y = state.Y
    if x < state.Width && Char.IsDigit(grid[y][x]) then
        let character = grid[y][x]
        let neighbours = 
            [ if x > 0 && y > 0 then grid[y-1][x-1] else '.'// topleft
              if x > 0 then grid[y][x-1] else '.' // left
              if x > 0 && y < state.Height - 1 then grid[y+1][x-1] else '.' // bottomleft
              if x < state.Width-1 && y > 0 then grid[y-1][x+1] else '.' // topright
              if x < state.Width-1 then grid[y][x+1] else '.' // right
              if x < state.Width-1  && y < state.Height - 1 then grid[y+1][x+1] else '.' // bottomRight
              if y > 0 then grid[y-1][x] else '.' // top
              if y < (state.Height-1) then grid[y+1][x] else '.' // bottom
            ]

        let (digits, symbolCount) = state.Numbers |> Seq.head
        if neighbours |> Seq.exists (fun x -> (not (Char.IsDigit(x))) && x <> '.') then
            readNumber { state with Numbers = (($"{digits}{character}"), (symbolCount + 1))::(state.Numbers |> List.tail);X = x + 1 } grid
        else
            readNumber { state with Numbers = (($"{digits}{character}"), (symbolCount))::(state.Numbers |> List.tail); X = x + 1 } grid
    else
        state

let rec recNumberFinder state (grid:char array array) =
    match state.X, state.Y with
    | _, y when y >= state.Height -> state
    | x, y when x >= state.Width -> 
        recNumberFinder { state with X = 0; Y = y + 1 } grid
    | x, y when x < state.Width && y < state.Height -> 
        let character = grid[y][x]
        if Char.IsDigit(character) then
            let newState = readNumber {state with Numbers = ("",0)::state.Numbers} grid
            recNumberFinder { newState with X = newState.X + 1 } grid
        else
            recNumberFinder { state with X = x + 1 } grid
    | _ -> failwith "invalid state"

let getNumbers (grid:char array array) =
    let width = grid[0] |> Array.length
    let height = grid |> Array.length

    let finalState = recNumberFinder { Numbers = []; X = 0; Y = 0; Width = width; Height = height } grid
    finalState.Numbers

getInput 3
// getTestInput 3
|> Array.map Array.ofSeq // to grid
|> getNumbers
|> List.filter (fun (_,y) -> y > 0)
|> List.map (fst >> int)
|> List.sum

// part 2
type GearFinderState = { Gears:list<list<string>>; X:int;Y:int;Width:int;Height:int }

let rec parseNumber (grid:char array array) y x =
    let row = grid[y]
    let toTheRight = row |> Array.skip x |> Array.takeWhile Char.IsDigit |> String
    let toTheLeft = row |> Array.take x |> Array.rev |>Array.takeWhile Char.IsDigit |> Array.rev |> String
    let number = $"{toTheLeft}{toTheRight}"
    (number, y, x - toTheLeft.Length, x + toTheRight.Length)

let recFindGearNumbers state (grid:char array array) =
    let x = state.X
    let y = state.Y

    let neighbours = 
        [ if x > 0 && y > 0 then (y-1,x-1) else (-1,-1)// topleft
          if x > 0 then (y,x-1) else (-1,-1) // left
          if x > 0 && y < state.Height - 1 then (y+1,x-1) else (-1,-1) // bottomleft
          if x < state.Width-1 && y > 0 then (y-1,x+1) else (-1,-1) // topright
          if x < state.Width-1 then (y,x+1) else (-1,-1) // right
          if x < state.Width-1  && y < state.Height - 1 then (y+1,x+1) else (-1,-1) // bottomRight
          if y > 0 then (y-1,x) else (-1,-1) // top
          if y < (state.Height-1) then (y+1,x) else (-1,-1) // bottom
        ]
    
    let numericNeighbours = 
        neighbours |> List.filter (fun (y,x) -> y >= 0 && x >= 0 && Char.IsDigit(grid[y][x]))
        |> List.map (fun (y,x ) -> parseNumber grid y x)
        |> List.distinctBy (fun (_,y,x1,x2) -> y,x1,x2)
        |> List.filter (fun (number,_,_,_) -> number <> "")
        |> List.map (fun (number,_, _, _) -> number)
    
    {state with Gears = numericNeighbours::state.Gears}

let rec gearsFinder state (grid:char array array) =
    match state.X, state.Y with
    | _, y when y >= state.Height -> state
    | x, y when x >= state.Width -> 
        gearsFinder { state with X = 0; Y = y + 1 } grid
    | x, y when x < state.Width && y < state.Height -> 
        let character = grid[y][x]
        if character = '*' then
            let newState = recFindGearNumbers state grid
            gearsFinder { newState with X = newState.X + 1 } grid
        else
            gearsFinder { state with X = x + 1 } grid
    | _ -> failwith "invalid state"

let findGears (grid:char array array) =
    let width = grid[0] |> Array.length
    let height = grid |> Array.length

    let finalState = gearsFinder { Gears = []; X = 0; Y = 0; Width = width; Height = height } grid
    finalState.Gears

getInput 3
// getTestInput 3
|> Array.map Array.ofSeq // to grid
|> findGears
|> List.filter (fun x -> x.Length = 2)
|> List.map (fun x -> x |> List.map int64 |> List.fold (*) (1L))
|> List.sum
