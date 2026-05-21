module Board

open Types
open Utils
open Themes
open System

let getSize difficulty =
    match difficulty with
    | Easy -> 4
    | Normal -> 6
    | Hard -> 8

let getRandomTheme () =
    themes[rand.Next(themes.Length)]

let createBoard size =
    let pairCount = (size * size) / 2

    let (themeName, words) = getRandomTheme()

    let selectedWords =
        words
        |> List.take pairCount

    let cards =
        selectedWords
        |> List.collect (fun word ->
            [ { Value = word; Revealed = false; Matched = false }
              { Value = word; Revealed = false; Matched = false } ])
        |> shuffle

    (cards, themeName)

let printBoard (state : GameState) =
    Console.Clear()

    printfn "Current Theme: %s" state.Theme
    printfn "Attempts: %d" state.Attempts
    printfn ""

    for row in 0 .. state.Size - 1 do
        for col in 0 .. state.Size - 1 do
            let index = row * state.Size + col
            let card = state.Board[index]

            if card.Revealed || card.Matched then
                printf "| %-12s " card.Value
            else
                printf "| %-12d " (index + 1)
        printfn ""