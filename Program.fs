open Types
open Board
open Game
open System

let rec selectDifficulty () =
    printfn "E / Easy   -> 4x4"
    printfn "N / Normal -> 6x6"
    printfn "H / Hard   -> 8x8"
    printf "Select difficulty: "

    let input =
        System.Console.ReadLine().Trim().ToLower()

    match input with
    | "e"
    | "easy" ->
        Easy

    | "n"
    | "normal" ->
        Normal

    | "h"
    | "hard" ->
        Hard

    | _ ->
        Console.Clear()
        printfn "Invalid difficulty."
        printfn ""
        selectDifficulty ()

[<EntryPoint>]
let main argv =

    let difficulty = selectDifficulty ()

    let size = getSize difficulty

    let board, theme = createBoard size

    let initialState =
        {
            Board = board
            Size = size
            Attempts = 0
            Theme = theme
            SeenCards = Map.empty
        }

    gameLoop initialState

    0
