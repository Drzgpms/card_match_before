open Types
open Board
open Game
open System
open System.Threading

let rec selectDifficulty () =
    Console.Clear()
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
        printfn "Invalid difficulty."
        printfn ""
        Thread.Sleep(700)
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
