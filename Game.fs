module Game

open System
open System.Threading
open Types
open Board
open Utils

let revealCard board index =
    board
    |> List.mapi (fun i card ->
        if i = index then { card with Revealed = true }
        else card)

let hideCards board i1 i2 =
    board
    |> List.mapi (fun i card ->
        if i = i1 || i = i2 then { card with Revealed = false }
        else card)

let markMatched board i1 i2 =
    board
    |> List.mapi (fun i card ->
        if i = i1 || i = i2 then { card with Matched = true }
        else card)

let isFinished board =
    board |> List.forall (fun c -> c.Matched)

let rec getValidChoice (state : GameState) prompt =
    printf "%s" prompt

    let input =
        Console.ReadLine().Trim().ToLower()

    match input with
    | "q"
    | "quit"
    | "exit" ->
        printfn "Game terminated."
        Environment.Exit(0)
        0

    | _ ->
        match Int32.TryParse(input) with
        | false, _ ->
            printfn "Invalid input!"
            Thread.Sleep(700)

            printBoard state
            getValidChoice state prompt

        | true, value ->

            let choice = value - 1

            if choice < 0 || choice >= List.length state.Board then
                printfn "Invalid choice!"
                Thread.Sleep(700)

                printBoard state
                getValidChoice state prompt

            elif state.Board.[choice].Revealed || state.Board.[choice].Matched then
                printfn "Card already opened!"
                Thread.Sleep(700)

                printBoard state
                getValidChoice state prompt

            else
                choice

let rec gameLoop (state : GameState) =
    printBoard state

    if isFinished state.Board then
        printfn "Congratulations!"
        printfn "You've matched all cards in %d attempts!" state.Attempts
    else
        let first = getValidChoice state "Select first card: "

        let board1 = revealCard state.Board first
        let state1 = { state with Board = board1 }

        printBoard state1

        let second = getValidChoice state1 "Select second card: "

        let board2 = revealCard board1 second
        let state2 = 
            { 
                state with 
                    Board = board2; 
                    Attempts = state.Attempts + 1 
            }

        printBoard state2

        let card1 = board2.[first]
        let card2 = board2.[second]

        if card1.Value = card2.Value then
            printfn "Matched!"

            Thread.Sleep(1000)

            let matchedBoard = markMatched board2 first second

            gameLoop { state2 with Board = matchedBoard }
        else
            printfn "Not matched!"

            Thread.Sleep(1500)

            let hiddenBoard = hideCards board2 first second

            gameLoop { state2 with Board = hiddenBoard }