module AI

open System
open System.Threading
open Types
open Board
open Logic

// 카드 기억
let rememberCard (state : GameState) index =

    let card = state.Board[index]

    let updatedSet =
        match state.SeenCards.TryFind(card.Value) with
        | Some s -> s.Add(index)
        | None -> Set.singleton index

    {
        state with
            SeenCards =
                state.SeenCards.Add(card.Value, updatedSet)
    }

// 이미 알고 있는 pair 찾기
let findKnownPair (state : GameState) =

    state.SeenCards
    |> Map.tryPick (fun _ indices ->

        let valid =
            indices
            |> Set.toList
            |> List.filter (fun i ->
                not state.Board[i].Matched)

        match valid with
        | a :: b :: _ when a <> b ->
            Some (a, b)

        | _ ->
            None
    )

// 아직 한번도 안 본 카드 찾기
let findUnknownCard (state : GameState) =

    state.Board
    |> List.indexed
    |> List.tryFind (fun (i, card) ->

        not card.Matched &&

        not (
            state.SeenCards
            |> Map.exists (fun _ indices ->
                Set.contains i indices)
        )
    )

// 첫 카드가 이미 열린 상태에서 AI 이어하기
let rec aiContinueFromFirstCard (state : GameState) firstIndex =

    printBoard state

    let firstCard =
        state.Board[firstIndex]

    match state.SeenCards.TryFind(firstCard.Value) with

    // 이미 짝 위치를 알고 있으면 바로 매칭
    | Some indices ->

        let possible =
            indices
            |> Set.toList
            |> List.filter (fun i ->
                i <> firstIndex &&
                not state.Board[i].Matched)

        match possible with

        | secondIndex :: _ ->

            let board2 =
                revealCard state.Board secondIndex

            let state2 =
                {
                    state with
                        Board = board2
                        Attempts = state.Attempts + 1
                }

            printBoard state2
            Thread.Sleep(700)

            let matchedBoard =
                markMatched board2 firstIndex secondIndex

            aiPlay { state2 with Board = matchedBoard }

        | [] ->

            // 새로운 카드 탐색
            match findUnknownCard state with

            | None ->

                aiPlay state

            | Some (secondIndex, _) ->

                let board2 =
                    revealCard state.Board secondIndex

                let tempState =
                    {
                        state with
                            Board = board2
                            Attempts = state.Attempts + 1
                    }

                let state2 =
                    rememberCard tempState secondIndex

                printBoard state2
                Thread.Sleep(700)

                let secondCard =
                    board2.[secondIndex]

                if firstCard.Value = secondCard.Value then

                    let matchedBoard =
                        markMatched board2 firstIndex secondIndex

                    aiPlay { state2 with Board = matchedBoard }

                else

                    Thread.Sleep(700)

                    let hiddenBoard =
                        hideCards board2 firstIndex secondIndex

                    aiPlay { state2 with Board = hiddenBoard }

    | None ->

        aiPlay state

// AI 플레이
and aiPlay (state : GameState) =

    printBoard state

    if isFinished state.Board then

        printfn "AI finished the game!"
        printfn "Attempts: %d" state.Attempts

    else

        // 1. 이미 알고 있는 pair 있으면 즉시 매칭
        match findKnownPair state with

        | Some (i1, i2) ->

            let board1 =
                revealCard state.Board i1

            let state1 =
                { state with Board = board1 }

            printBoard state1
            Thread.Sleep(500)

            let board2 =
                revealCard board1 i2

            let state2 =
                {
                    state1 with
                        Board = board2
                        Attempts = state.Attempts + 1
                }

            printBoard state2
            Thread.Sleep(700)

            let matchedBoard =
                markMatched board2 i1 i2

            aiPlay { state2 with Board = matchedBoard }

        // 2. 없으면 새로운 카드 탐색
        | None ->

            match findUnknownCard state with

            | None ->

                printfn "No unknown cards left."
                Thread.Sleep(1000)

                aiPlay state

            | Some (firstIndex, _) ->

                // 첫 번째 카드 공개
                let board1 =
                    revealCard state.Board firstIndex

                let tempState =
                    { state with Board = board1 }

                let state1 =
                    rememberCard tempState firstIndex

                printBoard state1
                Thread.Sleep(700)

                let firstCard =
                    state1.Board[firstIndex]

                // 3. 첫 카드의 pair를 이미 알고 있으면 즉시 매칭
                match state1.SeenCards.TryFind(firstCard.Value) with

                | Some indices ->

                    let possible =
                        indices
                        |> Set.toList
                        |> List.filter (fun i ->
                            i <> firstIndex &&
                            not state1.Board[i].Matched)

                    match possible with

                    | secondIndex :: _ ->

                        let board2 =
                            revealCard board1 secondIndex

                        let state2 =
                            {
                                state1 with
                                    Board = board2
                                    Attempts = state.Attempts + 1
                            }

                        printBoard state2
                        Thread.Sleep(700)

                        let matchedBoard =
                            markMatched board2 firstIndex secondIndex

                        aiPlay { state2 with Board = matchedBoard }

                    // 4. 없으면 또 새로운 카드 탐색
                    | [] ->

                        match findUnknownCard state1 with

                        | None ->

                            aiPlay state1

                        | Some (secondIndex, _) ->

                            let board2 =
                                revealCard board1 secondIndex

                            let temp2 =
                                {
                                    state1 with
                                        Board = board2
                                        Attempts = state.Attempts + 1
                                }

                            let state2 =
                                rememberCard temp2 secondIndex

                            printBoard state2
                            Thread.Sleep(700)

                            let card1 = board2[firstIndex]
                            let card2 = board2[secondIndex]

                            if card1.Value = card2.Value then

                                let matchedBoard =
                                    markMatched board2 firstIndex secondIndex

                                aiPlay { state2 with Board = matchedBoard }

                            else

                                Thread.Sleep(700)

                                let hiddenBoard =
                                    hideCards board2 firstIndex secondIndex

                                aiPlay { state2 with Board = hiddenBoard }

                | None ->

                    aiPlay state1