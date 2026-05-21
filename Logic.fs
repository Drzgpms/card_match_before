module Logic

open Types

let revealCard board index =
    board
    |> List.mapi (fun i card ->
        if i = index then { card with Revealed = true }
        else card)

let hideCards board i1 i2 =
    board
    |> List.mapi (fun i card ->
        if i = i1 || i = i2 then
            { card with Revealed = false }
        else
            card)

let markMatched board i1 i2 =
    board
    |> List.mapi (fun i card ->
        if i = i1 || i = i2 then
            { card with Matched = true }
        else
            card)

let isFinished board =
    board |> List.forall (fun c -> c.Matched)