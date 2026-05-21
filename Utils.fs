module Utils

open System

let rand = Random()

// List Shuffle
let shuffle list =
    list
    |> List.sortBy (fun _ -> rand.Next())