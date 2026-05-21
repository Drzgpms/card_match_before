module Types

//Card State
type Card = {
    Value : string
    Revealed : bool
    Matched : bool
}

type Difficulty =
    | Easy
    | Normal
    | Hard

type GameState = {
    Board : Card list
    Size : int
    Attempts : int
    Theme : string
}