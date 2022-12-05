namespace AoC2022

open Utils

module Day02 =

    let rock = 1
    let paper = 2
    let scissors = 3

    let win = 6
    let draw = 3
    let lose = 0

    let calculateScore ((l, r): string * string) : int =
        match r with
        | "X" ->
            rock
            + match l with
              | "A" -> draw
              | "B" -> lose
              | "C" -> win
        | "Y" ->
            paper
            + match l with
              | "A" -> win
              | "B" -> draw
              | "C" -> lose
        | "Z" ->
            scissors
            + match l with
              | "A" -> lose
              | "B" -> win
              | "C" -> draw

    let part1 (input: string) : string =
        input
        |> lines
        |> Array.map (words >> twoArrayToTuple >> calculateScore)
        |> Array.sum
        |> string

    let calculateNeededScore ((l, r): string * string) : int =
        match r with
        | "X" ->
            lose
            + match l with
              | "A" -> scissors
              | "B" -> rock
              | "C" -> paper
        | "Y" ->
            draw
            + match l with
              | "A" -> rock
              | "B" -> paper
              | "C" -> scissors
        | "Z" ->
            win
            + match l with
              | "A" -> paper
              | "B" -> scissors
              | "C" -> rock

    let part2 (input: string) : string =
        input
        |> lines
        |> Array.map (words >> twoArrayToTuple >> calculateNeededScore)
        |> Array.sum
        |> string
