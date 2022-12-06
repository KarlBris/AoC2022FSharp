namespace AoC2022

open Utils

module Day06 =

    let rec isAllDifferent (target: int) (acc: int) (input: char array list) =
        match input with
        | [] -> (-1)
        | s :: ss ->
            if Set.count (Set s) = target then
                acc
            else
                isAllDifferent target (acc + 1) ss

    let runner (windowSize: int) (input: string) =
        input
        |> Seq.windowed windowSize
        |> Seq.toList
        |> isAllDifferent windowSize 0
        |> (+) windowSize
        |> string

    let part1 (input: string) : string = runner 4 input

    let part2 (input: string) : string = runner 14 input
