namespace AoC2022

open Utils

module Day01 =
    let part1 (input: string) : string =
        input
        |> splitAtDoubleLines
        |> Array.map (lines >> Array.map int >> Array.sum)
        |> Array.sortDescending
        |> Array.head
        |> string

    let part2 (input: string) : string =
        input
        |> splitAtDoubleLines
        |> Array.map (lines >> Array.map int >> Array.sum)
        |> Array.sortDescending
        |> Array.take 3
        |> Array.sum
        |> string
