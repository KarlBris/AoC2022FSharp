namespace AoC2022

open Utils

module Day01 =
    let part1 (input: string) : string =
        input
        |> splitAtDoubleLines
        |> Array.map (lines >> (fun l -> Array.map int l) >> Array.sum)
        |> Array.sortDescending
        |> Array.head
        |> string

    let part2 (input: string) : string =
        input
        |> splitAtDoubleLines
        |> Array.map (lines >> (fun l -> Array.map int l) >> Array.sum)
        |> Array.sortDescending
        |> Array.take 3
        |> Array.sum
        |> string
