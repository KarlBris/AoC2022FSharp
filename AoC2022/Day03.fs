namespace AoC2022

open Utils

module Day03 =
    let valueList =
        List.zip ([ 'a' .. 'z' ] @ [ 'A' .. 'Z' ]) [ 1..52 ]
        |> Map.ofList

    let itemValue (c: char) : int = valueList[c]

    let findDuplicateItem ((l, r): string * string) : char =
        l
        |> Seq.filter (fun c -> r.Contains c)
        |> Seq.head

    let part1 (input: string) : string =
        input
        |> lines
        |> Array.map (
            (fun l -> Array.splitAt (l.Length / 2) (Seq.toArray l))
            >> (fun (l, r) -> System.String l, System.String r)
        )
        |> Array.map (findDuplicateItem >> itemValue)
        |> Array.sum
        |> string

    let findCommonItem (rucksacks: string []) : char =
        rucksacks
        |> Array.map (set)
        |> Set.intersectMany
        |> Set.toSeq
        |> Seq.head

    let part2 (input: string) : string =
        input
        |> lines
        |> Array.chunkBySize 3
        |> Array.map (findCommonItem >> itemValue)
        |> Array.sum
        |> string
