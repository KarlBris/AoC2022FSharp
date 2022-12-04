namespace AoC2022

open Utils

module Day04 =

    let hasOverlap (checkAll: bool) ((left, right): string * string) : bool =
        let (l1, l2) =
            left
            |> hyphens
            |> Array.map int
            |> twoArrayToTuple

        let (r1, r2) =
            right
            |> hyphens
            |> Array.map int
            |> twoArrayToTuple

        let leftList = [ l1..l2 ]
        let rightList = [ r1..r2 ]

        let checkFunction =
            if checkAll then
                List.forall
            else
                List.exists

        let leftContainsRight =
            rightList
            |> checkFunction (fun e -> List.contains e leftList)

        let rightContainsLeft =
            leftList
            |> checkFunction (fun e -> List.contains e rightList)

        leftContainsRight || rightContainsLeft

    let part1 (input: string) : string =
        input
        |> lines
        |> Array.map (commas >> twoArrayToTuple)
        |> Array.filter (hasOverlap true)
        |> Array.length
        |> string

    let part2 (input: string) : string =
        input
        |> lines
        |> Array.map (commas >> twoArrayToTuple)
        |> Array.filter (hasOverlap false)
        |> Array.length
        |> string
