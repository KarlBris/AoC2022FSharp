namespace AoC2022

open Utils

module Day05 =

    let rec runInstructions
        (multipleStacksAtOnce: bool)
        (stacks: Map<int, char list>)
        (instructions: (int * int * int) list)
        : (Map<int, char list>) =
        match instructions with
        | [] -> stacks
        | (num, fromIndex, toIndex) :: restOfInstructions ->

            let subNum = if multipleStacksAtOnce then num else 1

            let (newFromStack, endOfToStack) =
                List.splitAt
                    ((Seq.length (stacks.Item(fromIndex - 1))) - subNum)
                    (Seq.toList (stacks.Item(fromIndex - 1)))

            let stacks' =
                stacks
                |> Map.change (fromIndex - 1) (fun _ -> Some newFromStack)
                |> Map.change (toIndex - 1) (fun s -> Some((Option.get s) @ endOfToStack))

            if num - 1 = 0 || multipleStacksAtOnce then
                runInstructions multipleStacksAtOnce stacks' restOfInstructions
            else
                runInstructions
                    multipleStacksAtOnce
                    stacks'
                    ((num - 1, fromIndex, toIndex)
                     :: restOfInstructions)

    let makeStacks stacks =
        stacks
        |> lines
        |> Seq.transpose
        |> Seq.filter (fun l -> (Seq.last l) <> ' ')
        |> Seq.map (
            Seq.rev
            >> Seq.takeWhile (fun c -> c <> ' ')
            >> Seq.tail
        )
        |> Seq.map Seq.toList
        |> Seq.toList
        |> List.indexed
        |> Map.ofList

    let makeInstrunctions instructions =
        instructions
        |> lines
        |> Seq.map (
            words
            >> (fun ws -> (int ws[1], int ws[3], int ws[5]))
        )
        |> Seq.toList

    let getAnswer stacks =
        stacks
        |> Map.values
        |> Seq.map (List.last)
        |> Seq.toArray
        |> System.String

    let part1 (input: string) : string =
        let (stacks, instructions) = input |> splitAtDoubleLines |> twoArrayToTuple

        let stacks' = makeStacks stacks

        instructions
        |> makeInstrunctions
        |> runInstructions false stacks'
        |> getAnswer

    let part2 (input: string) : string =
        let (stacks, instructions) = input |> splitAtDoubleLines |> twoArrayToTuple

        let stacks' = makeStacks stacks

        instructions
        |> makeInstrunctions
        |> runInstructions true stacks'
        |> getAnswer
