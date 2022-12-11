namespace AoC2022

open Utils

module Day11 =

    type Monkey =
        { items: int list
          operation: int -> int
          test: int -> bool
          trueFalseTarget: int * int
          inspections: int }

    let parseMonkey (monkeyChunk: string) : Monkey =
        let inputWords = monkeyChunk |> lines |> Array.map words
       

        let operator =
            if inputWords[2][4] = "+" then
                (+)
            else
                (*)

        let items =
            inputWords[1][2..]
            |> String.concat ""
            |> commas
            |> Array.toList
            |> List.map int

        let operation =
            fun a ->
                let foo = inputWords[2][5]
                let var = if foo = "old" then a else int foo
                (operator a var)

        { items = items
          operation = operation
          test = (fun x -> eMod x (int (inputWords[3][3])) = 0)
          trueFalseTarget = (int (inputWords[4][5]), int (inputWords[5][5]))
          inspections = 0 }

    let addItemsToMonkey (monkey: Monkey) (items: int list) : Monkey =
        { monkey with items = (monkey.items @ items) }

    let rec monkeyRound (part1: bool) (monkeyTurn: int) (monkeys: Map<int, Monkey>) : Map<int, Monkey> =
        if monkeyTurn = Map.count monkeys then
            monkeys
        else
            let activeMonkey = monkeys[monkeyTurn]

            let monkeyTargets =
                activeMonkey.items
                |> List.fold
                    (fun state item ->
                        let newWorryLevel = activeMonkey.operation item

                        let calmedDownLevel =
                            if part1 then
                                newWorryLevel / 3
                            else
                                newWorryLevel

                        if activeMonkey.test calmedDownLevel then
                            Map.change
                                (fst activeMonkey.trueFalseTarget)
                                (fun list ->
                                    match list with
                                    | None -> Some [ calmedDownLevel ]
                                    | Some actualList -> Some((actualList) @ [ calmedDownLevel ]))
                                state
                        else
                            Map.change
                                (snd activeMonkey.trueFalseTarget)
                                (fun list ->
                                    match list with
                                    | None -> Some [ calmedDownLevel ]
                                    | Some actualList -> Some((actualList) @ [ calmedDownLevel ]))
                                state)
                    Map.empty

            // update active monkey to have no items and inspected += activeMonkey.items.length
            let monkeys' =
                Map.change
                    monkeyTurn
                    (fun monke ->
                        let monk = Option.get monke
                        let insps = monk.inspections

                        Some
                            { monk with
                                items = []
                                inspections = insps + activeMonkey.items.Length })
                    monkeys


            // update target monkeys to have items from monkeyTargets
            let monkeys'' =
                monkeyTargets
                |> Map.fold
                    (fun state k v -> Map.change k (fun monkey -> Some(addItemsToMonkey (Option.get monkey) v)) state)
                    monkeys'

            monkeyRound part1 (monkeyTurn + 1) monkeys''

    let rec letTheMonkeysPlay (part1: bool) (timesLeft: int) (monkeys: Map<int, Monkey>) : Map<int, Monkey> =
        if timesLeft = 0 then
            monkeys
        else
            letTheMonkeysPlay part1 (timesLeft - 1) (monkeyRound part1 0 monkeys)

    let part1 (input: string) : string =
        let monkeys =
            input
            |> splitAtDoubleLines
            |> Array.toList
            |> List.map parseMonkey
            |> List.indexed
            |> Map.ofList

        let monkeys' = letTheMonkeysPlay true 20 monkeys


        monkeys'
        |> Map.values
        |> Seq.map (fun (m: Monkey) -> m.inspections)
        |> Seq.sortDescending
        |> Seq.take 2
        |> Seq.fold (*) 1
        |> string
