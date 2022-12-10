namespace AoC2022

open Utils

module Day10 =

    type StrengthMap = Map<int, int>

    let rec followInstructions (xReg: int) (cycle: int) (smap: StrengthMap) (inputs: string list) : StrengthMap =
        match inputs with
        | [] -> smap
        | i :: is ->
            let ws = words i

            let smap' = Map.add cycle xReg smap

            match ws[0] with
            | "noop" -> followInstructions xReg (cycle + 1) smap' is
            | _ ->
                let xReg' = xReg + (int ws[1])
                followInstructions xReg' (cycle + 2) (Map.add (cycle + 2) xReg' (Map.add (cycle + 1) xReg smap')) is

    let getMap (input: string) : StrengthMap =
        input
        |> lines
        |> Array.toList
        |> followInstructions 1 1 Map.empty

    let part1 (input: string) : string =
        let map = getMap input

        [ 20; 60; 100; 140; 180; 220 ]
        |> List.map (fun n -> map[n] * n)
        |> List.sum
        |> string

    let isSpriteVisible (spritePos: int) (scanLinePos: int) : bool = abs (spritePos - scanLinePos) <= 1

    let part2 (input: string) : string =
        let map = getMap input

        let line =
            [ 0..5 ]
            |> List.map (fun i ->
                let line =
                    [ 0..39 ]
                    |> List.map (fun n ->
                        let mempos = n + (i * 40)

                        if isSpriteVisible map[mempos + 1] n then
                            "█"
                        else
                            " ")
                    |> String.concat ""

                line)

        "\n" + (line |> String.concat "\n")
