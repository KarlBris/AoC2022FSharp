namespace AoC2022

open Utils

module Day09 =

    type Coord = (int * int)

    type TailTrail = Coord Set

    type Rope = Coord list

    let getAllNeighbors (a: Coord) : Coord list =
        let (x, y) = a

        [ (x + 1, y)
          (x - 1, y)
          (x + 1, y + 1)
          (x - 1, y + 1)
          (x + 1, y - 1)
          (x - 1, y - 1)
          (x, y + 1)
          (x, y - 1) ]

    let isOnOrNeighbor (a: Coord) (b: Coord) : bool =
        a = b || List.contains b (getAllNeighbors a)

    let dist (a: Coord) (b: Coord) : float =
        sqrt (
            (float (pown ((fst a) - (fst b)) 2))
            + (float (pown ((snd a) - (snd b)) 2))
        )

    let walkInDir (dir: string) (pos: Coord) : Coord =
        let (x, y) = pos

        match dir with
        | "U" -> (x + 1, y)
        | "D" -> (x - 1, y)
        | "L" -> (x, y - 1)
        | "R" -> (x, y + 1)
        | _ -> failwith "Invalid direction"

    let rec ropeFollow (headPos: Coord) (headlessRope: Rope) : Rope =
        match headlessRope with
        | [] -> headPos :: headlessRope
        | headlessHead :: headlessTail ->
            if not (isOnOrNeighbor headPos headlessHead) then
                let newheadlessHeadPos =
                    headlessHead
                    |> getAllNeighbors
                    |> List.map (fun c -> (c, dist c headPos))
                    |> List.sortBy (fun (_, a) -> a)
                    |> List.head
                    |> fst

                headPos
                :: (ropeFollow newheadlessHeadPos headlessTail)
            else
                headPos :: headlessRope

    let rec walkSteps (rope: Rope) (trail: TailTrail) (instruction: (string * int)) =
        match instruction with
        | (_, 0) -> (rope, trail)
        | (dir, n) ->
            let headPos' = walkInDir dir (List.head rope)
            let rope' = ropeFollow headPos' (List.tail rope)

            walkSteps rope' (Set.add (List.last rope') trail) (dir, n - 1)

    let rec followInstructions (rope: Rope) (trail: TailTrail) (instructions: (string * int) list) : TailTrail =
        match instructions with
        | [] -> trail
        | i :: is ->
            let (rope', trail') = walkSteps rope trail i
            followInstructions rope' trail' is

    let simulateRope (ropeLength: int) (input: string) : string =
        input
        |> lines
        |> Array.map (words >> (fun ws -> (ws[0], int ws[1])))
        |> Array.toList
        |> followInstructions (List.replicate ropeLength (0, 0)) (Set.ofList [ (0, 0) ])
        |> Set.count
        |> string

    let part1 (input: string) : string = simulateRope 2 input

    let part2 (input: string) : string = simulateRope 10 input
