namespace AoC2022

open Utils

module Day14 =
    type bounds =
        { Top: int
          Bottom: int
          Left: int
          Right: int }

    type position = int * int

    type worldState = Map<position, char>
    let nullState: worldState = Map.empty
    let initialGrainPos = (500, 0)

    let makePositionsBetween (((startX, startY), (stopX, stopY)): position * position) : position array =
        if startX = stopX then
            let startVal = min startY stopY
            let stopVal = max startY stopY

            [| startVal..stopVal |]
            |> Array.map (fun v -> (startX, v))
        elif startY = stopY then
            let startVal = min startX stopX
            let stopVal = max startX stopX

            [| startVal..stopVal |]
            |> Array.map (fun v -> (v, startY))
        else
            failwith "illegal position pair"

    let findBounds (input: position array array) : bounds =
        let lrList = input |> Array.concat |> Array.map fst
        let udList = input |> Array.concat |> Array.map snd

        { Top = 0
          Bottom = udList |> Array.max
          Left = lrList |> Array.min
          Right = lrList |> Array.max }

    let addLine (startAndStop: (position * position)) (state: worldState) : worldState =
        let positionsToAdd = makePositionsBetween startAndStop

        positionsToAdd
        |> Array.fold (fun s pos -> Map.add pos '#' s) state

    let addPath (path: position array) (state: worldState) : worldState =
        path
        |> Array.windowed 2
        |> Array.map twoArrayToTuple
        |> Array.fold (fun s line -> addLine line s) state

    let addPaths (paths: position array array) : worldState =
        Array.fold (fun s path -> addPath path s) nullState paths

    let moveDown ((x, y): position) : position = (x, y + 1)

    let moveDownLeft ((x, y): position) : position = (x - 1, y + 1)

    let moveDownRight ((x, y): position) : position = (x + 1, y + 1)

    let isOutOfBounds ((x, y): position) (bounds: bounds) : bool =
        x < bounds.Left
        || x > bounds.Right
        || y < 0
        || y > bounds.Bottom

    let fallGrain (pos: position) (world: worldState) : position =
        if Map.containsKey (moveDown pos) world |> not then
            moveDown pos
        elif Map.containsKey (moveDownLeft pos) world |> not then
            moveDownLeft pos
        elif Map.containsKey (moveDownRight pos) world |> not then
            moveDownRight pos
        else
            pos

    let rec moveGrain (pos: position) (world: worldState) (bounds: bounds) : (worldState * bool) =
        let newPos = fallGrain pos world

        if isOutOfBounds newPos bounds then
            (world, true)
        else if newPos = pos then
            (Map.add pos 'o' world, false)
        else
            moveGrain newPos world bounds

    let rec dropGrain (world: worldState) (bounds: bounds) : worldState =
        let (newWorld, finished) = moveGrain initialGrainPos world bounds

        if finished then
            newWorld
        else
            dropGrain newWorld bounds

    let rec moveGrainWithFloor ((x, y) as pos: position) (world: worldState) (floor: int) : (worldState * bool) =
        if Map.containsKey (500, 0) world then
            (world, true)
        else
            let newPos =
                if y < floor - 1 then
                    fallGrain pos world
                else
                    pos

            if newPos = pos then
                (Map.add pos 'o' world, false)
            else
                moveGrainWithFloor newPos world floor

    let rec dropGrainWithFloor (world: worldState) (floor: int) : worldState =
        let (newWorld, finished) = moveGrainWithFloor initialGrainPos world floor

        if finished then
            newWorld
        else
            dropGrainWithFloor newWorld floor

    let parseInput (input: string) : position array array =
        input
        |> lines
        |> Array.map (fun s ->
            s.Split(" -> ")
            |> Array.map (commas >> Array.map int >> twoArrayToTuple))
        |> Array.distinct

    let countGrains (world: worldState) : int =
        world
        |> Map.values
        |> Seq.filter ((=) 'o')
        |> Seq.length

    let part1 (input: string) : string =
        let parsedInput = parseInput input

        let droppedWorld = dropGrain (addPaths parsedInput) (findBounds parsedInput)

        droppedWorld |> countGrains |> string

    let part2 (input: string) : string =
        let parsedInput = parseInput input

        let bounds = findBounds parsedInput

        let droppedWorld = dropGrainWithFloor (addPaths parsedInput) (bounds.Bottom + 2)

        droppedWorld |> countGrains |> string
