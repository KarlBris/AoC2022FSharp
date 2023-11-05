namespace AoC2022

open Utils

module Day15 =

    type position = int * int
    type state = (position * int) array

    let distance ((x1, y1): position) ((x2, y2): position) : int = (abs (x1 - x2)) + (abs (y1 - y2))

    let makePosition (positionString: string) : position =
        positionString
        |> commas
        |> Array.map (fun s -> s.Substring(2) |> int)
        |> twoArrayToTuple

    let parseInput (input: string) : (state * position array) =
        input
        |> lines
        |> Array.map (fun s ->
            s.Split(": ")
            |> twoArrayToTuple
            |> (fun (a, b) ->
                let sensorPosition = a.Substring(10) |> makePosition
                let beaconPosition = b.Substring(21) |> makePosition
                ((sensorPosition, distance sensorPosition beaconPosition), beaconPosition)))
        |> Array.unzip

    let isWithinDistance (state: state) (pos: position) : bool =
        state
        |> Array.map (fun (pos2, dist) -> distance pos pos2 <= dist)
        |> Array.fold (fun s b -> b || s) false

    let part1 (input: string) : string =
        let (parsed, beaconPositions) = parseInput input

        let leftBounds =
            parsed
            |> Array.map (fun ((x, _), dist) -> x - dist)
            |> Array.min

        let rightBounds =
            parsed
            |> Array.map (fun ((x, _), dist) -> x + dist)
            |> Array.max

        let yLevel =
            if Array.length parsed > 14 then
                2000000
            else
                10

        let positions =
            [| leftBounds..rightBounds |]
            |> Array.map (fun x -> (x, yLevel))
            |> Array.except beaconPositions

        positions
        |> Array.map (isWithinDistance parsed)
        |> Array.filter id
        |> Array.length
        |> string

    let rec findStepDistanceBasedOnFirstWithin (pos: position) (parsed: (position * int) list) : int option =
        match parsed with
        | [] -> None
        | (p, dist) :: xs ->
            let isWithin = distance pos p <= dist

            if isWithin then
                let halfLineWidth = (dist - ((abs ((snd pos) - (snd p)))))
                let xDistToSensor = (fst p) - (fst pos)
                Some(halfLineWidth + xDistToSensor + 1)
            else
                findStepDistanceBasedOnFirstWithin pos xs

    let rec traverseLine (yPosition: int) (xPosition: int) (rightBounds: int) (parsed: state) : position option =
        let pos = (xPosition, yPosition)

        let stepDist = findStepDistanceBasedOnFirstWithin pos (Array.toList parsed)

        match stepDist with
        | Some sd ->
            let newXposition = xPosition + sd

            if newXposition > rightBounds then
                None
            else
                traverseLine yPosition newXposition rightBounds parsed
        | None -> Some pos

    let rec traverse (yPositions: int list) (rightBounds: int) (parsed: state) : position =
        match yPositions with
        | [] -> failwith "could not find distress beacon"
        | y :: ys ->
            match traverseLine y 0 rightBounds parsed with
            | Some p -> p
            | None -> traverse ys rightBounds parsed

    let part2 (input: string) : string =
        let (parsed, _) = parseInput input

        let (bottomBounds, rightBounds) =
            if Array.length parsed > 14 then
                (4000000, 4000000)
            else
                (20, 20)

        let topToBottomYPositions = [ 0..bottomBounds ]

        let (distressX, distressY) = traverse topToBottomYPositions rightBounds parsed

        ((int64 distressX) * 4000000L) + (int64 distressY)
        |> string
