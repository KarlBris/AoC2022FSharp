namespace AoC2022

open Utils

module Day12 =

    type AreaMap = int array array

    type Direction =
        | U
        | D
        | L
        | R

    let directions = [ U; D; L; R ]

    type MaxValues = { A: int; B: int }

    type Position = int * int

    let heightMap =
        (List.zip [ 'a' .. 'z' ] [ 0..25 ])
        |> Map.ofList
        |> Map.add 'S' 0
        |> Map.add 'E' 25

    let makeHeightRow (cs: char seq) : int array =
        cs
        |> Seq.map (fun c -> heightMap[c])
        |> Array.ofSeq

    let movePosition (map: AreaMap) (maxValues: MaxValues) ((a, b): Position) (dir: Direction) : Position option =
        match dir with
        | U -> if a = 0 then None else Some(a - 1, b)
        | D ->
            if a = maxValues.A then
                None
            else
                Some(a + 1, b)
        | L -> if b = 0 then None else Some(a, b - 1)
        | R ->
            if b = maxValues.B then
                None
            else
                Some(a, b + 1)

    let getAtPosition (map: AreaMap) ((a, b): Position) : int =
        map[a][b]

    let getPossibleWalkPositionsFromPosition (map: AreaMap) (maxValues: MaxValues) (pos: Position) : Position list =
        directions
        |> List.map (movePosition map maxValues pos)
        |> List.map Option.toList
        |> List.concat
        |> List.filter (fun p -> (getAtPosition map p) - (getAtPosition map pos) <= 1 )

    let getAllCoordsFor (c: char) (letterMap: char array array) : Position list =
        letterMap
        |> Array.mapi (fun i row -> Array.mapi (fun j character -> ((i, j), character)) row)
        |> Array.concat
        |> Array.filter (fun (pos, character) -> character = c)
        |> Array.map fst
        |> Array.toList

    let rec findPath
        (maxValues: MaxValues)
        (startPositions: Position list)
        (endPos: Position)
        (areaMap: AreaMap)
        (visitedMap: Map<Position, int>)
        (stepsTaken: int)
        : Map<Position, int> =

        if visitedMap.ContainsKey endPos then
            visitedMap
        else
            let (newVisitedMap, newStartPositions) =
                List.fold
                    (fun (visitMap, newPositions) pos ->
                        let derivedPositions = 
                            (getPossibleWalkPositionsFromPosition areaMap maxValues pos)
                            |> List.filter (fun position -> visitMap |> Map.containsKey position |> not)
                        
                        let derivedVisitMap = 
                            derivedPositions
                            |> List.fold (fun s v -> Map.add v (stepsTaken+1) s ) visitMap
                        
                        (derivedVisitMap, List.append newPositions derivedPositions))
                    (visitedMap, [])
                    startPositions

            findPath maxValues newStartPositions endPos areaMap newVisitedMap (stepsTaken + 1)

    let part1 (input: string) : string =
        let letterMap = input |> lines |> Array.map (Array.ofSeq)
        let heightMap = letterMap |> Array.map makeHeightRow

        let startPos = letterMap |> getAllCoordsFor 'S'|> List.head
        let endPos = letterMap |> getAllCoordsFor 'E' |> List.head
        let mapWidth = Seq.length heightMap[0]
        let mapHeight = Seq.length heightMap
        
        let visitedMap: Map<Position, int> = Map.ofList [(startPos, 0)]
        let maxValues: MaxValues = { A = mapHeight - 1; B = mapWidth - 1 }

        let finalVisitedMap = findPath maxValues [ startPos ] endPos heightMap visitedMap 0

        finalVisitedMap[ endPos ].ToString()

    let part2 (input: string) : string = 
        let letterMap = input |> lines |> Array.map (Array.ofSeq)
        let heightMap = letterMap |> Array.map makeHeightRow
        
        let startPositions = [letterMap |> getAllCoordsFor 'S'; letterMap |> getAllCoordsFor 'a'] |> List.concat
        let endPos = letterMap |> getAllCoordsFor 'E' |> List.head
        let mapWidth = Seq.length heightMap[0]
        let mapHeight = Seq.length heightMap
        
        let visitedMap: Map<Position, int> = Map.ofList ( startPositions |> List.map (fun a -> a,0))
        let maxValues: MaxValues = { A = mapHeight - 1; B = mapWidth - 1 }

        let finalVisitedMap = findPath maxValues startPositions endPos heightMap visitedMap 0

        finalVisitedMap[ endPos ].ToString()
