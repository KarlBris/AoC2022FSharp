namespace AoC2022

open Utils

module Day08 =

    type Coord = (int * int)

    type TreeSet = Coord Set

    let rec findTreesInALine
        (coord: Coord)
        (nextCoordFunc: Coord -> Coord)
        (largestTree: int)
        (treeSet: TreeSet)
        (trees: int array array)
        : TreeSet =
        match largestTree with
        | 9 -> treeSet
        | _ ->
            let row = Array.tryItem (fst coord) trees

            match row with
            | None -> treeSet
            | Some r ->
                let col = Array.tryItem (snd coord) r

                match col with
                | None -> treeSet
                | Some tree ->
                    if tree > largestTree then
                        findTreesInALine (nextCoordFunc coord) nextCoordFunc tree (Set.add coord treeSet) trees
                    else
                        findTreesInALine (nextCoordFunc coord) nextCoordFunc largestTree treeSet trees

    let findVisibleTrees (trees: int array array) : TreeSet =
        let mapSize = Array.length trees
        let mutable treeSet = Set.empty

        for x = 0 to (mapSize - 1) do
            treeSet <- findTreesInALine (x, 0) (fun (a, b) -> (a, b + 1)) -1 treeSet trees
            treeSet <- findTreesInALine (x, mapSize - 1) (fun (a, b) -> (a, b - 1)) -1 treeSet trees

        for y = 0 to (mapSize - 1) do
            treeSet <- findTreesInALine (0, y) (fun (a, b) -> (a + 1, b)) -1 treeSet trees
            treeSet <- findTreesInALine (mapSize - 1, y) (fun (a, b) -> (a - 1, b)) -1 treeSet trees

        treeSet

    let part1 (input: string) : string =
        let intArray =
            input
            |> lines
            |> Array.map (fun l -> Array.map charToInt (Seq.toArray l))

        let visibleTrees = findVisibleTrees intArray

        visibleTrees |> Set.count |> string

    let rec getScore (view: int list) (vantageHeight: int) : int =
        match view with
        | [] -> 0
        | t :: ts ->
            if t >= vantageHeight then
                1
            else
                1 + getScore ts vantageHeight

    let calculateScenicScore (coord: Coord) (trees: int array array) : int =
        let mapSize = Array.length trees
        let xPos = fst coord
        let yPos = snd coord
        let rightDist = mapSize - 1 - yPos
        let downDist = mapSize - 1 - xPos

        let vantageHeight = trees[xPos][yPos]

        let rightView = Array.toList (Array.sub (trees[xPos]) (yPos + 1) rightDist)
        let rightScore = getScore rightView vantageHeight

        let leftView = Array.toList (Array.rev (Array.sub trees[xPos] 0 yPos))
        let leftScore = getScore leftView vantageHeight

        let transposedTrees = Array.transpose trees

        let downView = Array.toList (Array.sub (transposedTrees[yPos]) (xPos + 1) downDist)
        let downScore = getScore downView vantageHeight

        let upView = Array.toList (Array.rev (Array.sub transposedTrees[yPos] 0 xPos))
        let upScore = getScore upView vantageHeight

        rightScore * leftScore * downScore * upScore


    let part2 (input: string) : string =
        let intArray =
            input
            |> lines
            |> Array.map (fun l -> Array.map charToInt (Seq.toArray l))

        intArray
        |> Array.mapi (fun x row ->
            row
            |> Array.mapi (fun y _ -> calculateScenicScore (x, y) intArray))
        |> Array.concat
        |> Array.max
        |> string
