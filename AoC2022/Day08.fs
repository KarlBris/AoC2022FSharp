namespace AoC2022

open Utils

module Day08 =

    type Coord = (int * int)

    type TreeSet = Coord Set

    let rec findTreesInALine
        (coord: Coord)
        (nextCoordFunc: Coord -> Coord)
        (largestTree: int)
        (trees: int array array)
        (treeSet: TreeSet)
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
                        findTreesInALine (nextCoordFunc coord) nextCoordFunc tree trees (Set.add coord treeSet)
                    else
                        findTreesInALine (nextCoordFunc coord) nextCoordFunc largestTree trees treeSet

    let findVisibleTrees (trees: int array array) : TreeSet =
        let mapSize = Array.length trees
        let treeSet = Set.empty

        let treeSet2 =
            [ 0 .. (mapSize - 1) ]
            |> List.fold
                (fun foldTreeSet x ->
                    foldTreeSet
                    |> findTreesInALine (x, 0) (fun (a, b) -> (a, b + 1)) -1 trees
                    |> findTreesInALine (x, mapSize - 1) (fun (a, b) -> (a, b - 1)) -1 trees)
                treeSet

        let treeSet3 =
            [ 0 .. (mapSize - 1) ]
            |> List.fold
                (fun foldTreeSet y ->
                    foldTreeSet
                    |> findTreesInALine (0, y) (fun (a, b) -> (a + 1, b)) -1 trees
                    |> findTreesInALine (mapSize - 1, y) (fun (a, b) -> (a - 1, b)) -1 trees)
                treeSet2

        treeSet3

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
