namespace AoC2022

open Utils

module Day07 =

    type DirTree =
        | Directory of (string * DirTree list * int)
        | File of (string * int)

    let makePathList (path: string) : string list = path |> slashes |> Array.toList

    let rec getDirectoryTree (path: string list) (tree: DirTree) : DirTree option =
        match path with
        | [] -> Some tree
        | p :: ps ->
            match tree with
            | File _ -> None
            | Directory (_, subDirs, _) ->
                let subTreeOption =
                    subDirs
                    |> List.filter (fun d ->
                        match d with
                        | File _ -> false
                        | Directory (name, _, _) -> name = p)
                    |> List.tryHead

                match subTreeOption with
                | Some subTree -> getDirectoryTree ps subTree
                | None -> None

    let replaceSubTree (newSubTree: DirTree) (subTreeList: DirTree list) : (DirTree list) =
        let newSubTreeName =
            match newSubTree with
            | File (name, _) -> name
            | Directory (name, _, _) -> name

        List.map
            (fun stree ->
                match stree with
                | Directory (name, _, _) as dir ->
                    if name = newSubTreeName then
                        newSubTree
                    else
                        dir
                | file -> file)
            subTreeList

    let rec setDirectoryTree (path: string list) (newSubTree: DirTree) (tree: DirTree) : DirTree =
        match path with
        | [] ->
            match tree with
            | File _ -> tree
            | Directory _ -> newSubTree
        | p :: ps ->
            match tree with
            | File _ -> failwith "fail"
            | Directory (name, subs, size) as dir ->
                let subTreeOption =
                    subs
                    |> List.filter (fun d ->
                        match d with
                        | File _ -> false
                        | Directory (name, _, _) -> name = p)
                    |> List.tryHead

                match subTreeOption with
                | Some subTree ->
                    let newSubTrees = replaceSubTree (setDirectoryTree ps newSubTree subTree) subs
                    Directory(name, newSubTrees, size)
                | None -> dir

    let addToCurrentDirectory (currentPath: string) (tree: DirTree) (toAdd: DirTree) : DirTree =
        match getDirectoryTree (makePathList currentPath) tree with
        | None -> failwith "could not find current directory"
        | Some (File _) -> failwith "file found instead of directory"
        | Some (Directory (name, subs, size)) ->
            setDirectoryTree (makePathList currentPath) (Directory(name, toAdd :: subs, size)) tree

    let getDirNamesFromSubTree (subDirs: DirTree list) : string list =
        subDirs
        |> List.map (fun subDir ->
            match subDir with
            | File _ -> []
            | Directory (name, _, _) -> [ name ])
        |> List.concat

    let oneStepUp (path: string) : string =
        let a = path |> slashes |> Array.toList |> List.rev

        match a with
        | [] -> "/"
        | _ :: ds -> "/" + (String.concat "/" (List.rev ds))

    let rec parseTree (currentPath: string) (tree: DirTree) (lines: string list) : DirTree =
        match lines with
        | [] -> tree
        | line :: restOfLines ->
            let lineWords = words line

            match lineWords[0] with
            | "$" ->
                match lineWords[1] with
                | "cd" ->
                    match lineWords[2] with
                    | ".." -> parseTree (oneStepUp currentPath) tree restOfLines
                    | dir -> parseTree (currentPath + "/" + dir) tree restOfLines
                | _ -> parseTree currentPath tree restOfLines // ls case
            | "dir" ->
                let directory = (lineWords[1], [], 0)

                parseTree currentPath (addToCurrentDirectory currentPath tree (Directory directory)) restOfLines
            | _other ->
                let file = (lineWords[1], int lineWords[0])
                parseTree currentPath (addToCurrentDirectory currentPath tree (File file)) restOfLines

    let rec getSize (tree: DirTree) : int =
        match tree with
        | File (_, size) -> size
        | Directory (_, subs, _) -> subs |> List.map getSize |> List.sum

    let rec setWeights (tree: DirTree) : DirTree =
        match tree with
        | Directory (name, subs, _) ->
            let newSubTree = subs |> List.map setWeights
            let newSize = subs |> List.map getSize |> List.sum
            Directory(name, newSubTree, newSize)
        | file -> file

    let rec findSumOfDirsOfAtMostSize (maxSize: int) (tree: DirTree) : int =
        match tree with
        | Directory (_, subDirs, size) ->
            let a =
                subDirs
                |> List.map (findSumOfDirsOfAtMostSize maxSize)
                |> List.sum

            a + size
        | _ -> 0

    let rec getDirectorySizeList (tree: DirTree) : int list =
        match tree with
        | File _ -> []
        | Directory (_, subs, size) ->
            size
            :: (subs
                |> List.map getDirectorySizeList
                |> List.concat)

    let parseTreeAndDistributeWeights (input: string) : int list =
        input
        |> lines
        |> Array.toList
        |> parseTree "/" (Directory("/", [], 0))
        |> setWeights
        |> getDirectorySizeList

    let part1 (input: string) : string =
        input
        |> parseTreeAndDistributeWeights
        |> List.filter (fun s -> s <= 100000)
        |> List.sum
        |> string

    let part2 (input: string) : string =
        let dirList = input |> parseTreeAndDistributeWeights

        let freeSpace = (70000000 - (List.max dirList))
        let spaceNeeded = 30000000 - freeSpace

        dirList
        |> List.filter (fun s -> s >= spaceNeeded)
        |> List.sort
        |> List.head
        |> string
