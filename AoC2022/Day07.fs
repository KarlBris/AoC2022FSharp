namespace AoC2022

open Utils

module Day07 =

    type DirTree =
        | Directory of (string * DirTree list * int option)
        | File of (string * int)

    let rec getDirectoryTree (directory: string) (tree: DirTree) : DirTree option =
        match tree with
        | File _ -> None
        | Directory (name, subDirs, _) as tree ->
            if name = directory then
                Some tree
            else
                subDirs
                |> List.map ((getDirectoryTree directory) >> Option.toList)
                |> List.concat
                |> List.tryHead

    let rec setDirectoryTree (directory: string) (newSubTree: DirTree) (tree: DirTree) : DirTree =
        match tree with
        | File _ -> tree
        | Directory (name, subs, size) ->
            if name = directory then
                newSubTree
            else
                Directory(
                    name,
                    (subs
                     |> List.map (setDirectoryTree directory newSubTree)),
                    size
                )

    let addToCurrentDirectory (currentDirectory: string) (tree: DirTree) (toAdd: DirTree) : DirTree =
        match getDirectoryTree currentDirectory tree with
        | None -> failwith "could not find current directory"
        | Some (File _) -> failwith "file got instead of directory"
        | Some (Directory (name, subs, size)) ->
            setDirectoryTree currentDirectory (Directory(name, toAdd :: subs, size)) tree

    let getDirNamesFromSubTree (subDirs: DirTree list) : string list =
        subDirs
        |> List.map (fun subDir ->
            match subDir with
            | File _ -> []
            | Directory (name, _, _) -> [ name ])
        |> List.concat

    let rec findDirOneStepUp (currentDirectory: string) (tree: DirTree) : string option =
        // This sometimes goes straight to / for some reason
        match tree with
        | Directory (name, subDirs, size) ->
            let subDirNames = getDirNamesFromSubTree subDirs

            if List.isEmpty subDirNames then
                None
            else if List.contains currentDirectory subDirNames then
                Some name
            else
                subDirs
                |> List.map (findDirOneStepUp currentDirectory)
                |> List.tryFind (fun v -> Option.isSome v)
                |> Option.flatten

        | File (name, size) -> None

    let rec parseTree (currentDirectory: string) (tree: DirTree) (lines: string list) : DirTree =
        match lines with
        | [] -> tree
        | line :: restOfLines ->
            let lineWords = words line

            match lineWords[0] with
            | "$" ->
                match lineWords[1] with
                | "cd" ->
                    match lineWords[2] with
                    | ".." ->
                        match (findDirOneStepUp currentDirectory tree) with
                        | Some dirName -> parseTree dirName tree restOfLines
                        | None -> failwith "error"
                    | dir -> parseTree dir tree restOfLines
                | _ -> parseTree currentDirectory tree restOfLines // ls case
            | "dir" ->
                let directory = (lineWords[1], [], None)

                parseTree
                    currentDirectory
                    (addToCurrentDirectory currentDirectory tree (Directory directory))
                    restOfLines
            | _other ->
                let file = (lineWords[1], int lineWords[0])
                parseTree currentDirectory (addToCurrentDirectory currentDirectory tree (File file)) restOfLines

    let rec getSize (tree: DirTree) : int =
        match tree with
        | File (_, size) -> size
        | Directory (_, subs, _) -> subs |> List.map getSize |> List.sum

    let rec setWeights (tree: DirTree) : DirTree =
        match tree with
        | Directory (name, subs, size) ->
            let newSubTree = subs |> List.map setWeights
            let newSize = subs |> List.map getSize |> List.sum
            Directory(name, newSubTree, Some newSize)
        | file -> file

    let rec findSumOfDirsOfAtMostSize (maxSize: int) (tree: DirTree) : int =
        match tree with
        | Directory (_, subDirs, size) ->
            let a =
                subDirs
                |> List.map (findSumOfDirsOfAtMostSize maxSize)
                |> List.sum

            a + (Option.get size)
        | _ -> 0

    let rec getDirectoryList (tree: DirTree) : DirTree list =
        match tree with
        | File _ -> []
        | Directory (_, subs, _) as dir ->
            dir
            :: (subs |> List.map getDirectoryList |> List.concat)

    let part1 (input: string) : string =
        let tree =
            input
            |> lines
            |> Array.toList
            |> parseTree "/" (Directory("/", [], None))

        let weightedTree = setWeights tree
        let dirList = getDirectoryList weightedTree

        let b =
            dirList
            |> List.map (fun (Directory (_, _, size)) -> size)
            |> List.filter (fun s -> Option.get s <= 100000)
            |> List.map Option.get
            |> List.sum
            |> string

        b

    let part2 (input: string) : string = input
