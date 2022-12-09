namespace AoC2022

open System.IO
open System.Diagnostics

module TestRunner =

    let getInput day =
        let filename = $"..\..\..\inputs\input_day{day}.txt"

        if File.Exists filename then
            filename
            |> File.ReadAllText
            |> String.filter (fun c -> c <> '\r')
        else
            failwith $"Input file {filename} not found"

    let makeComparison (expectedResults: string []) (results: string []) =
        Array.zip results expectedResults
        |> Array.map (fun (r, e) -> (r, e, r = e))

    let printStatus ((res, expectedRes, success): string * string * bool) =
        printfn "%s! Got %s, expected %s." (if success then "Success" else "Failure") res expectedRes

    let run (examples: string []) expectedResults realInput (func: string -> string) title =
        printfn title

        if examples.Length = 0 then
            printfn "No examples found, running the real input..."
        else
            printfn "Running and verifying examples before the real input..."

        let resultList =
            examples
            |> Array.map func
            |> makeComparison expectedResults

        resultList |> Array.map printStatus |> ignore

        let examplesSuccessful =
            resultList
            |> Array.fold (fun b1 (_, _, b2) -> b1 && b2) true

        if examplesSuccessful then
            printfn "All examples were successful, running the real input..."
            let timer = new Stopwatch()
            timer.Start()
            printfn "Result from real input: %s" (func realInput)
            timer.Stop()
            printfn "Time elapsed: %A" timer.Elapsed
        else
            printfn "Some examples were not successful. PLEASE DO BETTER"

        printfn ""

    // Day1
    let input1 = getInput 1

    let examples1_1 =
        [| "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000" |]

    let exampleResults1_1 = [| "24000" |]

    let examples1_2 =
        [| "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000" |]

    let exampleResults1_2 = [| "45000" |]

    // Day2
    let input2 = getInput 2

    let examples2_1 = [| "A Y\nB X\nC Z" |]

    let exampleResults2_1 = [| "15" |]

    let examples2_2 = examples2_1

    let exampleResults2_2 = [| "12" |]

    // Day3
    let input3 = getInput 3

    let examples3_1 =
        [| "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw" |]

    let exampleResults3_1 = [| "157" |]

    let examples3_2 = examples3_1

    let exampleResults3_2 = [| "70" |]

    // Day4
    let input4 = getInput 4

    let examples4_1 = [| "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8" |]

    let exampleResults4_1 = [| "2" |]

    let examples4_2 = examples4_1

    let exampleResults4_2 = [| "4" |]

    // Day5
    let input5 = getInput 5

    let examples5_1 = [|"    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"|]

    let exampleResults5_1 = [|"CMZ"|]

    let examples5_2 = examples5_1

    let exampleResults5_2 = [|"MCD"|]

    // Day6
    let input6 = getInput 6

    let examples6_1 = [|"mjqjpqmgbljsphdztnvjfqwrcgsmlb"; "bvwbjplbgvbhsrlpgdmjqwftvncz"; "nppdvjthqldpwncqszvftbrmjlhg"; "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"; "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"|]

    let exampleResults6_1 = [|"7"; "5"; "6"; "10"; "11"|]

    let examples6_2 = [|"mjqjpqmgbljsphdztnvjfqwrcgsmlb"; "bvwbjplbgvbhsrlpgdmjqwftvncz"; "nppdvjthqldpwncqszvftbrmjlhg"; "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"; "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"|]

    let exampleResults6_2 = [|"19"; "23"; "23"; "29"; "26"|]

    // Day7
    let input7 = getInput 7

    let examples7_1 = [|"$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"|]

    let exampleResults7_1 = [|"95437"|]

    let examples7_2 = examples7_1

    let exampleResults7_2 = [|"24933642"|]

    // Day8
    let input8 = getInput 8

    let examples8_1 = [|"30373\n25512\n65332\n33549\n35390"|]

    let exampleResults8_1 = [|"21"|]

    let examples8_2 = examples8_1

    let exampleResults8_2 = [|"8"|]

    // Day9
    let input9 = getInput 9

    let examples9_1 = [|"R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"|]

    let exampleResults9_1 = [|"13"|]

    let examples9_2 = [|"R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"|]

    let exampleResults9_2 = [|"36"|]
