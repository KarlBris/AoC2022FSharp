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

    // Day10
    let input10 = getInput 10

    let examples10_1 = [|"addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop"|]

    let exampleResults10_1 = [|"13140"|]

    let examples10_2 = examples10_1

    let exampleResults10_2 = [|"\n██  ██  ██  ██  ██  ██  ██  ██  ██  ██  \n███   ███   ███   ███   ███   ███   ███ \n████    ████    ████    ████    ████    \n█████     █████     █████     █████     \n██████      ██████      ██████      ████\n███████       ███████       ███████     "|]

    // Day11
    let input11 = getInput 11

    let examples11_1 = [|"Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1"|]

    let exampleResults11_1 = [|"10605"|]

    let examples11_2 = examples11_1

    let exampleResults11_2 = [|"2713310158"|]
