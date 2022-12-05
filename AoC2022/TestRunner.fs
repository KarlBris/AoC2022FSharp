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
