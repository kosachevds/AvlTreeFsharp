
let writeToFile filename =
    (Seq.map string) >> (fun x -> System.IO.File.WriteAllLines(filename, x))

let runTests itemsCount =
    Tests.adding itemsCount
    Tests.removing itemsCount
    // Tests.removingDeep itemsCount
    printfn "%s" "Tests are passed"

let writeHeights filename count =
    Tests.getHeights count
    |> writeToFile filename

// let writeRemovingTime filename minCount maxCount countStep =
//     let times = Tests.timeRemoving minCount maxCount countStep
//     // |> List.toSeq
//     // |> writeToFile filename

let printRemovingTime minCount maxCount countStep =
    let printIntLn = printfn "%d"
    Tests.timeRemoving minCount maxCount countStep
    |> (Seq.iter printIntLn)

[<EntryPoint>]
let main argv =
    runTests 10

    writeHeights "heights.txt" 100

    printRemovingTime 10 100 10
    0