
let writeToFile<'a> filename (items: seq<'a>) =
    items
    |> Seq.map (fun x -> x.ToString())
    |> fun x -> System.IO.File.WriteAllLines(filename, x)

let runTests itemsCount =
    Tests.adding itemsCount
    Tests.removing itemsCount
    // Tests.removingDeep itemsCount
    printfn "%s" "Tests are passed"

let writeHeights filename count =
    Tests.getHeights count
    |> writeToFile filename

let writeRemovingTime filename minCount maxCount countStep =
    let times = Tests.timeRemoving minCount maxCount countStep
    writeToFile filename times

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