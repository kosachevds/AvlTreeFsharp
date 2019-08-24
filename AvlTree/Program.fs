
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

[<EntryPoint>]
let main argv =
    runTests 10

    writeHeights "heights.txt" 100

    Tests.timeRemoving 1000 10000 100
    // |> List.toSeq
    |> Seq.iter (fun x -> printfn "%d" x)
    // |> writeToFile "times.txt"
    0