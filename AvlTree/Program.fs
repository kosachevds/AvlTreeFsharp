
let writeToFile filename =
    (Seq.map string) >> (fun x -> System.IO.File.WriteAllLines(filename, x))

[<EntryPoint>]
let main argv =
    Tests.testAdd()
    Tests.testRemove 10
    Tests.testRemoveDeep 10
    printfn "%s" "Tests are passed"

    Tests.getHeights 100
    |> writeToFile "heights.txt"

    Tests.timeRemove 1000 10000 100
    // |> List.toSeq
    |> Seq.iter (fun x -> printfn "%d" x)
    // |> writeToFile "times.txt"
    0