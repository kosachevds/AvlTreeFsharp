let random = System.Random()

let createTreeWithItems =
    Seq.fold AvlTree.add AvlTree.createEmpty

let getRandomSeq itemCount maxItem =
    Seq.init itemCount (fun _ -> random.Next(maxItem))

let createRandomItemsTree itemCount maxItem =
    getRandomSeq itemCount maxItem
    |> createTreeWithItems

let removeRandom tree maxItem =
    AvlTree.remove tree (random.Next(maxItem))

let timeRemove minCount maxCount step  =
    let executionTimeMsec func =
        let timer = System.Diagnostics.Stopwatch()
        timer.Start()
        let temp = func()
        timer.ElapsedMilliseconds
    let timeRandomItemRemoving tree maxItem =
        executionTimeMsec (fun () -> removeRandom tree maxItem)
    seq {for c in minCount .. step .. (maxCount + 1) -> (c * 10, c) }
    |> Seq.toList
    |> List.map (
        (fun (maxItem, count) -> (maxItem, (createRandomItemsTree count maxItem))) >>
        (fun (maxItem, tree) -> timeRandomItemRemoving tree maxItem)
        )

let writeToFile filename =
    Seq.cast<string> >> (fun items -> System.IO.File.AppendAllLines(filename, items))

let testAdd() =
    let items = getRandomSeq 100 1000 |> Seq.toList
    let tree = createTreeWithItems items
    let good = items |> List.forall (AvlTree.contains tree)
    System.Diagnostics.Debug.Assert(good, "Tree is not good")

[<EntryPoint>]
let main argv =
    testAdd()
    let times = timeRemove 1000 10000 1000 |> Seq.toList
    times
    |> Seq.iter (fun x -> printfn "%d" x)
    // writeToFile "times.txt" times
    0 // return an integer exit code