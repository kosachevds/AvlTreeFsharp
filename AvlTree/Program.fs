let random = System.Random()

let createTreeWithItems =
    Seq.fold AvlTree.add AvlTree.createEmpty

let createRandomItemsTree itemCount maxItem =
    Seq.init itemCount (fun _ -> random.Next(maxItem))
    |> createTreeWithItems

let removeRandom tree maxItem =
    AvlTree.remove tree (random.Next(maxItem))

let timeRemove minCount maxCount step  =
    let executionTimeMsec func =
        let timer = System.Diagnostics.Stopwatch()
        timer.Start()
        func() |> ignore
        timer.ElapsedMilliseconds
    let timeRandomItemRemoving tree maxItem =
        executionTimeMsec (fun () -> removeRandom tree maxItem)
    seq {minCount .. step .. (maxCount + 1)}
    |>
    Seq.map (
        (fun x -> (10 * x, createRandomItemsTree x (10 * x))) >>
        (fun (maxItem, tree) -> timeRandomItemRemoving tree maxItem)
        )


[<EntryPoint>]
let main argv =

    printfn "%A" argv
    0 // return an integer exit code