let random = System.Random()

let createTreeWithItems =
    let emptyTree = AvlTree.createEmpty()
    Seq.fold AvlTree.add emptyTree

let getRandomSeq itemCount maxItem =
    Seq.init itemCount (fun _ -> random.Next(maxItem))

let createRandomItemsTree itemCount maxItem =
    getRandomSeq itemCount maxItem
    |> createTreeWithItems

let removeRandom tree maxItem =
    AvlTree.remove tree (random.Next(maxItem))

let timeRemove minCount maxCount step  =
    let executionTimeTicks func =
        let timer = System.Diagnostics.Stopwatch()
        timer.Start()
        func() |> ignore
        timer.Stop()
        timer.ElapsedTicks
    let timeRandomItemRemoving tree maxItem =
        executionTimeTicks (fun () -> removeRandom tree maxItem)
    seq {for c in minCount .. step .. (maxCount + 1) -> (c * 10, c) }
    |> Seq.toList
    |> List.map (
        (fun (maxItem, count) -> (maxItem, (createRandomItemsTree count maxItem))) >>
        (fun (maxItem, tree) -> timeRandomItemRemoving tree maxItem)
        )

let writeToFile filename =
    (Seq.map string) >> (fun x -> System.IO.File.WriteAllLines(filename, x))

let testAdd() =
    let items = getRandomSeq 100 1000 |> Seq.toList
    let tree = createTreeWithItems items
    let good = items |> List.forall (AvlTree.contains tree)
    System.Diagnostics.Debug.Assert(good, "Tree is not good")

let getHeights maxCount =
    seq {0 .. maxCount}
    // getRandomSeq maxCount (maxCount * 10)
    |> Seq.scan AvlTree.add (AvlTree.createEmpty())
    |> Seq.map AvlTree.getHeight

let testRemove itemsCount =
    let items = {0 .. itemsCount}
    let tree = createTreeWithItems items
    let removeItemAndCheckContains item =
        let tree' = AvlTree.remove tree item
        AvlTree.contains tree' item
    let good =
        items
        |> Seq.map removeItemAndCheckContains
        |> Seq.forall not
    System.Diagnostics.Debug.Assert(good, "Tree remove is not good")

let testRemoveDeep itemsCount =
    let items = {0 .. itemsCount} |> Seq.toList
    let success =
        items
        |> Seq.scan AvlTree.remove (AvlTree.createEmpty())
        |> Seq.zip (items |> Seq.mapi (fun i x -> (i, x)))
        |> Seq.map (
            fun ((index, item), tree) ->
            (
                let containsItem = AvlTree.contains tree item
                let containsAllNext =
                    items
                    |> Seq.skip index
                    |> Seq.forall (AvlTree.contains tree)
                (not containsItem) && containsAllNext
            )
        )
        |> Seq.forall id
    System.Diagnostics.Debug.Assert(success, "Tree remove is not good")


[<EntryPoint>]
let main argv =
    // testAdd()
    testRemove 10
    // testRemoveDeep 10
    printfn "%s" "Tests are passed"

    // getHeights 100
    // |> writeToFile "heights.txt"

    // timeRemove 1000 10000 100
    // |> Seq.toList
    // // |> Seq.iter (fun x -> printfn "%d" x)
    // |> writeToFile "times.txt"
    0