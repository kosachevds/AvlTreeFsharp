module Tests

let private random = System.Random()

let testAdd() =
    let items = TreeFactory.getRandomSeq 100 1000 |> Seq.toList
    let tree = TreeFactory.createWithItems items
    let good = items |> List.forall (AvlTree.contains tree)
    System.Diagnostics.Debug.Assert(good, "Tree is not good")

let testRemove itemsCount =
    let items = {0 .. itemsCount}
    let tree = TreeFactory.createWithItems items
    let checkContains (item, tree) =
        AvlTree.contains tree item
    let temp =
        items
        |> Seq.map (AvlTree.remove tree)
        |> Seq.zip items
        |> Seq.forall (checkContains >> not)
    System.Diagnostics.Debug.Assert(temp, "Tree remove is not good")

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
        (fun (maxItem, count) -> (maxItem, (TreeFactory.createRandomItemsTree count maxItem))) >>
        (fun (maxItem, tree) -> timeRandomItemRemoving tree maxItem)
        )

let getHeights maxCount =
    seq {0 .. maxCount}
    // getRandomSeq maxCount (maxCount * 10)
    |> Seq.scan AvlTree.add (AvlTree.createEmpty())
    |> Seq.map AvlTree.getHeight
