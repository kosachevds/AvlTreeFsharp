module Tests

let private random = System.Random()

let adding itemsCount =
    let items =
        TreeFactory.getRandomSeq itemsCount (10 * itemsCount)
        |> Seq.toList
    let tree = TreeFactory.createWithItems items
    let good = items |> List.forall (AvlTree.contains tree)
    System.Diagnostics.Debug.Assert(good, "Tree adding is not good")

let removing itemsCount =
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

let removingDeep itemsCount =
    let items = {0 .. itemsCount} |> Seq.toList
    let getItemsExceptIndex i =
        let tail =
            items
            |> Seq.take i
        items
        |> Seq.skip (i + 1)
        |> Seq.append tail

    let treeContainsAll tree =
        Seq.forall (AvlTree.contains tree)

    let tree = TreeFactory.createWithItems items
    let trees =
        items
        |> Seq.map (AvlTree.remove tree)
        |> Seq.toList
    let containsRemovedItems =
        trees
        |> Seq.zip items
        |> Seq.forall (fun (x, tree) -> AvlTree.contains tree x)
    let containsOtherItems =
        items
        |> Seq.mapi (fun i _ -> getItemsExceptIndex i)
        |> Seq.zip trees
        |> Seq.forall (fun (tree, others) -> treeContainsAll tree others)
    let success = not(containsRemovedItems) && containsOtherItems
    System.Diagnostics.Debug.Assert(success, "Tree remove is not good")


let timeRemoving minCount maxCount step  =
    let executionTimeTicks func =
        let timer = System.Diagnostics.Stopwatch()
        timer.Start()
        func() |> ignore
        timer.Stop()
        timer.ElapsedTicks
    let timeRandomItemRemoving tree =
        let item = random.Next()
        executionTimeTicks (fun () -> AvlTree.remove tree item)
    let createTree itemCount =
        TreeFactory.createWithRandomItems itemCount System.Int32.MaxValue

    let counts =
        [minCount]  // repeat first for strange large time in first launch
        |> List.toSeq
        |> Seq.append (seq {minCount .. step .. (maxCount + 1) })
    counts
    |> Seq.map (createTree >> timeRandomItemRemoving)
    |> Seq.skip 1  // skip first because strange large time


let getHeights maxCount =
    seq {0 .. maxCount}
    // getRandomSeq maxCount (maxCount * 10)
    |> Seq.scan AvlTree.add (AvlTree.createEmpty())
    |> Seq.map AvlTree.getHeight
