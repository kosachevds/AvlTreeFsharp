module TreeFactory

let private random = System.Random()

let createTreeWithItems =
    let emptyTree = AvlTree.createEmpty()
    Seq.fold AvlTree.add emptyTree

let getRandomSeq itemCount maxItem =
    Seq.init itemCount (fun _ -> random.Next(maxItem))

let createRandomItemsTree itemCount maxItem =
    getRandomSeq itemCount maxItem
    |> createTreeWithItems
