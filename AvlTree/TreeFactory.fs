module TreeFactory

let private random = System.Random()

let createWithItems =
    let emptyTree = AvlTree.createEmpty()
    Seq.fold AvlTree.add emptyTree

let getRandomSeq itemCount maxItem =
    Seq.init itemCount (fun _ -> random.Next(maxItem))

let createWithRandomItems itemCount maxItem =
    getRandomSeq itemCount maxItem
    |> createWithItems
