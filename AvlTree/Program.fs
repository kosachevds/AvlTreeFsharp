let createTreeWithItems =
    Seq.fold AvlTree.add AvlTree.createEmpty

let createRandomItemsTree itemCount maxItem =
    let random = System.Random()
    Seq.init itemCount (fun _ -> random.Next(maxItem))
    |> createTreeWithItems


[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code