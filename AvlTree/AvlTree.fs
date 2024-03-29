module AvlTree

module Node =
    type Node<'a> = {
        key: 'a
        height: uint8
        left: Node<'a> option
        right: Node<'a> option
    }

    let create key height left right =
        {key=key; height=height; left=left; right=right}

    let createWithKey key =
        create key 1uy None None

    let setLeft node newLeft =
        {node with left = newLeft}

    let setSomeLeft node =
        Some >> setLeft node

    let setRight node newRight =
        {node with right = newRight}

    let setSomeRight node =
        Some >> setRight node

    let getLeft x = x.left

    let getRight x = x.right

    let heightOrZero node =
        match node with
        | None -> 0uy
        | Some x -> x.height

    let bFactor node =
        sbyte(heightOrZero node.right) - sbyte(heightOrZero node.left)

    let fixHeight node =
        let leftHeight = heightOrZero node.left
        let rightHeight = heightOrZero node.right
        let maxHeight = max leftHeight rightHeight
        {node with height = maxHeight + 1uy}

    let rotateRight node =
        let rotate node leftChild =
            setLeft node leftChild.right
            |> fixHeight
            |> setSomeRight leftChild
            |> fixHeight
        match node.left with
        | None -> node
        | Some leftChild -> rotate node leftChild

    let rotateLeft node =
        let rotate node rightChild =
            setRight node rightChild.left
            |> fixHeight
            |> setSomeLeft rightChild
            |> fixHeight
        match node.right with
        | None -> node
        | Some rightChild -> rotate node rightChild

    let balance node =
        let bigLeftRotate node =
            let conditionalRotate node right =
                match (bFactor right) with
                | x when (x < 0y) -> rotateRight right |> setSomeRight node
                | _ -> node
            match node.right with
            | None -> node
            | Some rightChild -> conditionalRotate node rightChild |> rotateLeft

        let bigRightRotate node =
            let conditionalRotate node left =
                match (bFactor left) with
                | x when (x > 0y) -> rotateLeft left |> setSomeLeft node
                | _ -> node
            match node.left with
            | None -> node
            | Some leftChild -> conditionalRotate node leftChild |> rotateRight

        let node' = fixHeight node
        match (bFactor node') with
        | 2y -> bigLeftRotate node'
        | -2y -> bigRightRotate node'
        |_ -> node'

    let balanceToOption node =
        node
        |> balance
        |> Some

    let rec insert root =
        let insertToSubtree root =
            function
            | k when (k < root.key) -> insert root.left k |> setSomeLeft root
            | k -> insert root.right k |> setSomeRight root
        match root with
        | None -> createWithKey
        | Some r -> insertToSubtree r >> balance

    let rec findMin root =
        match root.left with
        | None -> root
        | Some x -> findMin x

    let rec find root key =
        match root with
        | None -> None
        | Some root when (key < root.key) -> find root.left key
        | Some root when (key > root.key) -> find root.right key
        | _ -> root

    let rec removeMin root =
        let removeLeftMin root =
            removeMin >> setLeft root >> balanceToOption
        match root.left with
        | None -> root.right // TODO: remove root.right ?
        | Some left -> removeLeftMin root left

    let rec remove root key =
        let replaceWithRightMin node right =
            let rightMin = findMin right
            { rightMin with left = node.left; right = (removeMin right) }
            |> balanceToOption
        let removeNode node =
            match node.right with
            | None -> node.left
            | Some r -> replaceWithRightMin node r
        match root with
        | Some root when (key < root.key) -> key |> remove root.left |> setLeft root |> balanceToOption
        | Some root when (key > root.key) -> key |> remove root.right |> setRight root |> balanceToOption
        | Some root -> root.left
        | None -> None

type Tree<'a> = {
    root: Node.Node<'a> option
}

let createEmpty() =
    {root=None}

let create root =
    {root=root}

let add tree key =
    Node.insert tree.root key
    |> Some
    |> create

let remove tree key =
    create (Node.remove tree.root key)

let contains tree key =
    let node = Node.find tree.root key
    node.IsSome

let getHeight tree =
    Node.heightOrZero tree.root
