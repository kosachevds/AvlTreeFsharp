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

    let heightOrZero node =
        match node with
        | None -> 0uy
        | Some x -> x.height
        // TODO: cast to sbyte

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
            let node' = fixHeight (setRight node rightChild.left)
            fixHeight (setSomeLeft rightChild node')
        match node.right with
        | None -> node
        | Some rightChild -> rotate node rightChild

    let balance node =
        let bigLeftRotate node =
            let conditionalRotate node right =
                match (bFactor right) with
                | x when x < 0y -> setSomeRight node (rotateRight right)
                | _ -> node
            match node.right with
            | None -> node
            | Some rightChild -> rotateLeft (conditionalRotate node rightChild)

        let bigRightRotate node =
            let conditionalRotate node left =
                match (bFactor left) with
                | x when x > 0y -> setSomeLeft node (rotateLeft left)
                | _ -> node
            match node.left with
            | None -> node
            | Some leftChild -> rotateRight (conditionalRotate node leftChild)

        let node' = fixHeight node
        let pBFactor = bFactor node'
        match pBFactor with
        | 2y -> bigLeftRotate node'
        | -2y -> bigRightRotate node'
        |_ -> node'

    let rec insert root key =
        let insertToSubtree root key =
            match key with
            | k when k < root.key -> setSomeLeft root (insert root.left k)
            | k -> setSomeRight root (insert root.right k)
        match root with
        | None -> createWithKey key
        | Some r -> balance (insertToSubtree r key)

    let rec findMin root =
        match root.left with
        | None -> root
        | Some x -> findMin x

    let rec find root key =
        match root with
        | None -> None
        | Some root when key < root.key -> find root.left key
        | Some root when key > root.key -> find root.right key
        | _ -> root

    let rec removeMin root =
        let removeMinInLeftSubtree root left =
            let root' = setLeft root (removeMin left)
            balance root'
        match (root, root.left) with
        | (root, None) -> root.right // TODO: remove root.right ?
        | (root, Some left) -> Some (removeMinInLeftSubtree root left)

    let rec remove root key =
        // TODO: remade
        let balanceToOption x =
            // balance >> Some
            Some (balance x)
        let removeNode node =
            let q = node.left
            let r = node.right
            match r with
            | None -> q
            | Some r -> (
                         let minNode = findMin r
                         let minNode' = setRight minNode (removeMin r)
                         let minNode'' = setLeft minNode' q
                         balanceToOption minNode''
            )
        let removeInSomeRoot root key =
            match key with
            | key when key < root.key -> balanceToOption (setLeft root (remove root.left key))
            | key when key > root.key -> balanceToOption (setRight root (remove root.right key))
            | _ -> removeNode root
        match root with
        | None -> None
        | Some root -> removeInSomeRoot root key

type Tree<'a> = {
    root: Node.Node<'a> option
}

let createEmpty() =
    {root=None}

let create root =
    {root=root}

let add tree key =
    let newRoot = Node.insert tree.root key
    create (Some newRoot)

let remove tree key =
    create (Node.remove tree.root key)

let contains tree key =
    let node = Node.find tree.root key
    node.IsSome

let getHeight tree =
    Node.heightOrZero tree.root
