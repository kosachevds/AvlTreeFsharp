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

    let setRight node newRight =
        {node with right = newRight}

    let heightOrZero node =
        match node with
        | None -> 0uy
        | Some x -> x.height
        // TODO: cast to sbyte

    let bFactor node =
        sbyte(heightOrZero node.left) - sbyte(heightOrZero node.right)

    let fixHeight node =
        let leftHeight = heightOrZero node.left
        let rightHeight = heightOrZero node.right
        let maxHeight = max leftHeight rightHeight
        {node with height = maxHeight + 1uy}

    let rotateRight p =
        // TODO: try to remade
        let setPLeft =
            setLeft p
        match p.left with
        | None -> p  // TODO: or what?
        | Some q -> setRight q (Some(setLeft p q.right))
        // | Some q -> Some (setRight q (setLeft p q.right))

    let rotateLeft p =
        match p.right with
        | None -> p
        | Some q -> setLeft q (Some(setRight p q.left))


    let bigLeftRotate p =
        let conditionalRotate node right =
            match (bFactor right) with
            | x when x < 0y -> setRight node (Some(rotateRight(right)))
            | _ -> node
        match p.right with
        | None -> p
        | Some q -> rotateLeft (conditionalRotate p q)


    let bigRightRotate p =
        let conditionalRotate node left =
            match (bFactor left) with
            | x when x > 0y -> setLeft node (Some(rotateLeft left))
            | _ -> node
        match p.left with
        | None -> p
        | Some q -> rotateRight (conditionalRotate p q)

    let balance p =
        let fixedP = fixHeight p
        let pBFactor = bFactor fixedP
        match pBFactor with
        | -2y -> bigLeftRotate fixedP
        | 2y -> bigRightRotate fixedP
        |_ -> fixedP

    let rec insert root key =
        let insertToSubtree root key =
            match key with
            | k when k < root.key -> setLeft root (Some(insert root.left k))
            | k -> setRight root (Some(insert root.right k))
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
        let balanceToOption x =
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
