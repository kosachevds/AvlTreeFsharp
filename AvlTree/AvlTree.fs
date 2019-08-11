module AvlTree

module private Node =
    type Node<'a> = {
        key: 'a
        height: uint8
        mutable left: Node<'a> option
        mutable right: Node<'a> option
    }

    let create key height left right =
        {key=key; height=height; left=left; right=right}

    let createWithKey key =
        create key 1uy Option.None Option.None

    let setLeft node left =
        create node.key node.height left node.right
        // create node.key node.height left node.right

    let setRight node right =
        create node.key node.height node.left right

    let heightOrZero node =
        match node with
        | None -> 0uy
        | Some x -> x.height
        // TODO: cast to sbyte

    let bFactor node =
        sbyte(heightOrZero node.left) - sbyte(heightOrZero node.right)

    let fixHeight node =
        let left = heightOrZero node.left
        let right = heightOrZero node.right
        {key=node.key; height=(max left right); left=node.left; right=node.right}

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
        | 2y -> bigLeftRotate fixedP
        | -2y -> bigRightRotate fixedP
        |_ -> fixedP

    let rec insert root key =
        let root' =
            match (root, key) with
            | (None, k) -> createWithKey k
            | (Some r, k) when k < r.key -> setLeft r (Some(insert r.left k))
            | (Some r, k) -> setRight r (Some(insert r.right k))
        balance root'

    let rec findMin root =
        match root.left with
        | None -> root
        | Some x -> findMin x

