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

    let getHeight node =
        match node with
        | None -> 0uy
        | Some x -> x.height
        // TODO: cast to sbyte

    let bFactor node =
        sbyte(getHeight node.left) - sbyte(getHeight node.right)

    let fixHeight node =
        let left = getHeight node.left
        let right = getHeight node.right
        {key=node.key; height=(max left right); left=node.left; right=node.right}

    let rotateRight p =
        // TODO: try to remade
        let setPLeft =
            setLeft p
        match p.left with
        | None -> p  // TODO: or what?
        | Some q -> setRight q (Some(setLeft p q.right))
        // | Some q -> Some (setRight q (setLeft p q.right))

    let rorateLeft p =
        match p.right with
        | None -> p
        | Some q -> setLeft q (Some(setRight p q.left))


    let bigLeftRotate p =
        let conditionalRotate node right =
            if (bFactor right) < 0y then
                setRight node (Some(rotateRight(right)))
            else
                node
        match p.right with
        | None -> p
        | Some q -> rorateLeft (conditionalRotate p q)


    let bigRightRotate p =
        let conditionalRotate node left =
            if (bFactor left) > 0y then
                setLeft node (Some(rorateLeft left))
            else
                node
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




