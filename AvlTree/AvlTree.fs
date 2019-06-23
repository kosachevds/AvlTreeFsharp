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

    let balance p =
        let fixedP = fixHeight p
        match (bFactor fixedP) with
        | 2y -> 
        | -2y ->
        |_ -> fixedP




