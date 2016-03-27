module QuadTree
// Quad tree implementation for spatial partitioning of a 2D plane
type 'a QuadTree = 
    // A leaf of the quad tree contains a list of 'a that are in its bounds
    | Leaf of  data : 'a list
    // A branch contains 'a instances that do not fit perfectly in a sub tree,
    // and 4 sub trees, splitting the 2d area evenly
    | Branch of
        data : 'a list *
        TR : QuadTree<'a> *
        BR : QuadTree<'a> *
        BL : QuadTree<'a> *
        TL : QuadTree<'a> 

type QuadBounds = { x : int; y : int; width : int; height : int }
    with member this.Contains(other:QuadBounds) =
            if    this.x <= other.x 
               && this.x + this.width >= other.x + other.width
               && this.y <= other.y
               && this.y + this.height >= other.y + other.height
            then true
            else false

type private QuadTreeLocation =
    | TopRight    
    | BottomRight
    | BottomLeft    
    | TopLeft    
        
let private getQuadrant itemBounds outerQuadrantBounds =
    let TR = { x = outerQuadrantBounds.x + (outerQuadrantBounds.width / 2)
               y = outerQuadrantBounds.y
               width = outerQuadrantBounds.width / 2 
               height = outerQuadrantBounds.height / 2 }

    let BR = { x = outerQuadrantBounds.x + (outerQuadrantBounds.width / 2)
               y = outerQuadrantBounds.y + (outerQuadrantBounds.height / 2)
               width = outerQuadrantBounds.width / 2 
               height = outerQuadrantBounds.height / 2 }

    let BL = { x = outerQuadrantBounds.x 
               y = outerQuadrantBounds.y + (outerQuadrantBounds.height / 2)
               width = outerQuadrantBounds.width / 2 
               height = outerQuadrantBounds.height / 2 }

    let TL = { x = outerQuadrantBounds.x 
               y = outerQuadrantBounds.y 
               width = outerQuadrantBounds.width / 2 
               height = outerQuadrantBounds.height / 2 }

    if   TR.Contains itemBounds then Some(TopRight, TR)
    elif BR.Contains itemBounds then Some(BottomRight, BR)
    elif BL.Contains itemBounds then Some(BottomLeft, BL)
    elif TL.Contains itemBounds then Some(TopLeft, TL)
    else None

let rec private insert getItemBounds maxItems maxDepth currentDepth bounds tree item =
    match tree with
    | Leaf data when data.Length < maxItems || currentDepth = maxDepth ->
        // max items not reached or max depth is reached, add item to list
        Leaf(item::data)
    | Branch(data,TR,BR,BL,TL) when maxDepth = currentDepth ->
        Branch(item::data,TR,BR,BL,TL)
    | Leaf data ->
        // reached capacity, split current data into a new branch 
        let branch = Branch([], Leaf [], Leaf[], Leaf [], Leaf[])
        (branch,item::data)
        ||> List.fold(insert getItemBounds maxItems maxDepth (currentDepth+1) bounds) 

    | Branch(data,TR,BR,BL,TL) -> 
        // if there is no clear fit, add to this branch data, else recurse down the correct branch
        let insert' = insert getItemBounds maxItems maxDepth (currentDepth+1)
        match getQuadrant (getItemBounds item) bounds with
        | Some(TopRight, newBounds)     -> Branch(data,insert' newBounds TR item,BR,BL,TL)
        | Some(BottomRight, newBounds)  -> Branch(data,TR,insert' newBounds BR item,BL,TL)
        | Some(BottomLeft, newBounds)   -> Branch(data,TR,BR,insert' newBounds BL item,TL)
        | Some(TopLeft, newBounds)      -> Branch(data,TR,BR,BL,insert' newBounds TL item)
        | None                          -> Branch(item::data,TR,BR,BL,TL)

let private walkData f tree =
    let rec aux = function
        | Leaf data -> 
            Leaf (f data)
        | Branch(data,TR,BR,BL,TL) -> 
            Branch(f data,aux TR,aux BR,aux BL,aux TL)
    aux tree

let private collectData tree =
    let out = ResizeArray<_>() // ooh mutable state!
    let rec aux = function
        | Leaf data -> out.AddRange data
        | Branch(data,TR,BR,BL,TL) -> 
            out.AddRange data
            aux TR
            aux BL
            aux BR
            aux TL
    aux tree
    List.ofSeq out

let create items getItemBounds maxItems maxDepth bounds  =
    (Leaf [],items) ||> List.fold(insert getItemBounds maxItems maxDepth 0 bounds)

let findNeighbours pred bounds maxBounds tree =
    let rec aux currentBounds = function
        | Leaf data -> 
            match List.tryFind pred data with
            | Some _ -> data
            | None -> []            
        | Branch(data,TR,BR,BL,TL) -> 
            match List.tryFind pred data with
            | Some _ -> 
                // todo - work out which quadrants overlap and return only those
                [ yield! collectData TR
                  yield! collectData BR
                  yield! collectData BL
                  yield! collectData TL ]
            | None -> 
                // recurse down the tree where the bounds fit
                match getQuadrant bounds currentBounds with
                | Some(TopRight, newBounds)     -> aux newBounds TR
                | Some(BottomRight, newBounds)  -> aux newBounds BR
                | Some(BottomLeft, newBounds)   -> aux newBounds BL
                | Some(TopLeft, newBounds)      -> aux newBounds TR
                | None                          -> []
    aux maxBounds tree

let map f = walkData (List.map f) 

let fold f acc = walkData (List.fold f acc) 

let filter f = walkData (List.filter f)

