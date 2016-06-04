module PathFinding

  open System.Collections.Generic
  open System.Linq
  open TreatzGame
  open CommonData

  let getNeighbours allNodes node=            
      let x = node.Identity.Column
      let y = node.Identity.Row
      let identites = [(1<cell>,0<cell>); (-1<cell>, 0<cell>); (0<cell>, -1<cell>); (0<cell>, 1<cell>)]
                      |> List.map(fun (o,r) -> 
                          {Column= (x + o); Row= (y + r)} )
      [|for i in identites do
        if Map.containsKey (i.Column, i.Row) allNodes then
            yield allNodes.[(i.Column, i.Row)] |]

  let costDistance (node:Node) goalNode =    
    abs (node.Identity.Column - goalNode.Identity.Column) + abs( node.Identity.Row - goalNode.Identity.Row) 

  let convertToArray (pathNode :NodePath) =
    let rec someFn n (array: Node array) =
      match n.Parent with
      | None -> array
      | Some parent -> someFn parent (Array.append [|n.GridNode|] array)    
    someFn pathNode Array.empty 

  let areNeighbours n1 n2 = 
    let difX = abs(n1.Column - n2.Column) = 1<cell> 
    let difY = abs(n1.Row - n2.Row) = 1<cell>

    match difX, difY with
    | true, true | false, false -> false
    | true, _ | _, true -> true

  let findNeigbourParent nodePath node =
    let rec findP nodePath =
      match nodePath.Parent with
      | None ->   nodePath
      | Some p -> if (areNeighbours p.GridNode.Identity node.Identity) then 
                      p 
                  else
                      findP p
    findP nodePath
    

  let search startNode goal : Node array =    
    let frontier = HashSet<Node>() 
    frontier.Add startNode |> ignore 
    let explored = new HashSet<Node>() 
    let mutable currentPathNode = {Parent = None; PathCost= 0.; GridNode = startNode}    
    
    if (frontier.Count = 0) then 
        [||]
    else  
    while frontier.Count > 0 do      
      let currentNode = 
          frontier.ToArray() 
          |> Seq.minBy(fun x -> costDistance x goal + x.Cost + (int currentPathNode.PathCost) * 1<cell> )
      currentPathNode <- { 
                            Parent = if areNeighbours currentPathNode.GridNode.Identity currentNode.Identity then  
                                        Some(currentPathNode)  
                                     else Some(findNeigbourParent currentPathNode currentNode)
                            PathCost = double currentNode.Cost + currentPathNode.PathCost 
                            GridNode= currentNode }
      frontier.Remove(currentNode)  |> ignore
      if (currentNode.Identity <> goal.Identity) then
          explored.Add(currentNode) |> ignore 
          
          (currentNode.Neighbours)
          |> Seq.iter(fun n -> 
                if not(explored.Contains(n)) then 
                  frontier.Add(n) |> ignore )          
      else
          frontier.Clear()  //yuck          
    convertToArray currentPathNode

