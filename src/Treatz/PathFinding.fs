module PathFinding

  open System.Collections.Generic
  open System.Linq
  open TreatzGame

  let getNeighbours allNodes node=            
      let x = node.Identity.X
      let y = node.Identity.Y
      let identites = [(1,0); (-1, 0); (0, -1); (0, 1)]
                      |> List.map(fun (o,r) -> 
                          {X= (x + o); Y= (y + r)} )
      [|for i in identites do
        if Map.containsKey (i.X, i.Y) allNodes then
            yield allNodes.[(i.X, i.Y)] |]

  let createInitialFrontier state : HashSet<Node> =
    let f = HashSet<Node>()
    f.Add(state) |> ignore
    f

  // not the most efficient way to calc distance but ...
  let calcDistance (node:Node) goalNode =    
    abs (node.Identity.X - goalNode.Identity.X) + abs( node.Identity.Y - goalNode.Identity.Y) 

  let convertToArray (pathNode :NodePath) =
    let rec someFn n (array: Node array) =
      match n.Parent with
      | None -> array
      | Some parent -> someFn parent (Array.append [|n.GridNode|] array)    
    someFn pathNode Array.empty 
    
  
  let search startNode goal : Node array =
    let frontier = createInitialFrontier startNode 
    let explored = new HashSet<Node>() //mutable    
    let mutable currentPathNode = {Parent = None; PathCost= 0.; GridNode = startNode}    
    
    if (frontier.Count = 0) then 
        [||]
    else  
      while frontier.Count > 0 do      
        let currentNode = 
            frontier.ToArray() 
            |> Seq.minBy(fun x -> calcDistance x goal + x.Cost + int currentPathNode.PathCost )
        currentPathNode <- {currentPathNode with 
                              Parent = Some(currentPathNode) 
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


