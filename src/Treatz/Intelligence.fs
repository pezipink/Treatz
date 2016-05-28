module Intelligence 
  open TreatzGame
  open CommonData
  open Behaviours  
  open SDLUtility
  open System
  open QuadTree

  let wanderDefault = { 
                      BehaviourState.RateOfChangeOfDirection = 0.1; 
                      BehaviourState.CircleDistance = 1.00 ;  
                      BehaviourState.CircleRadius = 2.50 ; 
                      BehaviourState.WanderingAngle = 0.10; 
                      SteeringDirection = Vector2.Zero }  

  let intelligence (state: TreatzState) =
      
      let findClosestTreat (mikishida:Mikishida) =
            let clamp x = if x < 0 then 0 else x
            let r = mikishida.AsQuadBounds
            let bounds = {r with x = clamp r.x - 25; y = clamp r.y - 25; width = 50; height = 50; }
//            let bounds = {r with x = clamp r.x - 150; y = clamp r.y - 150; width = 300; height = 300; }
            
            state.SpatialIndex 
            |> QuadTree.findNeighbours(fun m -> match m.kind with Treat -> true | _ -> false) bounds screenQuadBounds
//            state.findMikishidas(fun m -> match m.kind with Treat -> true | _ -> false) bounds
            |> function
                | [] ->                 
//                    Console.WriteLine("No treats found, wander")     
                    None
                | treats -> // find the cloest treat and head towards it
                    let treat = List.minBy(mikishida.ManhattanDistance) treats
                    match treat.kind with 
                    | Treat -> Some(treat.location)
                    | _ -> None
                        
      let update mikishida =                    
          match mikishida.kind with                      

          | Dragon(Wander behaviourState)  ->      

              let newBehaviourState = wander state.Chaos mikishida behaviourState 
              let velocity = mikishida.kind.defaultSpeed * newBehaviourState.SteeringDirection.normalize       
              match findClosestTreat mikishida with
              | Some treat -> {mikishida with kind = Dragon(PathFind treat); }
              | _ -> {mikishida with kind = Dragon(Wander newBehaviourState); velocity = velocity}

          | Dragon(FollowPath(pathTo, dest))  ->   
              // if treat has gone, do something else              
                if pathTo.Length > 0  &&  Set.contains (dest.GridX, dest.GridY) state.TreatsLookup then
                    let destinationCell = Array.head pathTo
                    // move towards the cetre of the destination cell, check if we are there yet by looking 
                    // where the centre of the dragon is.
                    let destinationCentre = 
                        (double destinationCell.Column) * cellWidthf/1.0<px> + (cellWidthf / 2.0<px>),
                        (double destinationCell.Row) * cellHeightf/1.0<px> + (cellHeightf / 2.0<px>)
                
                    let dragonCentre = 
                        (double mikishida.location.X + (cellWidthf / 2.0<px>)) ,
                        (double mikishida.location.Y + (cellHeightf / 2.0<px>))
                
                    let destinationCell = int (fst destinationCentre / cellWidthf), int (snd destinationCentre / cellHeightf)
                    // top left
                    let dragonCell1 = int (mikishida.location.X / cellWidthf), int (mikishida.location.Y / cellHeightf) 
                    // top right
                    let dragonCell2 = int ((mikishida.location.X + cellWidthf/1.0<px>) / cellWidthf), int (mikishida.location.Y / cellHeightf)
                    // bottom left
                    let dragonCell3 = int ((mikishida.location.X) / cellWidthf), int ((mikishida.location.Y + cellHeightf/1.0<px>) / cellHeightf)
                    // bottom right
                    let dragonCell4 = int ((mikishida.location.X + cellWidthf/1.0<px>) / cellWidthf), int ((mikishida.location.Y + cellHeightf/1.0<px>) / cellHeightf)

                    if destinationCell = dragonCell1 ||  destinationCell = dragonCell2 || destinationCell = dragonCell3 || destinationCell = dragonCell4 then 
                      {mikishida with kind = Dragon(FollowPath (pathTo |> Array.tail,dest)) }
                    else                        
                        let target = {Vector2.X = fst destinationCentre - fst dragonCentre ; Y = snd destinationCentre - snd dragonCentre}.normalize
                        let velocity = mikishida.kind.defaultSpeed * target
                        { mikishida with velocity = velocity}
                        
                
                else {mikishida with kind = Dragon(Wander wanderDefault); }              
              
          | Dragon(PathFind( treatLocation)) ->              
              
              let rec getNode loc =
                let r = Map.tryFind loc state.PathFindingData                    
                match r with
                | Some node-> Some(node)
                | None  -> 
                      let x, y = loc
                      let x', y' = state.Chaos.Next(-1,2), state.Chaos.Next(-1,2)
                      getNode (x+x', y+y') 
                
              let destinationNode = getNode (treatLocation.GridX,treatLocation.GridY) 
              let origin = getNode(mikishida.location.GridX,mikishida.location.GridY) 
              
              match destinationNode, origin with
              | Some destination, Some origin ->
                  let gridPath = [| yield! PathFinding.search origin destination ; yield destination |]
                  {mikishida with kind = Dragon(FollowPath(gridPath |> Array.map(fun x -> x.Identity), treatLocation))}
              |  _ -> printfn "this should never happen, wtf"
                      {mikishida with kind = Dragon(Wander wanderDefault)}
          | _ -> mikishida

      { state with Mikishidas = List.map update state.Mikishidas }
