module Intelligence 
  open TreatzGame
  open CommonData
  open Behaviours  
  open SDLUtility
  open System

  let wanderDefault = { 
                      BehaviourState.RateOfChangeOfDirection = 0.5; 
                      BehaviourState.CircleDistance = 1.00 ;  
                      BehaviourState.CircleRadius = 2.50 ; 
                      BehaviourState.WanderingAngle = 0.10; 
                      SteeringDirection = Vector2.Zero }  

  let intelligence (state: TreatzState) =
//      let treatTree =
//          // create a quadtree of all the treats on the map
//          state.Mikishidas
//          |> List.filter(fun k -> match k.kind with Treat -> true | _ -> false)
//          |> QuadTree.create (fun j -> j.AsQuadBounds) 5 5 screenQuadBounds
      let toSeconds (ticks: uint32) = ticks / uint32 1000 

      let update mikishida =                    
          match mikishida.kind with                      
          | Dragon(Nothing)  -> 
            // if our dragon is doing nothing, see if we can find a nearby treat within 50 px
            let clamp x = if x < 0 then 0 else x
            let r = mikishida.AsQuadBounds
            let bounds = {r with x = clamp r.x - 50; y = clamp r.y - 50; width = 100; height = 100; }
            
            state.findMikishidas(fun m -> match m.kind with Treat -> true | _ -> false) bounds
            |> function
                | [] ->                 
                    Console.WriteLine("No treats found, wander")      
                    {mikishida with kind = Dragon(Wander wanderDefault)  }
                | treats -> // find the cloest treat and head towards it
                    let treat = List.minBy(mikishida.ManhattanDistance) treats
                    match treat.kind with 
                    | Treat -> 
                        let xd = treat.location.X - mikishida.location.X
                        let yd = treat.location.Y - mikishida.location.Y
                        let xd = if xd > 0.0 then mikishida.kind.defaultSpeed else -mikishida.kind.defaultSpeed
                        let yd = if yd > 0.0 then mikishida.kind.defaultSpeed else -mikishida.kind.defaultSpeed
                        {mikishida with kind = Dragon(Wander(wanderDefault )); velocity = {X=xd;Y=yd}}
                    | _ -> System.Diagnostics.Debugger.Break(); mikishida
          | Dragon(Wander steering)  ->      
              printfn "wander %A" steering
              let w = wander state.Chaos mikishida steering 
              let v = (getTicks() |> toSeconds |> double) * ( mikishida.kind.defaultSpeed * w.SteeringDirection.normalize )                      
              {mikishida with kind = Dragon(Nothing );velocity = v}

          | Dragon(FollowPath pathTo)  ->              
              printfn "Follow Path %A" pathTo
                         
              if pathTo.Length > 0 then
                {mikishida with location = Array.head pathTo ; kind = Dragon(FollowPath (pathTo |> Array.tail)) }
              else
                {mikishida with kind = Dragon(Wander wanderDefault)}              
          | Dragon(PathFind( treatLocation)) ->               
              printfn "In pathfinding %A" treatLocation
              
              let path = state.PathFindingData 
                          |> Option.map(
                              fun nodes ->
                                let destination = nodes 
                                                  |> Seq.tryFind(fun n -> 
                                                      {X = double treatLocation.GridX; Y = double treatLocation.GridY} = n.Identity) 
                                let origin = nodes 
                                              |> Seq.tryFind(fun n -> {X = double mikishida.location.GridX; Y = double mikishida.location.GridY} = n.Identity) 
                                              |> Option.map(fun x -> x.Neighbours <- PathFinding.getNeighbours nodes x
                                                                     x)
                                match origin, destination with
                                | Some o, Some d -> PathFinding.search o d
                                | _ -> [||])
              let pathVector : Vector2 [] = 
                match path with
                | Some p -> p |> Array.map(fun x-> x.Identity )
                | _ -> [||]
              printfn "The path is %A"  pathVector
              {mikishida with kind = Dragon(FollowPath(pathVector ))}
              
                    
          
          | _ -> mikishida
      printfn "ticks? %A"  (getTicks().ToString())
      { state with Mikishidas = List.map update state.Mikishidas }
      

