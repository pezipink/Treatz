module Intelligence 
  open TreatzGame
  open CommonData
  open Behaviours  
  open SDLUtility
  open System

  let wanderDefault = { 
                      BehaviourState.RateOfChangeOfDirection = 0.1; 
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
      
      let findClosestTreat (mikishida:Mikishida) =
            let clamp x = if x < 0 then 0 else x
            let r = mikishida.AsQuadBounds
            let bounds = {r with x = clamp r.x - 50; y = clamp r.y - 50; width = 100; height = 100; }
            
            state.findMikishidas(fun m -> match m.kind with Treat -> true | _ -> false) bounds
            |> function
                | [] ->                 
                    Console.WriteLine("No treats found, wander")     
                    None
                | treats -> // find the cloest treat and head towards it
                    let treat = List.minBy(mikishida.ManhattanDistance) treats
                    match treat.kind with 
                    | Treat -> Some(treat.location)
                    | _ -> None
                        
      let update mikishida =                    
          match mikishida.kind with                      

          | Dragon(Wander behaviourState)  ->      
              printfn "wander %A" behaviourState
              let newBehaviourState = wander state.Chaos mikishida behaviourState 
              let v =    mikishida.kind.defaultSpeed * newBehaviourState.SteeringDirection.normalize       
              
              match findClosestTreat mikishida with
              | Some treat -> {mikishida with kind = Dragon(PathFind treat  );}
              | _ -> {mikishida with kind = Dragon(Wander newBehaviourState); velocity = v}

          | Dragon(FollowPath pathTo)  ->                           
              if pathTo.Length > 0 then
                let d = {Vector2.X = double (Array.head pathTo).X ; Y = double (Array.head pathTo).Y}
                {mikishida with location = d ; kind = Dragon(FollowPath (pathTo |> Array.tail)) }
              else
                {mikishida with kind = Dragon(Wander wanderDefault)}              
          | Dragon(PathFind( treatLocation)) ->               
              printfn "In pathfinding %A" treatLocation
              let findOrigin (location: Vector2) nodes =
                nodes 
                    |> Seq.tryFind(fun n -> 
                              let x = location.GridX
                              let y = location.GridY 
                              {X= x; Y= y} = n.Identity) 

              let path = state.PathFindingData 
                          |> Option.map(
                              fun nodes ->
                                let destination = nodes 
                                                  |> Seq.tryFind(fun n -> 
                                                      {NodeVector.X = treatLocation.GridX; 
                                                      Y = treatLocation.GridY} = n.Identity) 
                                let origin = 
                                    let mikiLoc = findOrigin mikishida.location nodes                                    
                                    match mikiLoc with
                                    | Some l -> l
                                    | None -> Seq.head nodes

//                                origin.Neighbours <- PathFinding.getNeighbours nodes origin                              
                                match destination with
                                | Some d -> PathFinding.search origin d
                                | _ -> [||])
              let gridPath  = 
                match path with
                | Some p -> p |> Array.map(fun x-> x.Identity )
                | _ -> [||]
              printfn "The path is %A"  gridPath
              {mikishida with kind = Dragon(FollowPath(gridPath ))}
          
          | _ -> mikishida
      printfn "ticks? %A"  (getTicks().ToString())
      { state with Mikishidas = List.map update state.Mikishidas }
