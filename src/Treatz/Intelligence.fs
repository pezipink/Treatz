module Intelligence 
  open TreatzGame
  open CommonData
  open Behaviours  
  open SDLUtility
  open System

  let intelligence (state: TreatzState) =
//      let treatTree =
//          // create a quadtree of all the treats on the map
//          state.Mikishidas
//          |> List.filter(fun k -> match k.kind with Treat -> true | _ -> false)
//          |> QuadTree.create (fun j -> j.AsQuadBounds) 5 5 screenQuadBounds

      let toSeconds (ticks: uint32) = ticks / uint32 1000 
      let wanderDefault = { 
                      BehaviourState.RateOfChangeOfDirection = 0.5; 
                      BehaviourState.CircleDistance = 1.00 ;  
                      BehaviourState.CircleRadius = 2.50 ; 
                      BehaviourState.WanderingAngle = 0.10; 
                      SteeringDirection = Vector2.Zero }  

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
                        {mikishida with kind = Dragon(Temporary(treat.location)); velocity = {X=xd;Y=yd}}
                    | _ -> System.Diagnostics.Debugger.Break(); mikishida
          | Dragon(Wander steering)  ->               
              let w = wander state.Chaos mikishida steering 
              let v = (getTicks() |> toSeconds |> double) * ( mikishida.kind.defaultSpeed * w.SteeringDirection.normalize )                      
              {mikishida with kind = Dragon(Wander w);velocity = v}
          | Dragon(Seek treat)  ->               
              //todo: follow path?
              
              {mikishida with kind = Dragon(Wander wanderDefault)  }
          | Dragon(Temporary(treatLocation)) -> 
              Console.WriteLine ("in temporary {0}", treatLocation)
//              {mikishida with kind = Dragon(Wander wanderDefault)  }

             // this really is temporary! jsut to get something moving
              let xd = treatLocation.X - mikishida.location.X
              let yd = treatLocation.Y - mikishida.location.Y
              let xd = if xd > 0.0 then mikishida.kind.defaultSpeed else -mikishida.kind.defaultSpeed
              let yd = if yd > 0.0 then mikishida.kind.defaultSpeed else -mikishida.kind.defaultSpeed
              let x = {X=xd;Y=yd}
              
              {mikishida with velocity = {X=xd;Y=yd}}
              
          | Dragon(PathFind( node)) -> 
              printfn "In pathfinding %A" node
              mikishida      
          | _ -> mikishida
      { state with Mikishidas = List.map update state.Mikishidas }

