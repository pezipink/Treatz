module Intelligence 
  open TreatzGame
  open CommonData
  open Behaviours  
  open SDLUtility

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
                    let steering = { 
                      BehaviourState.RateOfChangeOfDirection = 1.5; 
                      BehaviourState.CircleDistance = 50.0 ;  
                      BehaviourState.CircleRadius = 35.0 ; 
                      BehaviourState.WanderingAngle = 0.0; 
                      SteeringDirection = Point.Zero }    
                    {mikishida with kind = Dragon(Wander steering)  }
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
                  let v = (getTicks() |> toSeconds |> double) * w.SteeringDirection
//                          |> Point.Truncate mikishida.kind.defaultSpeed

                  {mikishida with kind = Dragon(Wander w);velocity = v}

          
          | Dragon(Seek data)  -> mikishida //todo: follow path?
          | Dragon(Temporary(p)) -> 
             // this really is temporary! jsut to get something moving
              let xd = p.X - mikishida.location.X
              let yd = p.Y - mikishida.location.Y
              let xd = if xd > 0.0 then mikishida.kind.defaultSpeed else -mikishida.kind.defaultSpeed
              let yd = if yd > 0.0 then mikishida.kind.defaultSpeed else -mikishida.kind.defaultSpeed
              {mikishida with velocity = {X=xd;Y=yd}}
          | _ -> mikishida
      { state with Mikishidas = List.map update state.Mikishidas }

