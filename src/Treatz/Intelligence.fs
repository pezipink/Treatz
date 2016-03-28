module Intelligence 
  open TreatzGame
  open CommonData
  

  let intelligence (state: TreatzState) =
//      let treatTree =
//          // create a quadtree of all the treats on the map
//          state.Mikishidas
//          |> List.filter(fun k -> match k.kind with Treat -> true | _ -> false)
//          |> QuadTree.create (fun j -> j.AsQuadBounds) 5 5 screenQuadBounds

      let update juan =
          match juan.kind with
          | Dragon(Nothing)  -> 
            // if our dragon is doing nothing, see if we can find a nearby treat within 50 px
            let clamp x = if x < 0 then 0 else x
            let r = juan.AsQuadBounds
            let bounds = {r with x = clamp r.x - 50; y = clamp r.y - 50; width = 100; height = 100; }
            
            state.findMikishidas(fun m -> match m.kind with Treat -> true | _ -> false) bounds
            |> function
                | [] -> // nothing nearby, pick a random direction to roam in (todo)
                    {juan with kind = Dragon(Roam 0); velocity = (0.,0.) }
                | treats -> // find the cloest treat and head towards it
                    let treat = List.minBy(fun treat -> juan.Distance treat) treats
                    let xd = fst treat.location - fst juan.location
                    let yd = snd treat.location - snd juan.location
                    let xd = if xd > 0.0 then juan.kind.defaultSpeed else -juan.kind.defaultSpeed
                    let yd = if yd > 0.0 then juan.kind.defaultSpeed else -juan.kind.defaultSpeed
                    {juan with kind = Dragon(Temporary(treat.location)); velocity = xd,yd}
            
          | Dragon(Roam frames)  -> { juan with kind = Dragon(Roam (frames+1)) }
          | Dragon(Seek data)  -> juan //todo: follow path?
          | Dragon(Temporary(tx,ty)) -> 
              // this really is temporary! jsut to get something moving
              let xd = tx - fst juan.location
              let yd = ty - snd juan.location
              let xd = if xd > 0.0 then juan.kind.defaultSpeed else -juan.kind.defaultSpeed
              let yd = if yd > 0.0 then juan.kind.defaultSpeed else -juan.kind.defaultSpeed
              {juan with velocity = xd,yd}
          | _ -> juan
      { state with Mikishidas = List.map update state.Mikishidas }

