module Intelligence 

  open TreatzGame
  open CommonData
  

  let intelligence (state: TreatzState) =
      let treatTree =
          // create a quadtree of all the treats on the map
          state.Juans
          |> List.filter(fun k -> match k.kind with Treat -> true | _ -> false)
          |> QuadTree.create (fun j -> j.AsQuadBounds) 5 5 screenQuadBounds

      let update juan =
          match juan.kind with
          | Dragon(Nothing)  -> 
              // if our dragon is doing nothing, see if we can find a nearby treat
              treatTree
              |> QuadTree.findNeighbours (fun _ -> true) juan.AsQuadBounds screenQuadBounds
              |> function
                 | [] -> // nothing nearby, pick a random direction to roam in (todo)
                      {juan with kind = Dragon(Roam 0)}
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
      { state with Juans = List.map update state.Juans }

