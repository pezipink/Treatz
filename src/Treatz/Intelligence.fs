module Intelligence 

  open TreatzGame
  open CommonData
  

  let intelligence (state: TreatzState) =
      let treatTree =
          // create a quadtree of all the treats on the map
          state.Juans
          |> List.filter(fun k -> match k.kind with Treat -> true | _ -> false)
          |> QuadTree.create (fun j -> j.AsQuadBounds) 5 5 screenQuadBounds

      let update mikishida =
          match mikishida.kind with
          | Dragon(Nothing)  -> 
              // if our dragon is doing nothing, see if we can find a nearby treat
              treatTree
              |> QuadTree.findNeighbours (fun _ -> true) mikishida.AsQuadBounds screenQuadBounds
              |> function
                 | [] -> // nothing nearby, pick a random direction to roam in (todo)
                      {mikishida with kind = Dragon(Roam 0)}
                 | treats -> // find the cloest treat and head towards it
                     let treat = List.minBy(fun treat -> mikishida.Distance treat) treats
                     let xd = fst treat.location - fst mikishida.location
                     let yd = snd treat.location - snd mikishida.location
                     let xd = if xd > 0.0 then mikishida.kind.defaultSpeed else -mikishida.kind.defaultSpeed
                     let yd = if yd > 0.0 then mikishida.kind.defaultSpeed else -mikishida.kind.defaultSpeed
                     {mikishida with kind = Dragon(Temporary(treat.location)); velocity = xd,yd}
            
          | Dragon(Roam frames)  -> { mikishida with kind = Dragon(Roam (frames + 1)) }
          | Dragon(Seek data)  -> mikishida //todo: follow path?
          | Dragon(Temporary(tx,ty)) -> 
              // this really is temporary! jsut to get something moving
              let xd = tx - fst mikishida.location
              let yd = ty - snd mikishida.location
              let xd = if xd > 0.0 then mikishida.kind.defaultSpeed else -mikishida.kind.defaultSpeed
              let yd = if yd > 0.0 then mikishida.kind.defaultSpeed else -mikishida.kind.defaultSpeed
              {mikishida with velocity = xd,yd}
          | _ -> mikishida
      { state with Juans = List.map update state.Juans }

