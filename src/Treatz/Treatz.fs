module Treatz

open System
open TreatzGame
open CommonData
open Intelligence

open SDLUtility
open SDLPixel
open SDLRender
open SDLKeyboard
open SDLGameController

let fps = 60.0;
let delay_time = 1000.0 / fps;
let delay_timei = uint32 delay_time

let toSeconds (ticks: uint32) = double ticks / 1000. 

type RenderingContext =
    {Renderer:SDLRender.Renderer;
     Texture:SDLTexture.Texture;
     Surface:SDLSurface.Surface;
     mutable LastFrameTick : uint32 }



let updatePositions (state:TreatzState) = 
    let updateMikishidas mikishida = 
        // the locaiton is measured from the top left corner of the bounding box (or map cell)
        // at any point a sprite could be in up to four cells at once (one for each corner)
        // and check the target is valid, else snap to the nearest grid boundary
      
        let tempLoc = mikishida.location +  mikishida.velocity
        
        //yuck, whatever for now
        match mikishida.kind with 
        | CaughtDragon _ 
        | TreatEaten _ -> Some { mikishida with location = tempLoc }
        | Dragon _ -> 
            
            if Map.containsKey tempLoc.Grid state.Player1.AsPlayerData.Foam then Some { mikishida with kind = Dragon(Wander(Intelligence.wanderDefault)) }
            elif Map.containsKey tempLoc.Grid state.Player2.AsPlayerData.Foam then Some { mikishida with kind = Dragon(Wander(Intelligence.wanderDefault)) }
            else None
        | _ -> None
        |> function
           | None ->
                let newX = 
                    if mikishida.velocity.X > 0.0 && (state.IsCellUnpassable(tempLoc.X + cellWidthf, mikishida.location.Y) || state.IsCellOutofbounds(tempLoc.X + cellWidthf, mikishida.location.Y)) then
                        tempLoc.GridX * cellWidth |> double
                    elif mikishida.velocity.X < 0.0 && (state.IsCellUnpassable(tempLoc.X,mikishida.location.Y)  ||state.IsCellOutofbounds(tempLoc.X,mikishida.location.Y) ) then
                        mikishida.location.GridX * cellWidth |> double
                    else
                        tempLoc.X
        
                let newY = 
                    if mikishida.velocity.Y > 0.0 && (state.IsCellUnpassable(mikishida.location.X, tempLoc.Y + cellHeightf) || state.IsCellOutofbounds(mikishida.location.X, tempLoc.Y + cellHeightf)) then
                        tempLoc.GridY * cellHeight |> double
                    elif mikishida.velocity.Y < 0.0 && (state.IsCellUnpassable(mikishida.location.X,tempLoc.Y)  || state.IsCellOutofbounds(mikishida.location.X,tempLoc.Y)) then
                        mikishida.location.GridY * cellHeight |> double
                    else
                        tempLoc.Y     

                { mikishida with location = {X = newX; Y = newY } }
            | Some state -> state
    { 
      state with
        Player1 = updateMikishidas state.Player1
        Player2 = updateMikishidas state.Player2
        Mikishidas = List.map updateMikishidas state.Mikishidas        
    }


let collisionDetection state = 
    let collisionTree =
        // create a quadtree of all dragons, players and treats
        state.Player1 :: state.Player2 :: state.Mikishidas
        |> List.filter(fun k -> match k.kind with Treat | Player _  -> true | _ -> false)
        |> QuadTree.create (fun j -> j.AsQuadBounds) 5 30 screenQuadBounds
    
    let createAnimation f alpha speed (location:Vector2)  =
        let angle = {currentAngle  = 0.0; alpha  = alpha }
        let vx = state.Chaos.NextDouble() * speed
        let vy = state.Chaos.NextDouble() * speed
        {kind = f angle; 
         location = location; 
         velocity = 
            {X = (if state.Chaos.Next(2) = 0 then vx else -vx); 
            Y = (if state.Chaos.Next(2) = 0 then vy else -vy)}}

    let createDragonCaught = createAnimation CaughtDragon 128 3.0
    let createEatenTreat =  createAnimation TreatEaten 255 8.0
    
    // ross, sort this shit out. signed, ross
    let update (removals,mikis) miki =
        match miki.kind with
        | Dragon _ -> 
            let newCollisions =
                collisionTree
                |> QuadTree.findNeighbours (fun k -> overlap(miki.AsRect,k.AsRect)) miki.AsQuadBounds screenQuadBounds
                
            match newCollisions |> List.tryFind(fun t -> match t.kind with Player _ -> true | _ -> false) with
            | Some p -> 
                // a dragon in contact with a player means the dragon is caught!
                match p.kind with 
                | Player data ->  data.DragonsCaught <- data.DragonsCaught + 1
                | _ -> ()
                // add the dragon to the removal pile and a dragon animation to the caught dragons
                Set.add miki removals, (createDragonCaught p.location) :: mikis
            | None -> 
                // yum yum treats, reset drag to no behaviour
                if newCollisions.IsEmpty then
                    removals,
                    {miki with kind = if newCollisions.Length > 0 then Dragon(Wander(Intelligence.wanderDefault)) else miki.kind } 
                    :: mikis
                else
                    Set.union (Set.ofList newCollisions) removals, 
                        {miki with kind = if newCollisions.Length > 0 then Dragon(Wander(Intelligence.wanderDefault)) else miki.kind } 
                        :: (createEatenTreat miki.location)
                        :: mikis
        | _ -> removals, miki :: mikis
    
    // determine stuff we need to remove and any new additions (caught animations)
    let (removals,juans) = List.fold(fun acc juan -> update acc juan) (Set.empty,[]) state.Mikishidas
    // remove removals from the list
    let mikis = List.filter (fun t -> Set.contains t removals |> not) juans  
    // maintain treats lookup
    let lookup = Set.difference state.TreatsLookup (Set.map(fun t->(t.location.GridX, t.location.GridY))removals)  
    // return new state
    {state with Mikishidas = mikis; TreatsLookup = lookup; SpatialIndex = collisionTree}


let prepareLevel state = 
    // create some dragons and treats
    let mountains = 
        [for y = 10 to 35 do             
            for x = 12 to 16 do
                yield x,y
                yield x+35,y
        ] @
        [for y = 5 to 9 do             
            for x = 19 to 44 do
                yield x,y
                yield x,y + 32] 
        |> Set.ofList
    
    let gen n f s =
        let rec aux acc i s =
            if i = n then acc, s else
            let p = randomGridLocation state.Chaos
            if Set.contains p s then aux acc i s
            else aux (f p::acc) (i+1) (Set.add p s)
        aux [] 0 s 

    let toPoint x = {Vector2.X = double(fst x) * cellWidthf; Y=double(snd x) * cellHeightf}

    let dragons, blocked = gen maxDragons (fun p -> {kind = MikishidaKinds.Dragon( Wander {Intelligence.wanderDefault with RateOfChangeOfDirection = (state.Chaos.NextDouble()/0.5)}) ; location = toPoint p; velocity = {X=0.0;Y=0.0}} ) mountains
    let treatz, _  = gen maxTreats (fun p -> {kind = MikishidaKinds.Treat; location = toPoint p; velocity = {X=0.0;Y=0.0}} ) blocked
    let treatzSet = treatz |> List.map(fun t -> int t.location.GridX, int t.location.GridY ) |> Set.ofList
    let mountains' = mountains |> Set.map(fun p -> {kind = MikishidaKinds.Mountainountain; location = toPoint p; velocity = {X=0.0;Y=0.0}}) |> Set.toList


    
    //setup graph for pathfinding
    let graphForPathfinding obstacles =
        let getIdentity x y = {X= x; Y = y}
        let getCost point obstacles =
          match obstacles |>List.tryFind(fun x -> {NodeVector.X = x.location.GridX; Y= x.location.GridY }= point) with
          | Some _ -> Int32.MaxValue 
          | None -> 1
         
        let createGraph() =
          let allNodes = ResizeArray<(int*int)*Node>()
          for y = 0 to mapHeight - 1 do 
            for x = 0 to mapWidth - 1 do 
              let id = getIdentity x y
              let cost = getCost id obstacles
              if cost < Int32.MaxValue then
                  let newNode = {
                      Node.Identity= id ;
                      Node.Neighbours = Seq.empty; 
                      Cost = getCost id obstacles} 
                  allNodes.Add((x,y),newNode)
              
          Map.ofSeq allNodes

        // Maybe here we can do a thing where instead of checking each grid
        //   you can have a list of non existing nodes and compare against that?
        // this is just a first pass                
        let graph = createGraph()     
        graph
        |> Map.iter(fun _ node -> node.Neighbours <-(PathFinding.getNeighbours graph node))
        graph

    { state with 
          Mikishidas = dragons @ treatz @ mountains'; 
          UnpassableLookup = mountains; 
          TreatsLookup = treatzSet 
          PathFindingData = graphForPathfinding mountains'}

let defaultState(sprites) = 
        {GameState = TitleScreen
         Player1 = {kind = Player(PlayerData.Blank); location = {X=494.; Y=330.}; velocity = {X=0.0; Y=0.0}}
         Player2 = {kind = Player(PlayerData.Blank); location = {X=564.; Y=330.}; velocity = {X=0.0; Y=0.0}}
         Mikishidas = []
         SpatialIndex = QuadTree.Leaf []
         UnpassableLookup = Set.empty
         TreatsLookup = Set.empty
         PressedKeys = Set.empty
         Sprites = sprites
         Controllers = Set.empty, Set.empty
         TurkeyAngle = 0.0
         Chaos = System.Random(System.DateTime.Now.Millisecond)
         PathFindingData = Map.empty
         LastFrameTime = getTicks()
         
         } |> prepareLevel

let miscUpdates state = 
    // 60 fps, rotate once every 2 seconds - 120 steps =
    let angle = 
        let angle = state.TurkeyAngle + (360.0 / 120.0)
        if angle > 360.0 then 0. else angle
    
    // maintain max amount of treats and treat lookup
    let (treats, lookups) =
        let toPoint x = {Vector2.X = double(fst x) * cellWidthf; Y=double(snd x) * cellHeightf}
        let rec aux treats lookups =
            if Set.count lookups = maxTreats then (treats,lookups) else
            let p = randomGridLocation state.Chaos
            if Set.contains p lookups || Set.contains p state.UnpassableLookup || state.IsCellOutofbounds(float(fst p), float(snd p)) then aux treats lookups
            else
                let t = {kind = MikishidaKinds.Treat; location = toPoint p; velocity = {X=0.0;Y=0.0}}
                aux (t::treats) (Set.add p lookups)
        aux [] state.TreatsLookup
    
    let updateDragonFoam (foam:Map<int*int,int>) =
        foam
        |> Map.map(fun _ v -> v + 1)
        |> Map.filter(fun _ v -> v < foamFrames)
        
    // using mutable state here to save record headaches, because why not
    state.Player1.AsPlayerData.Foam <- updateDragonFoam state.Player1.AsPlayerData.Foam 
    state.Player2.AsPlayerData.Foam <- updateDragonFoam state.Player2.AsPlayerData.Foam 

    let mikis = 
        state.Mikishidas 
        |> List.filter(fun m -> 
            match m.kind with 
            // out of bounds caught dragons and eaten treats are removed. also we are going to be cheeky 
            // and in this filter perform and update and sideeffect so we don't have to iterate this
            // list AGAIN!
                
            | CaughtDragon data ->
                if state.IsCellOutofbounds(m.location.X,m.location.Y) || data.alpha = 0 then false 
                else 
                    let angle = data.currentAngle + 10.0
                    data.currentAngle <- if angle > 360.0 then angle - 360.0 else angle
                    true
            | TreatEaten data -> 
                // out of bounds caught dragons and eaten treats are removed. also we are going to be cheeky 
                // and in this filter perform and update and sideeffect so we don't have to iterate this
                // list AGAIN!
                if state.IsCellOutofbounds(m.location.X,m.location.Y) then false 
                else 
                    let angle = data.currentAngle + 20.0
                    data.currentAngle <- if angle > 360.0 then angle - 360.0 else angle
                    data.alpha <- if data.alpha - 5 < 0 then 0 else data.alpha - 5
                    true
            | AntiDragonFoam _ -> 
                if Map.containsKey m.location.Grid state.Player1.AsPlayerData.Foam then true
                elif Map.containsKey m.location.Grid state.Player2.AsPlayerData.Foam then true
                else false 
            | _ -> true )   
    
    { state with TurkeyAngle = angle; TreatsLookup = lookups; Mikishidas =  mikis @ treats; LastFrameTime = getTicks()}

let testGameOver state = 
    match state.GameState with
    | Playing ->
        if state.Mikishidas |> List.filter(fun m -> match m.kind with Dragon _  -> true | _ -> false ) |> List.length = 0 then
            if state.Player1.AsPlayerData.DragonsCaught > state.Player2.AsPlayerData.DragonsCaught then
                { defaultState(state.Sprites) with GameState = Player1Wins}
            else
                { defaultState(state.Sprites) with GameState = Player2Wins}
        else state
    | _ -> state

let tryDropFoam player =
    match player.kind with
    | Player data -> 
        if data.Foam.Count < maxPlayerFoam + data.DragonsCaught && Map.containsKey(player.location.CentreGridX,player.location.CentreGridY) data.Foam = false then
            let f = { kind = AntiDragonFoam (getTicks()); location = {X=float player.location.CentreGridX*cellWidthf; Y=float player.location.CentreGridY*cellHeightf} ; velocity = {X= 0.0; Y = 0.0} }
            Some(f, { player with kind = Player({data with Foam = Map.add (player.location.CentreGridX,player.location.CentreGridY) 0 data.Foam })})
        else None
    | _ -> None
    
let updateInputs state =     
    let controller1 = fun s -> fst s.Controllers
    let controller2 = fun s -> snd s.Controllers
    let buttonPressed f button s = Set.contains button (f s)
    let down1 = buttonPressed controller1
    let down2 = buttonPressed controller2
    let up1 x y = down1 x y |> not
    let up2 x y = down2 x y |> not
    match state.GameState with
    | TitleScreen -> 
        if state.PressedKeys.Count > 0 || (fst state.Controllers).Count > 0 || (snd state.Controllers).Count > 0 then 
            { state with GameState = Playing }
        else 
            state
    | Player1Wins 
    | Player2Wins -> 
        if state.PressedKeys.Contains(ScanCode.Return) then 
            { state with GameState = TitleScreen; PressedKeys = Set.empty }
        else 
            state
    | Playing -> 
        // yuuuuck!
        let moves = 
            [
                // WOULDN'T MUTABLE STATE BE MUCH EASIER HMM??? WHAT ARE WE GAINING FROM THIS ???
                (down1 ControllerButton.BUTTON_DPAD_LEFT),  fun state -> { state with Player1 = { state.Player1 with velocity = {X = -state.Player1.kind.defaultSpeed; Y = state.Player1.velocity.Y } } } 
                (down1 ControllerButton.BUTTON_DPAD_RIGHT), fun state -> { state with Player1 = { state.Player1 with velocity = {X = state.Player1.kind.defaultSpeed; Y = state.Player1.velocity.Y } } } 
                (down1 ControllerButton.BUTTON_DPAD_UP),    fun state -> { state with Player1 = { state.Player1 with velocity = {X = state.Player1.velocity.X; Y = -state.Player1.kind.defaultSpeed } } } 
                (down1 ControllerButton.BUTTON_DPAD_DOWN),  fun state -> { state with Player1 = { state.Player1 with velocity = {X = state.Player1.velocity.X; Y = state.Player1.kind.defaultSpeed } } } 
                (down1 ControllerButton.BUTTON_A), 
                    fun state -> 
                        match tryDropFoam state.Player1 with
                        | Some(foam,player) -> {state with Player1 = player; Mikishidas = foam :: state.Mikishidas }
                        | None -> state
                // todo: clean this up!
                (fun s-> up1 ControllerButton.BUTTON_DPAD_LEFT  s
                      && up1 ControllerButton.BUTTON_DPAD_RIGHT s ), fun state -> { state with Player1 = { state.Player1 with velocity = {X =0.0; Y = state.Player1.velocity.Y }} } 
                (fun s-> up1 ControllerButton.BUTTON_DPAD_UP  s
                      && up1 ControllerButton.BUTTON_DPAD_DOWN s ), fun state -> { state with Player1 = { state.Player1 with velocity = {X=state.Player1.velocity.X;Y=0.} } } 

                (down2 ControllerButton.BUTTON_DPAD_LEFT),  fun state -> { state with Player2 = { state.Player2 with velocity = {X = -state.Player2.kind.defaultSpeed; Y = state.Player2.velocity.Y } } } 
                (down2 ControllerButton.BUTTON_DPAD_RIGHT), fun state -> { state with Player2 = { state.Player2 with velocity = {X = state.Player2.kind.defaultSpeed; Y = state.Player2.velocity.Y } } } 
                (down2 ControllerButton.BUTTON_DPAD_UP),    fun state -> { state with Player2 = { state.Player2 with velocity = {X = state.Player2.velocity.X; Y = -state.Player2.kind.defaultSpeed } } } 
                (down2 ControllerButton.BUTTON_DPAD_DOWN),  fun state -> { state with Player2 = { state.Player2 with velocity = {X = state.Player2.velocity.X; Y = state.Player2.kind.defaultSpeed } } } 
                (down2 ControllerButton.BUTTON_A), 
                    fun state -> 
                        match tryDropFoam state.Player2 with
                        | Some(foam,player) -> {state with Player2 = player; Mikishidas = foam :: state.Mikishidas  }
                        | None -> state
                (fun s-> up2 ControllerButton.BUTTON_DPAD_LEFT  s
                      && up2 ControllerButton.BUTTON_DPAD_RIGHT s ), fun state -> { state with Player2 = { state.Player2 with velocity = {X =0.0; Y = state.Player2.velocity.Y }} } 
                (fun s-> up2 ControllerButton.BUTTON_DPAD_UP  s
                      && up2 ControllerButton.BUTTON_DPAD_DOWN s ), fun state -> { state with Player2 = { state.Player2 with velocity = {X=state.Player2.velocity.X;Y=0.} } } 


            ]
        (state,moves) ||> List.fold (fun acc (pred,f) -> if pred acc then f acc else acc)

let update (state:TreatzState) : TreatzState =
    match state.GameState with
    | Playing -> 
        state
        |> miscUpdates 
        |> updateInputs
        |> updatePositions
        |> collisionDetection
        |> intelligence
        |> testGameOver
    | _ -> 
        state
        |> updateInputs
    

let rec eventPump (renderHandler:'TState->unit) (eventHandler:SDLEvent.Event->'TState->'TState option) (update:'TState->'TState) (state:'TState) : unit =
    match SDLEvent.pollEvent() with
    | Some(SDLEvent.Other(_)) ->   
        eventPump renderHandler eventHandler update state
    | Some event ->
        match state |> eventHandler event with
        | Some newState -> eventPump renderHandler eventHandler update newState
        | None -> ()
    | None -> 
        let state = update state
        renderHandler state
        eventPump renderHandler eventHandler update state


let handleEvent (event:SDLEvent.Event) (state:TreatzState) : TreatzState option =
    match event with
    | SDLEvent.KeyDown keyDetails when keyDetails.Keysym.Scancode = ScanCode.Escape ->
        None
    | SDLEvent.Quit _ -> 
        None
    | SDLEvent.ControllerButtonDown event  ->
        if event.Which = 0 then 
            Some({ state with Controllers = Set.add (enum<ControllerButton>(int event.Button)) (fst state.Controllers), (snd state.Controllers) } )
        else
            Some({ state with Controllers = (fst state.Controllers), Set.add (enum<ControllerButton>(int event.Button))(snd state.Controllers) } )
    | SDLEvent.ControllerButtonUp event  ->
        if event.Which = 0 then 
            Some({ state with Controllers = Set.remove (enum<ControllerButton>(int event.Button)) (fst state.Controllers), (snd state.Controllers) } )
        else
            Some({ state with Controllers = (fst state.Controllers), Set.remove (enum<ControllerButton>(int event.Button))(snd state.Controllers) } )
    | SDLEvent.KeyDown keyDetails -> 
        Some( { state with PressedKeys = Set.add keyDetails.Keysym.Scancode state.PressedKeys} )
    | SDLEvent.KeyUp keyDetails -> 
        Some( { state with PressedKeys = Set.remove keyDetails.Keysym.Scancode state.PressedKeys} )
    | _ -> Some state
        

let render(context:RenderingContext) (state:TreatzState) =
    // clear screen
    context.Renderer |> SDLRender.setDrawColor (0uy,0uy,0uy,0uy) |> ignore
    context.Renderer |> SDLRender.clear |> ignore

    context.Surface
    |> SDLSurface.fillRect None {Red=0uy;Green=0uy;Blue=0uy;Alpha=255uy}
    |> ignore
    
    context.Texture
    |> SDLTexture.update None context.Surface
    |> ignore
    context.Renderer |> SDLRender.copy context.Texture None None |> ignore
     
    match state.GameState with
    | TitleScreen -> 
        let src = { X = 0<px>; Y = 100<px>; Width=1000<px>; Height=100<px> } : SDLGeometry.Rectangle                
        context.Renderer |> copy state.Sprites.["dt"]   None (Some src) |> ignore

        let src = { X = 0<px>; Y = 200<px>; Width=1000<px>; Height=300<px> } : SDLGeometry.Rectangle                
        context.Renderer |> copy state.Sprites.["title"]   None (Some src) |> ignore

        if System.DateTime.Now.Second % 2 = 0 then
            let src = { X = 0<px>; Y = 500<px>; Width=1000<px>; Height=100<px> } : SDLGeometry.Rectangle                
            context.Renderer |> copy state.Sprites.["anykey"]   None (Some src) |> ignore
    | Player1Wins -> 
        context.Renderer |> copy state.Sprites.["win1"]   None None |> ignore
    | Player2Wins -> 
        let src = { X = 0<px>; Y = 0<px>; Width=1024<px>; Height=768<px> } : SDLGeometry.Rectangle                
        context.Renderer |> copy state.Sprites.["win2"]   None None |> ignore

    | Playing ->
        // we can hardcode the grass and mountain rendering !
        // YES I KNOW THIS IS HORRIBLE.  CRY ME A RIVER
        let t = state.Sprites.["tiles"]  
        for y = 0 to mapHeight do
            for x = 0 to mapWidth do
                let x' = x*cellWidth*1<px>
                let y' = y*cellHeight*1<px>
                let dst = { X = x'; Y = y'; Width=16<px>; Height=16<px> }  : SDLGeometry.Rectangle    
            
                // top left mountain tiles
                if( y= 10 && x = 12 ) || (y=10 && x = 12+35) || (y = 5 && x = 19) || (y=5+32 && x = 19)  then
                    let src = { X = 50<px>; Y = 0<px>; Width=16<px>; Height=16<px> } : SDLGeometry.Rectangle                
                    context.Renderer |> copy t (Some src) (Some dst) |> ignore
                // bottom left mountain tiles
                elif( y= 35 && x = 12 ) || (y=35&& x = 12+35) || (y = 9 && x = 19) || (y=9+32 && x = 19)  then
                    let src = { X = 50<px>; Y = 34<px>; Width=16<px>; Height=16<px> } : SDLGeometry.Rectangle                
                    context.Renderer |> copy t (Some src) (Some dst) |> ignore
                // top right mountain tiles
                elif( y= 10 && x = 16 ) || (y=10 && x = 16+35) || (y = 5 && x = 44) || (y=5 && x = 44+32)  then
                    let src = { X = 84<px>; Y = 0<px>; Width=16<px>; Height=16<px> } : SDLGeometry.Rectangle                
                    context.Renderer |> copy t (Some src) (Some dst) |> ignore
                // bottom right mountain tiles
                elif( y= 35 && x = 16 ) || (y=35&& x = 16+35) || (y = 9 && x = 44) || (y=9+32 && x = 44)  then
                    let src = { X = 84<px>; Y = 34<px>; Width=16<px>; Height=16<px> } : SDLGeometry.Rectangle                
                    context.Renderer |> copy t (Some src) (Some dst) |> ignore
                // left mountain tiles
                elif (y >= 10 && y <= 35 && (x = 12 || x = 12+35)) || (x = 19 && ((y >= 5 && y <= 9) || y>=5+32 && y <= 9+32)) then
                    let src = { X = 50<px>; Y = 17<px>; Width=16<px>; Height=16<px> } : SDLGeometry.Rectangle                
                    context.Renderer |> copy t (Some src) (Some dst) |> ignore
                // right mountain tiles
                elif (y >= 10 && y <= 35 && (x = 16 || x = 16+35)) || (x = 44 && ((y >= 5 && y <= 9) || y>=5+32 && y <= 9+32)) then
                    let src = { X = 84<px>; Y = 17<px>; Width=16<px>; Height=16<px> } : SDLGeometry.Rectangle                
                    context.Renderer |> copy t (Some src) (Some dst) |> ignore
                // top mountain tiles
                elif( y= 10 && x >= 12 && x <= 16 ) || ( y= 10 && x >= 12+35 && x <= 16+35 ) ||
                    ( y = 5 && x >= 19 && x <= 44 ) || ( y= 5+32  && x >= 19 && x <= 44 ) then
                    let src = { X = 67<px>; Y = 0<px>; Width=16<px>; Height=16<px> } : SDLGeometry.Rectangle                
                    context.Renderer |> copy t (Some src) (Some dst) |> ignore
                // bottom mountain tiles
                elif( y = 35 && x >= 12 && x <= 16 ) || ( y= 35 && x >= 12+35 && x <= 16+35 ) ||
                    ( y = 9 && x >= 19 && x <= 44 ) || ( y= 9+32  && x >= 19 && x <= 44 ) then
                    let src = { X = 67<px>; Y = 34<px>; Width=16<px>; Height=16<px> } : SDLGeometry.Rectangle                
                    context.Renderer |> copy t (Some src) (Some dst) |> ignore
                // all mountain tiles
                elif (y >= 10 && y <= 35 && x >=12 && x <=16) || (y >= 10 && y <= 35 && x >=12+35 && x <=16+35) || 
                   (y >= 5 && y <= 9 && x >= 19 && x <= 44) || (y >= 5+32 && y <= 9+32 && x >= 19 && x <= 44) then
                    let src = { X = 67<px>; Y = 17<px>; Width=16<px>; Height=16<px> } : SDLGeometry.Rectangle                
                    context.Renderer |> copy t (Some src) (Some dst) |> ignore
                else // everything else is central grass
                    let src = { X = 17<px>; Y = 17<px>; Width=16<px>; Height=16<px> } : SDLGeometry.Rectangle                
                    context.Renderer |> copy t (Some src) (Some dst) |> ignore
            

        for j in state.Mikishidas |> List.sortBy(fun x -> match x.kind with Dragon _ -> 1 | Treat -> 2 | AntiDragonFoam _ -> 3 | _ -> 4) do
            match j.kind with
            | Dragon _ ->     
                let d = state.Sprites.["drag"]  
                let flip = if j.velocity.X > 0.0 then 1 else 0
                context.Renderer  |> copyEx d None (Some j.AsRect) 0.0 flip |> ignore
            | CaughtDragon data ->     
                let d = state.Sprites.["drag"]  
                SDLTexture.setAlpha data.alpha d |> ignore
                context.Renderer  |> copyEx d None (Some j.AsRect) data.currentAngle 0 |> ignore
                SDLTexture.setAlpha 255 d |> ignore
            | TreatEaten data ->     
                let d = state.Sprites.["treat"]  
                SDLTexture.setAlpha data.alpha d |> ignore
                context.Renderer  |> copyEx d None (Some j.AsRect) data.currentAngle 0 |> ignore
                SDLTexture.setAlpha 255 d |> ignore
            | Treat ->     
                let d = state.Sprites.["treat"]  
                context.Renderer  |> copy d None (Some j.AsRect) |> ignore
            | AntiDragonFoam tickPlaced ->
                let d = state.Sprites.["foam"]  
                // work out transparacey from 100 to 255 depending on how long the foam has been in play
                let maxTicks = float(foamFrames * 1000)
                let pct = (float((getTicks() - tickPlaced)) / maxTicks) * 100.0
                let amount = 255 - int((pct * 155.0) + 100.0) + 100
                SDLTexture.setAlpha amount d |> ignore
                context.Renderer  |> copy d None (Some j.AsRect) |> ignore        
            | _ -> () 
    
        let determinePlayerFrame player =   
            if player.velocity.Y = 0.0 && player.velocity.X > 0.0 then 2 // right
            elif player.velocity.Y = 0.0 && player.velocity.X < 0.0 then 3 // left
            elif player.velocity.X = 0.0 && player.velocity.Y < 0.0 then 1 // up
            else 0 // down

        let juanFrame = determinePlayerFrame state.Player1
        let src = { X = juanFrame*16<px>; Y = 0<px>; Width=16<px>; Height=16<px> } : SDLGeometry.Rectangle                
        context.Renderer |> copy state.Sprites.["juan"]   (Some src) (Some state.Player1.AsRect) |> ignore

        let juanitaFrame = determinePlayerFrame state.Player2
        let src = { X = juanitaFrame*16<px>; Y = 0<px>; Width=16<px>; Height=16<px> } : SDLGeometry.Rectangle                
        context.Renderer |> copy state.Sprites.["juanita"]   (Some src) (Some state.Player2.AsRect) |> ignore


        let turkeyTexture = state.Sprites.["turkey"]  
        let dst = { X = 504<px>; Y = 350<px>; Width=50<px>; Height=50<px> } : SDLGeometry.Rectangle    
        context.Renderer  |> copyEx turkeyTexture None (Some dst) state.TurkeyAngle 0 |> ignore
    
        // show the scores by bliting double sized, transparent dragons over each other in the bottom left and right corners
    
        let d = state.Sprites.["drag"]  
        SDLTexture.setAlpha 80 d |> ignore
        for x = 1 to state.Player1.AsPlayerData.DragonsCaught do
        
            let r = 
             { 
              X = (1 + (x * 10)) * 1<px>
              Y = screenHeight - 32<px>
              Width = 32<px>
              Height = 32<px> 
             } : SDLGeometry.Rectangle
            context.Renderer  |> copyEx d None (Some r) 0.0 1 |> ignore

        for x = 1 to state.Player2.AsPlayerData.DragonsCaught do
            let r = 
             { 
              X = ((screenWidth - 32<px>) - (x * 10<px>)) 
              Y = screenHeight - 32<px>
              Width = 32<px>
              Height = 32<px> 
             } : SDLGeometry.Rectangle
            context.Renderer  |> copy d None (Some r) |> ignore

    
        SDLTexture.setAlpha 255 d |> ignore



    // present and fps lock
    context.Renderer |> SDLRender.present 
    
    // delay to lock at 60fps (we could do extra work here)
    let frameTime = getTicks() - context.LastFrameTick
    if frameTime < delay_timei then delay(delay_timei - frameTime)
    else printfn "Frametime %A" frameTime
    context.LastFrameTick <- getTicks()    


let main() = 
    use system = new SDL.System(SDL.Init.Everything)
    use mainWindow = SDLWindow.create "test" 100<px> 100<px> screenWidth screenHeight 0u //(uint32 SDLWindow.Flags.FullScreen)
    //use mainWindow = SDLWindow.create "test" 100<px> 100<px> screenWidth screenHeight (uint32 SDLWindow.Flags.FullScreen) // FULLSCREEN!
    use mainRenderer = SDLRender.create mainWindow -1 SDLRender.Flags.Accelerated
    use surface = SDLSurface.createRGB (screenWidth,screenHeight,32<bit/px>) (0x00FF0000u,0x0000FF00u,0x000000FFu,0x00000000u)
    
    use turkeyBitmap = SDLSurface.loadBmp SDLPixel.RGB888Format @"..\..\..\..\images\turkey.bmp"
    use dragBitmap = SDLSurface.loadBmp SDLPixel.RGB888Format @"..\..\..\..\images\drag.bmp"
    use treatBitmap = SDLSurface.loadBmp SDLPixel.RGB888Format @"..\..\..\..\images\treat.bmp"
    use tilesBitmap = SDLSurface.loadBmp SDLPixel.RGB888Format @"..\..\..\..\images\tiles2.bmp"
    use juanitaBitmap = SDLSurface.loadBmp SDLPixel.RGB888Format @"..\..\..\..\images\juanita.bmp"
    use juanBitmap = SDLSurface.loadBmp SDLPixel.RGB888Format @"..\..\..\..\images\juan.bmp"
    use foamBitmap = SDLSurface.loadBmp SDLPixel.RGB888Format @"..\..\..\..\images\foam.bmp"
    use anykeyBitmap = SDLSurface.loadBmp SDLPixel.RGB888Format @"..\..\..\..\images\anykey.bmp"
    use titleBitmap = SDLSurface.loadBmp SDLPixel.RGB888Format @"..\..\..\..\images\title.bmp"
    use dtBitmap = SDLSurface.loadBmp SDLPixel.RGB888Format @"..\..\..\..\images\DRAGONTREATZ.bmp"
    use win1Bitmap = SDLSurface.loadBmp SDLPixel.RGB888Format @"..\..\..\..\images\1win.bmp"
    use win2Bitmap = SDLSurface.loadBmp SDLPixel.RGB888Format @"..\..\..\..\images\2win.bmp"
    
    SDLGameController.gameControllerOpen 0
    SDLGameController.gameControllerOpen 1

    let setKey bitmap colour =    
        bitmap
        |> SDLSurface.setColorKey (Some colour)
        |> ignore    
    
    let magenta = {Red=255uy;Green=0uy;Blue=255uy;Alpha=0uy}
    
    setKey turkeyBitmap magenta
    setKey dragBitmap magenta
    setKey treatBitmap magenta
    setKey tilesBitmap magenta
    setKey juanitaBitmap magenta
    setKey juanBitmap magenta
    setKey foamBitmap magenta
        
    use mainTexture = mainRenderer |> SDLTexture.create SDLPixel.RGB888Format SDLTexture.Access.Streaming (screenWidth,screenHeight)
    mainRenderer |> SDLRender.setLogicalSize (screenWidth,screenHeight) |> ignore

    let turkeyTex = SDLTexture.fromSurface mainRenderer turkeyBitmap.Pointer
    let dragTex = SDLTexture.fromSurface mainRenderer dragBitmap.Pointer
    let treatTex = SDLTexture.fromSurface mainRenderer treatBitmap.Pointer
    let tilesTex = SDLTexture.fromSurface mainRenderer tilesBitmap.Pointer
    let juanTex = SDLTexture.fromSurface mainRenderer juanBitmap.Pointer
    let juanitaTex = SDLTexture.fromSurface mainRenderer juanitaBitmap.Pointer
    let foamTex = SDLTexture.fromSurface mainRenderer foamBitmap.Pointer
    let titleTex = SDLTexture.fromSurface mainRenderer titleBitmap.Pointer
    let anykeyTex = SDLTexture.fromSurface mainRenderer anykeyBitmap.Pointer
    let dtTex = SDLTexture.fromSurface mainRenderer dtBitmap.Pointer
    let win1Tex = SDLTexture.fromSurface mainRenderer win1Bitmap.Pointer
    let win2Tex = SDLTexture.fromSurface mainRenderer win2Bitmap.Pointer
    let sprites = 
        ["turkey", turkeyTex; 
         "drag", dragTex; 
         "treat", treatTex; 
         "tiles", tilesTex; 
         "juan", juanTex;
         "juanita", juanitaTex;
         "foam", foamTex;
         "title", titleTex;
         "anykey", anykeyTex
         "dt", dtTex
         "win1", win1Tex
         "win2", win2Tex
        ] |> Map.ofList
    
    
    let context =  { Renderer = mainRenderer; Texture = mainTexture; Surface = surface; LastFrameTick = getTicks() }
    let state = defaultState(sprites)

    eventPump (render context) handleEvent update state
        
main()
