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

type RenderingContext =
    {Renderer:SDLRender.Renderer;
     Texture:SDLTexture.Texture;
     Surface:SDLSurface.Surface;
     mutable LastFrameTick : uint32 }

let updatePositions state = 
    let updateJuan juan = 
        let loc = juan.location + juan.velocity
        { juan with location = loc }
    { 
      state with
        Player1 = updateJuan state.Player1
        Player2 = updateJuan state.Player2
        Mikishidas = List.map updateJuan state.Mikishidas        
    }


let collisionDetection state = 
    let treatTree =
        // create a quadtree of all the treats on the map
        // (we are currently duplicating this work but we might have not jsut treats in this tree in the future, or different tree params
        state.Mikishidas
        |> List.filter(fun k -> match k.kind with Treat -> true | _ -> false)
        |> QuadTree.create (fun j -> j.AsQuadBounds) 5 30 screenQuadBounds
    
    let update (treats,juans) juan =
        match juan.kind with
        | Dragon _ -> 
            let eatenTreats =
                treatTree
                |> QuadTree.findNeighbours (fun k -> overlap(juan.AsRect,k.AsRect)) juan.AsQuadBounds screenQuadBounds
                |> Set.ofList
            // yum yum treats, reset drag 
            Set.union eatenTreats treats, {juan with kind = Dragon(Nothing) } :: juans
        | _ -> treats, juan :: juans
    
    let (treats,juans) = List.fold(fun acc juan -> update acc juan) (Set.empty,[]) state.Mikishidas
    
    let mikis = List.filter (fun t -> Set.contains t treats |> not) juans  
    let lookup = Set.difference state.TreatsLookup (treats|>Set.map(fun t->(int t.location.X, int t.location.Y)))
    // todo - this is mega ineffectient, sort it out!
    {state with Mikishidas = mikis; TreatsLookup = lookup}


let prepareLevel state = 
    // create some dragons and treats
    let mountains = 
        [for y = 30 to 80 do             
            for x = 30 to 40 do
                yield x,y
                yield x+90,y
        ] @
        [for y = 10 to 20 do             
            for x = 50 to 110 do
                yield x,y
                yield x,y + 80]    
        |> Set.ofList
    
 
    let gen n f s =
        let rec aux acc i s =
            if i = n then acc, s else
            let p = randomGridLocation state.Chaos
            if Set.contains p s then aux acc i s
            else aux (f p::acc) (i+1) (Set.add p s)
        aux [] 0 s 

    let toPoint x = {X = double(fst x) * cellWidthf; Y=double(snd x) * cellHeightf}

    let dragons, blocked = gen 25  (fun p -> {kind = MikishidaKinds.Dragon Nothing; location = toPoint p; velocity = {X=0.0;Y=0.0}} ) mountains
    let treatz, _  = gen maxTreats (fun p -> {kind = MikishidaKinds.Treat; location = toPoint p; velocity = {X=0.0;Y=0.0}} ) blocked
    let treatzSet = treatz |> List.map(fun t -> int t.location.X, int t.location.Y ) |> Set.ofList
    let mountains' = mountains |> Set.map(fun p -> {kind = MikishidaKinds.Mountainountain; location = toPoint p; velocity = {X=0.0;Y=0.0}}) |> Set.toList
    
    { state with Mikishidas = dragons @ treatz @ mountains'; UnpassableLookup = mountains; TreatsLookup = treatzSet }

let miscUpdates state = 
    // 60 fps, rotate once every 2 seconds - 120 steps =
    let angle = 
        let angle = state.TurkeyAngle + (360.0 / 120.0)
        if angle > 360.0 then 0. else angle
    
    let (treats, lookups) =
        let toPoint x = {X = double(fst x) * cellWidthf; Y=double(snd x) * cellHeightf}
        let rec aux treats lookups =
            if Set.count lookups = maxTreats then (treats,lookups) else
            let p = randomGridLocation state.Chaos
            if Set.contains p lookups || Set.contains p state.UnpassableLookup then aux treats lookups
            else
                let t = {kind = MikishidaKinds.Treat; location = toPoint p; velocity = {X=0.0;Y=0.0}}
                (t::treats,Set.add p lookups)
        aux [] state.TreatsLookup
        
        
    { state with TurkeyAngle = angle; TreatsLookup = lookups; Mikishidas =  state.Mikishidas @ treats }

let updateInputs state =     
    let controller1 = fun s -> fst s.Controllers
    let controller2 = fun s -> snd s.Controllers
    let buttonPressed f button s = Set.contains button (f s)
    let down1 = buttonPressed controller1
    let down2 = buttonPressed controller2
    let up1 x y = down1 x y |> not
    let up2 x y = down2 x y |> not
    
    // yuuuuck!
    let moves = 
        [
            // WOULDN'T MUTABLE STATE BE MUCH EASIER HMM??? WHAT ARE WE GAINING FROM THIS ???
            // TODO: Clamp bounds
            // TODO: Check new space is occupiable (this is NOT collision detection!)
            (down1 ControllerButton.BUTTON_DPAD_LEFT),  fun state -> { state with Player1 = { state.Player1 with velocity = {X = -state.Player1.kind.defaultSpeed; Y = state.Player1.velocity.Y } } } 
            (down1 ControllerButton.BUTTON_DPAD_RIGHT), fun state -> { state with Player1 = { state.Player1 with velocity = {X = state.Player1.kind.defaultSpeed; Y = state.Player1.velocity.Y } } } 
            (down1 ControllerButton.BUTTON_DPAD_UP),    fun state -> { state with Player1 = { state.Player1 with velocity = {X = state.Player1.velocity.X; Y = -state.Player1.kind.defaultSpeed } } } 
            (down1 ControllerButton.BUTTON_DPAD_DOWN),  fun state -> { state with Player1 = { state.Player1 with velocity = {X = state.Player1.velocity.X; Y = state.Player1.kind.defaultSpeed } } } 
        
            // todo: clean this up!
            (fun s-> up1 ControllerButton.BUTTON_DPAD_LEFT  s
                  && up1 ControllerButton.BUTTON_DPAD_RIGHT s ), fun state -> { state with Player1 = { state.Player1 with velocity = {X =0.0; Y = state.Player1.velocity.Y }} } 
            (fun s-> up1 ControllerButton.BUTTON_DPAD_UP  s
                  && up1 ControllerButton.BUTTON_DPAD_DOWN s ), fun state -> { state with Player1 = { state.Player1 with velocity = {X=state.Player1.velocity.X;Y=0.} } } 

            (down2 ControllerButton.BUTTON_DPAD_LEFT),  fun state -> { state with Player2 = { state.Player2 with velocity = {X = -state.Player2.kind.defaultSpeed; Y = state.Player2.velocity.Y } } } 
            (down2 ControllerButton.BUTTON_DPAD_RIGHT), fun state -> { state with Player2 = { state.Player2 with velocity = {X = state.Player2.kind.defaultSpeed; Y = state.Player2.velocity.Y } } } 
            (down2 ControllerButton.BUTTON_DPAD_UP),    fun state -> { state with Player2 = { state.Player2 with velocity = {X = state.Player2.velocity.X; Y = -state.Player2.kind.defaultSpeed } } } 
            (down2 ControllerButton.BUTTON_DPAD_DOWN),  fun state -> { state with Player2 = { state.Player2 with velocity = {X = state.Player2.velocity.X; Y = state.Player2.kind.defaultSpeed } } } 
            
            (fun s-> up2 ControllerButton.BUTTON_DPAD_LEFT  s
                  && up2 ControllerButton.BUTTON_DPAD_RIGHT s ), fun state -> { state with Player2 = { state.Player2 with velocity = {X =0.0; Y = state.Player2.velocity.Y }} } 
            (fun s-> up2 ControllerButton.BUTTON_DPAD_UP  s
                  && up2 ControllerButton.BUTTON_DPAD_DOWN s ), fun state -> { state with Player2 = { state.Player2 with velocity = {X=state.Player2.velocity.X;Y=0.} } } 
        ]
    (state,moves) ||> List.fold (fun acc (pred,f) -> if pred acc then f acc else acc)

let update (state:TreatzState) : TreatzState =
    state
    |> miscUpdates 
    |> updateInputs
    |> updatePositions
    |> collisionDetection
    |> intelligence
    

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
    
    context.Surface
    |> SDLSurface.fillRect (Some state.Player1.AsRect) {Red=255uy;Green=0uy;Blue=255uy;Alpha=255uy}
    |> ignore

    context.Surface
    |> SDLSurface.fillRect (Some state.Player2.AsRect) {Red=0uy;Green=0uy;Blue=255uy;Alpha=255uy}
    |> ignore

    for j in state.Mikishidas do
        let c = match j.kind with Dragon _-> {Red=255uy;Green=0uy;Blue=0uy;Alpha=255uy}
                                | Treat   -> {Red=0uy;Green=255uy;Blue=0uy;Alpha=255uy}
                                | Mountainountain -> {Red=139uy;Green=69uy;Blue=19uy;Alpha=255uy}
                                | _       -> {Red=255uy;Green=255uy;Blue=255uy;Alpha=255uy}
        context.Surface
        |> SDLSurface.fillRect (Some j.AsRect) c
        |> ignore
    

    context.Texture
    |> SDLTexture.update None context.Surface
    |> ignore

    context.Renderer |> SDLRender.copy context.Texture None None |> ignore
    
    let t = state.Sprites.["turkey"]  
    let dst = { X = 375<px>; Y = 275<px>; Width=50<px>; Height=50<px> } : SDLGeometry.Rectangle    
    context.Renderer  |> copyEx t None (Some dst) state.TurkeyAngle 0 |> ignore
    
    context.Renderer |> SDLRender.present 

    // delay to lock at 60fps (we could do extra work here)
    let frameTime = getTicks() - context.LastFrameTick
    if frameTime < delay_timei then delay(delay_timei - frameTime)
    else printfn "%A" frameTime
    context.LastFrameTick <- getTicks()    

let main() = 
    use system = new SDL.System(SDL.Init.Everything)
    use mainWindow = SDLWindow.create "test" 100<px> 100<px> screenWidth screenHeight 0u
    use mainRenderer = SDLRender.create mainWindow -1 SDLRender.Flags.Accelerated
    use surface = SDLSurface.createRGB (screenWidth,screenHeight,32<bit/px>) (0x00FF0000u,0x0000FF00u,0x000000FFu,0x00000000u)
    
    use bitmap = SDLSurface.loadBmp SDLPixel.RGB888Format @"..\..\..\..\images\turkey.bmp"
    SDLGameController.gameControllerOpen 0
    bitmap
    |> SDLSurface.setColorKey (Some {Red=255uy;Green=0uy;Blue=255uy;Alpha=0uy})
    |> ignore    

    use mainTexture = mainRenderer |> SDLTexture.create SDLPixel.RGB888Format SDLTexture.Access.Streaming (screenWidth,screenHeight)
    mainRenderer |> SDLRender.setLogicalSize (screenWidth,screenHeight) |> ignore

    let turkeyTex = SDLTexture.fromSurface mainRenderer bitmap.Pointer
    
    let sprites = ["turkey", turkeyTex] |> Map.ofList

    let context =  { Renderer = mainRenderer; Texture = mainTexture; Surface = surface; LastFrameTick = getTicks() }
    let state = 
        {Player1 = {kind = Player(PlayerData.Blank); location = {X=10.; Y=10.}; velocity = {X=0.0; Y=0.0}}
         Player2 = {kind = Player(PlayerData.Blank); location = {X=20.; Y=20.}; velocity = {X=0.0; Y=0.0}}
         Mikishidas = []
         UnpassableLookup = Set.empty
         TreatsLookup = Set.empty
         PressedKeys = Set.empty
         Sprites = sprites
         Controllers = Set.empty, Set.empty
         TurkeyAngle = 0.0
         Chaos = System.Random(System.DateTime.Now.Millisecond)} |> prepareLevel

    eventPump (render context) handleEvent update state
        
main()