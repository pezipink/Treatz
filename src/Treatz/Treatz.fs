module Treatz
open System
open SDLUtility
open SDLGeometry
open SDLPixel
open SDLRender
open SDLKeyboard
open SDLGameController

let fps = 60.0;
let delay_time = 1000.0 / fps;
let delay_timei = uint32 delay_time

let screenWidth = 800<px>
let screenHeight = 600<px>

let cellWidth = 5
let cellHeight = 5
let cellWidthf = 5.0
let cellHeightf = 5.0

let mapWidth = 160
let mapHeight = 120
let mapWidthf = 160.0
let mapHeightf = 120.0

let screenQuadBounds =
    { x = 0; y = 0; width = int screenWidth; height = int screenHeight }  : QuadTree.QuadBounds

[<Struct>]
[<CustomEquality>]
[<CustomComparison>]
// used in the quadtrees and pathfinding algorithms 
type FastPoint(x: int, y: int) =
    member __.X = x
    member __.Y = y
    
    override lhs.Equals rhs =
        let rhs = (rhs :?> FastPoint)
        lhs.X = lhs.X && lhs.Y = rhs.X
    
    interface System.IComparable with
        member lhs.CompareTo rhs =
            let rhs = (rhs :?> FastPoint)
            let c = compare lhs.X rhs.X
            if c <> 0 then c else
            compare lhs.Y rhs.Y
    
    override p.GetHashCode() =
        x + 65536 * y

    member __.GridX = x / cellWidth
    member __.GridY = y / cellHeight
    

type PlayerData = 
    {dragonsCaught : int}
    with static member Blank = {dragonsCaught = 0}

type DragonData =
    | Nothing
    | Roam of roamingFrames : int 
    | Seek of (double * double) list
    | Temporary of treat : double * double // no really, this is going

type JuanTypes =
    | Player of PlayerData
    | Dragon of DragonData
    | Treat
    | Wall
    | Water
    | AntiDragonFoam
    | Squirrel
    | Cat
    | Otter
    with 
    member this.defaultSpeed =
        match this with
        | Player _ -> 3.0
        | Dragon _  -> 5.0
        | _ -> 0.9

let (|Player|_|) = function
    | Player data -> Some data
    | _ -> None

    
type Juan = 
    { kind : JuanTypes; location : double * double; velocity : double * double }
    with 
    member this.size =
        match this.kind with
        | Treat -> 5<px>, 5<px>
        | _ -> 10<px>, 10<px>

    member this.asRect = 
        let w, h = this.size
        { 
          X = (fst this.location |> int)*1<px> 
          Y = (snd this.location |> int)*1<px>
          Width = w
          Height = h 
        }
    member this.asQuadBounds : QuadTree.QuadBounds = 
        let w, h = this.size
        { 
          x = ((fst this.location) ) |> int
          y = ((snd this.location) ) |> int
          width = int w 
          height = int h
        }
    member this.Distance(other:Juan) =
        let xd = fst other.location - fst this.location
        let yd = snd other.location - snd this.location
        sqrt(xd*xd+yd*yd)

    member this.ManhattanDistance(other:Juan) =
        abs(fst other.location - fst this.location) + abs(snd other.location - snd this.location)


type TreatzState =
    { Player1 : Juan
      Player2 : Juan
      Juans : Juan list
//      StaticLookup : Set<double*double> // set of immovable Juans locations for fast lookup
      PressedKeys : Set<ScanCode> 
      Controllers : Set<ControllerButton> * Set<ControllerButton>
      Sprites : Map<string, SDLTexture.Texture>
      TurkeyAngle : float
      Chaos : System.Random
      }

type RenderingContext =
    {Renderer:SDLRender.Renderer;
     Texture:SDLTexture.Texture;
     Surface:SDLSurface.Surface;
     mutable lastFrameTick : uint32 }

let updatePositions state = 
    let updateJuan juan = 
        let loc = (fst juan.location) + fst(juan.velocity),(snd juan.location) + snd(juan.velocity)
        { juan with location = loc }
    { 
      state with
        Player1 = updateJuan state.Player1
        Player2 = updateJuan state.Player2
        Juans = List.map updateJuan state.Juans        
    }

let overlap(rectA, rectB) =
    let xa2,xb2 = rectA.X + rectA.Width,rectB.X + rectB.Width
    let ya2,yb2 = rectA.Y + rectA.Height,rectB.Y + rectB.Height
    rectA.X < xb2 && xa2 > rectB.X && 
    rectA.Y < yb2 && ya2 > rectB.Y

let collisionDetection state = 
    let treatTree =
        // create a quadtree of all the treats on the map
        // (we are currently duplicating this work but we might have not jsut treats in this tree in the future, or different tree params
        state.Juans
        |> List.filter(fun k -> match k.kind with Treat -> true | _ -> false)
        |> QuadTree.create (fun j -> j.asQuadBounds) 3 10 screenQuadBounds

    let update (treats,juans) juan =
        match juan.kind with
        | Dragon _ -> 
            let eatenTreats =
                treatTree
                |> QuadTree.findNeighbours (fun _ -> true) juan.asQuadBounds screenQuadBounds
                |> List.filter(fun treat -> overlap(juan.asRect,treat.asRect))
            // yum yum treats, reset drag 
            eatenTreats @ treats, {juan with kind = Dragon(Nothing) } :: juans
        | _ -> treats, juan :: juans
    
    let (treats,juans) = List.fold(fun acc juan -> update acc juan) ([],[]) state.Juans
    
    // todo - this is mega ineffectient, sort it out!
    {state with Juans = List.filter (fun j -> List.contains j treats |> not) juans  }

let intelligence state =
    let treatTree =
        // create a quadtree of all the treats on the map
        state.Juans
        |> List.filter(fun k -> match k.kind with Treat -> true | _ -> false)
        |> QuadTree.create (fun j -> j.asQuadBounds) 5 5 screenQuadBounds

    let update juan =
        match juan.kind with
        | Dragon(Nothing)  -> 
            // if our dragon is doing nothing, see if we can find a nearby treat
            treatTree
            |> QuadTree.findNeighbours (fun _ -> true) juan.asQuadBounds screenQuadBounds
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

let prepareLevel state = 
    // create some dragons and treats
    let randomLocation (chaos:System.Random) =
        (chaos.NextDouble()) * 800.0, (chaos.NextDouble()) * 600.0
    
    // todo: don't let stuff overlap, with an extra margin
    let dragons = [for _ in 1..10 -> {kind = JuanTypes.Dragon Nothing; location = randomLocation state.Chaos; velocity = (0.0,0.0)} ]
    let treatz =  [for _ in 1..250 -> {kind = JuanTypes.Treat;  location = randomLocation state.Chaos; velocity = (0.0,0.0)} ]
    
    { state with Juans = dragons @ treatz }

let miscUpdates state = 
    // 60 fps, rotate once every 2 seconds - 120 steps =
    let angle = 
        let angle = state.TurkeyAngle + (360.0 / 120.0)
        if angle > 360.0 then 0. else angle
    { state with TurkeyAngle = angle }

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
            (down1 ControllerButton.BUTTON_DPAD_LEFT),  fun state -> { state with Player1 = { state.Player1 with velocity = (-state.Player1.kind.defaultSpeed,snd state.Player1.velocity) } } 
            (down1 ControllerButton.BUTTON_DPAD_RIGHT), fun state -> { state with Player1 = { state.Player1 with velocity = (state.Player1.kind.defaultSpeed,snd state.Player1.velocity) } } 
            (down1 ControllerButton.BUTTON_DPAD_UP),    fun state -> { state with Player1 = { state.Player1 with velocity = (fst state.Player1.velocity,-state.Player1.kind.defaultSpeed) } } 
            (down1 ControllerButton.BUTTON_DPAD_DOWN),  fun state -> { state with Player1 = { state.Player1 with velocity = (fst state.Player1.velocity,state.Player1.kind.defaultSpeed) } } 
        
            // todo: clean this up!
            (fun s-> up1 ControllerButton.BUTTON_DPAD_LEFT  s
                  && up1 ControllerButton.BUTTON_DPAD_RIGHT s ), fun state -> { state with Player1 = { state.Player1 with velocity = (0.0,snd state.Player1.velocity) } } 
            (fun s-> up1 ControllerButton.BUTTON_DPAD_UP  s
                  && up1 ControllerButton.BUTTON_DPAD_DOWN s ), fun state -> { state with Player1 = { state.Player1 with velocity = (fst state.Player1.velocity,0.0) } } 

            (down2 ControllerButton.BUTTON_DPAD_LEFT),  fun state -> { state with Player2 = { state.Player2 with velocity = (-state.Player2.kind.defaultSpeed,snd state.Player2.velocity) } } 
            (down2 ControllerButton.BUTTON_DPAD_RIGHT), fun state -> { state with Player2 = { state.Player2 with velocity = (state.Player2.kind.defaultSpeed,snd state.Player2.velocity) } } 
            (down2 ControllerButton.BUTTON_DPAD_UP),    fun state -> { state with Player2 = { state.Player2 with velocity = (fst state.Player2.velocity,-state.Player2.kind.defaultSpeed) } } 
            (down2 ControllerButton.BUTTON_DPAD_DOWN),  fun state -> { state with Player2 = { state.Player2 with velocity = (fst state.Player2.velocity,state.Player2.kind.defaultSpeed) } } 

            (fun s-> up2 ControllerButton.BUTTON_DPAD_LEFT  s
                  && up2 ControllerButton.BUTTON_DPAD_RIGHT s ), fun state -> { state with Player2 = { state.Player2 with velocity = (0.0,snd state.Player2.velocity) } } 
            (fun s-> up2 ControllerButton.BUTTON_DPAD_UP  s
                  && up2 ControllerButton.BUTTON_DPAD_DOWN s ), fun state -> { state with Player2 = { state.Player2 with velocity = (fst state.Player2.velocity,0.0) } } 

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
    |> SDLSurface.fillRect (Some state.Player1.asRect) {Red=255uy;Green=0uy;Blue=255uy;Alpha=255uy}
    |> ignore

    context.Surface
    |> SDLSurface.fillRect (Some state.Player2.asRect) {Red=0uy;Green=0uy;Blue=255uy;Alpha=255uy}
    |> ignore

    for j in state.Juans do
        let c = match j.kind with Dragon _-> {Red=255uy;Green=0uy;Blue=0uy;Alpha=255uy}
                                | Treat   -> {Red=0uy;Green=255uy;Blue=0uy;Alpha=255uy}
                                | _       -> {Red=255uy;Green=255uy;Blue=255uy;Alpha=255uy}
        context.Surface
        |> SDLSurface.fillRect (Some j.asRect) c
        |> ignore
    

    context.Texture
    |> SDLTexture.update None context.Surface
    |> ignore

    context.Renderer |> SDLRender.copy context.Texture None None |> ignore
    
    let t = state.Sprites.["turkey"]  
    let dst = { X = 375<px>; Y = 275<px>; Width=50<px>; Height=50<px> }    
    context.Renderer  |> copyEx t None (Some dst) state.TurkeyAngle 0 |> ignore
    
    context.Renderer |> SDLRender.present 

    // delay to lock at 60fps (we could do extra work here)
    let frameTime = getTicks() - context.lastFrameTick
    if frameTime < delay_timei then delay(delay_timei - frameTime)
    context.lastFrameTick <- getTicks()    

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

    let context =  { Renderer = mainRenderer; Texture = mainTexture; Surface = surface; lastFrameTick = getTicks() }
    let state = 
        {Player1 = {kind = Player(PlayerData.Blank); location = (10.,10.); velocity = (0.0,0.0)}
         Player2 = {kind = Player(PlayerData.Blank); location = (20.,20.); velocity = (0.0,0.0)}
         Juans = []
         PressedKeys = Set.empty
         Sprites = sprites
         Controllers = Set.empty, Set.empty
         TurkeyAngle = 0.0
         Chaos = System.Random(System.DateTime.Now.Millisecond)} |> prepareLevel

    eventPump (render context) handleEvent update state
        
main()