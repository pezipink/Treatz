module Treatz
open System
open SDLUtility
open SDLGeometry
open SDLPixel
open SDLRender
open SDLKeyboard

let fps = 60.0;
let delay_time = 1000.0 / fps;
let delay_timei = uint32 delay_time

let screenWidth = 800<px>
let screenHeight = 600<px>

let cellWidth = 5
let cellHeight = 5

let mapWidth = 160
let mapHeight = 120

[<Struct>]
[<CustomEquality>]
[<CustomComparison>]
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
    {speed : int; dragonsCaught : int}
    with static member Blank = {speed = 3; dragonsCaught = 0}

type JuanTypes =
    | Player of PlayerData
    | Dragon
    | Treat
    | Wall
    | Water
    | AntiDragonFoam

let (|Player|_|) = function
    | Player data -> Some data
    | _ -> None
    
type Juan = 
    { kind : JuanTypes; location : FastPoint }

type TreatzState =
    { Player1 : Juan
      Player2 : Juan
      Juans : Juan list
      PressedKeys : Set<ScanCode> 
      Sprites : Map<string, SDLTexture.Texture>
      mutable TurkeyAngle : float}

type RenderingContext =
    {Renderer:SDLRender.Renderer;
     Texture:SDLTexture.Texture;
     Surface:SDLSurface.Surface;
     mutable lastFrameTick : uint32 }

let update (state:TreatzState) : TreatzState =
    let pressed (code:ScanCode) = if state.PressedKeys.Contains code then true else false
    let getSpeed = function Player data -> data.speed | _ -> failwith "!"
    let update (scancode, f) state = if pressed scancode then f state else state
    // yuuuuck!
    let movement = 
        [
        ScanCode.Left , fun state ->  { state with Player2 = { state.Player2 with location = FastPoint(state.Player2.location.X - getSpeed state.Player2.kind, state.Player2.location.Y ) } } 
        ScanCode.Right , fun state ->  { state with Player2 = { state.Player2 with location = FastPoint(state.Player2.location.X + getSpeed state.Player2.kind, state.Player2.location.Y ) } } 
        ScanCode.Up , fun state ->  { state with Player2 = { state.Player2 with location = FastPoint(state.Player2.location.X, state.Player2.location.Y - getSpeed state.Player2.kind) } } 
        ScanCode.Down , fun state ->  { state with Player2 = { state.Player2 with location = FastPoint(state.Player2.location.X, state.Player2.location.Y + getSpeed state.Player2.kind) } } 
        ScanCode.A , fun state ->  { state with Player1 = { state.Player1 with location = FastPoint(state.Player1.location.X - getSpeed state.Player1.kind, state.Player1.location.Y ) } } 
        ScanCode.D , fun state ->  { state with Player1 = { state.Player1 with location = FastPoint(state.Player1.location.X + getSpeed state.Player1.kind, state.Player1.location.Y ) } } 
        ScanCode.W , fun state ->  { state with Player1 = { state.Player1 with location = FastPoint(state.Player1.location.X, state.Player1.location.Y - getSpeed state.Player1.kind ) } } 
        ScanCode.S , fun state ->  { state with Player1 = { state.Player1 with location = FastPoint(state.Player1.location.X, state.Player1.location.Y + getSpeed state.Player1.kind) } } 
        ]
    (state,movement) ||> List.fold (fun acc f -> update f acc)

let rec eventPump (renderHandler:'TState->unit) (eventHandler:SDLEvent.Event->'TState->'TState option) (update:'TState->'TState) (state:'TState) : unit =
    match SDLEvent.pollEvent() with
    | Some event ->
        match state |> eventHandler event with
        | Some newState -> eventPump renderHandler eventHandler update newState
        | None -> ()
    | None -> 
        let state = update state
        state
        |> renderHandler
        eventPump renderHandler eventHandler update state


let handleEvent (event:SDLEvent.Event) (state:TreatzState) : TreatzState option =
    match event with
    | SDLEvent.KeyDown keyDetails when keyDetails.Keysym.Scancode = ScanCode.Escape ->
        None
    | SDLEvent.Quit _ -> 
        None
    | SDLEvent.KeyDown keyDetails -> 
        Some( { state with PressedKeys = Set.add keyDetails.Keysym.Scancode state.PressedKeys} )
    | SDLEvent.KeyUp keyDetails -> 
        Some( { state with PressedKeys = Set.remove keyDetails.Keysym.Scancode state.PressedKeys} )
    | _ -> Some state
        // core logic function here
        

let render(context:RenderingContext) (state:TreatzState) =
    // clear screen
    context.Renderer |> SDLRender.setDrawColor (0uy,0uy,0uy,0uy) |> ignore
    context.Renderer |> SDLRender.clear |> ignore

    context.Surface
    |> SDLSurface.fillRect None {Red=0uy;Green=0uy;Blue=0uy;Alpha=255uy}
    |> ignore
    
    context.Surface
    |> SDLSurface.fillRect (Some {X=(state.Player1.location.X)*1<px>;Y=(state.Player1.location.Y)*1<px>;Width=25<px>;Height=25<px>}) {Red=255uy;Green=0uy;Blue=255uy;Alpha=255uy}
    |> ignore

    context.Surface
    |> SDLSurface.fillRect (Some {X=(state.Player2.location.X)*1<px>;Y=(state.Player2.location.Y)*1<px>;Width=25<px>;Height=25<px>}) {Red=0uy;Green=0uy;Blue=255uy;Alpha=255uy}
    |> ignore

    context.Texture
    |> SDLTexture.update None context.Surface
    |> ignore

    context.Renderer |> SDLRender.copy context.Texture None None |> ignore
    
    let t = state.Sprites.["turkey"]  
    let dst = { X = 350<px>; Y = 250<px>; Width=50<px>; Height=50<px> }
    // 60 fps, rotate once every 2 seconds - 120 steps =
    let angle = state.TurkeyAngle + (360.0 / 120.0)
    if angle > 360.0 then state.TurkeyAngle <- 0. else state.TurkeyAngle <- angle
    context.Renderer  |> copyEx t None (Some dst) state.TurkeyAngle 0 |> ignore
    
    context.Renderer |> SDLRender.present 

    // delay to lock at 60fps (we could do extra work here)
    let frameTime = getTicks() - context.lastFrameTick
    if frameTime < delay_timei then delay(delay_timei - frameTime)
    context.lastFrameTick <- getTicks()    

let main() = 
    use system = new SDL.System(SDL.Init.Video ||| SDL.Init.Events)
    use mainWindow = SDLWindow.create "test" 100<px> 100<px> screenWidth screenHeight 0u
    use mainRenderer = SDLRender.create mainWindow -1 SDLRender.Flags.Accelerated
    use surface = SDLSurface.createRGB (screenWidth,screenHeight,32<bit/px>) (0x00FF0000u,0x0000FF00u,0x000000FFu,0x00000000u)
    
    use bitmap = SDLSurface.loadBmp SDLPixel.RGB888Format @"..\..\..\..\images\turkey.bmp"

    bitmap
    |> SDLSurface.setColorKey (Some {Red=255uy;Green=0uy;Blue=255uy;Alpha=0uy})
    |> ignore

    

    use mainTexture = mainRenderer |> SDLTexture.create SDLPixel.RGB888Format SDLTexture.Access.Streaming (screenWidth,screenHeight)
    mainRenderer |> SDLRender.setLogicalSize (screenWidth,screenHeight) |> ignore

    let turkeyTex = SDLTexture.fromSurface mainRenderer bitmap.Pointer
    
    let sprites = ["turkey", turkeyTex] |> Map.ofList

    let context =  { Renderer = mainRenderer; Texture = mainTexture; Surface = surface; lastFrameTick = getTicks() }
    let state = 
        {Player1 = {kind = Player(PlayerData.Blank); location = FastPoint(10,10)}
         Player2 = {kind = Player(PlayerData.Blank); location = FastPoint(20,20)}
         Juans = []
         PressedKeys = Set.empty
         Sprites = sprites
         TurkeyAngle = 0.0 }

    eventPump (render context) handleEvent update state
        
main()