module Treatz
open System
open SDLUtility
open SDLGeometry
open SDLPixel
open SDLRender


let fps = 60.0;
let delay_time = 1000.0 / fps;
let delay_timei = uint32 delay_time

type TreatzState =
    { placeholder : int }

type RenderingContext =
    {Renderer:SDLRender.Renderer;
     Texture:SDLTexture.Texture;
     Surface:SDLSurface.Surface;
     WorkSurface:SDLSurface.Surface}

let rec eventPump (renderHandler:'TState->unit) (eventHandler:SDLEvent.Event->'TState->'TState option) (state:'TState) : unit =
    match SDLEvent.pollEvent() with
    | Some event ->
        match state |> eventHandler event with
        | Some newState -> eventPump renderHandler eventHandler newState
        | None -> ()
    | None -> 
        state
        |> renderHandler
        eventPump renderHandler eventHandler state

let handleEvent (event:SDLEvent.Event) (state:TreatzState) : TreatzState option =
    match event with
    | SDLEvent.Quit quitDetails -> 
        None
    | SDLEvent.KeyDown keyDetails -> 
        // handle keypress
        Some state
        //state |> handleKeyDownEvent sumLocationsFunc setVisibleFunc createFunc random keyDetails
    | _ -> 
        // core logic function here
        Some state

let render(context:RenderingContext) (state:TreatzState) =
    // clear screen
    context.Renderer |> SDLRender.setDrawColor (255uy,0uy,255uy,255uy) |> ignore
    context.Renderer |> SDLRender.clear |> ignore

    context.Surface
    |> SDLSurface.fillRect None {Red=255uy;Green=0uy;Blue=255uy;Alpha=255uy}
    |> ignore

    context.Texture
    |> SDLTexture.update None context.Surface
    |> ignore

    context.Renderer |> SDLRender.copy context.Texture None None |> ignore
    context.Renderer |> SDLRender.present 

let main() = 
    use system = new SDL.System(SDL.Init.Video ||| SDL.Init.Events)

    use mainWindow = SDLWindow.create "test" 100<px> 100<px> 640<px> 480<px> 0u

    use mainRenderer = SDLRender.create mainWindow -1 SDLRender.Flags.Accelerated

    use surface = SDLSurface.createRGB (320<px>,240<px>,32<bit/px>) (0x00FF0000u,0x0000FF00u,0x000000FFu,0x00000000u)

    use workSurface = SDLSurface.createRGB (8<px>,8<px>,32<bit/px>) (0x00FF0000u,0x0000FF00u,0x000000FFu,0x00000000u)

    workSurface
    |> SDLSurface.setColorKey (Some {Red=0uy;Green=0uy;Blue=0uy;Alpha=0uy})
    |> ignore

    use mainTexture = mainRenderer |> SDLTexture.create SDLPixel.RGB888Format SDLTexture.Access.Streaming (320<px>,240<px>)

    mainRenderer |> SDLRender.setLogicalSize (320<px>,240<px>) |> ignore

    let context = 
        { Renderer = mainRenderer; Texture = mainTexture; Surface = surface; WorkSurface = workSurface }

    let state = {placeholder = 0}

    eventPump (render context) (handleEvent) state

        
main()