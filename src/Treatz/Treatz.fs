open System

open SDL2


let fps = 60.0;
let delay_time = 1000.0 / fps;
let delay_timei = uint32 delay_time

let main() = 
    SDL.SDL_Init(SDL.SDL_INIT_EVERYTHING) |> ignore
    let window = SDL.SDL_CreateWindow("Treatz",0,0,800,600,SDL.SDL_WindowFlags.SDL_WINDOW_BORDERLESS ||| SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN)
    let renderer = SDL.SDL_CreateRenderer(window,-1,0ul)
//    let surface = SDL.SDL_CreateRGBSurface(
//                    0ul, 800,600,32, 
//                    0x00FF0000ul,
//                    0x0000FF00ul,
//                    0x000000FFul,
//                    0ul)
    //let texture = SDL.SDL_CreateTextureFromSurface(renderer,surface)
    let running = ref true

    let mutable srcRect = SDL.SDL_Rect()
    let event = ref (SDL.SDL_Event())

    while !running do
        let frameStart = SDL.SDL_GetTicks()

        // handle sdl events
        while SDL.SDL_PollEvent(event) > 0 do
            match (!event).``type`` with
            | SDL.SDL_EventType.SDL_KEYDOWN ->
                match (!event).key.keysym.scancode with
                | SDL.SDL_Scancode.SDL_SCANCODE_ESCAPE -> running := false
                | _ -> ()
            | SDL.SDL_EventType.SDL_QUIT -> 
                running := false
            | _ -> ()

        // update
        ()

        // render
        srcRect.h <- 100
        srcRect.w <- 100
        srcRect.x <- 400
        srcRect.y <- 300
        SDL.SDL_SetRenderDrawColor(renderer, 255uy, 255uy, 255uy, 0uy) |> ignore
        SDL.SDL_RenderDrawRect(renderer,ref srcRect) |> ignore

        // present
        SDL.SDL_RenderPresent(renderer)

        // lock to 60fps
        let frameTime = SDL.SDL_GetTicks() - frameStart
        if frameTime < delay_timei then
            SDL.SDL_Delay(delay_timei-frameTime)
        
main()