module SDLGameController

#nowarn "9"

open System.Runtime.InteropServices
open System
open Microsoft.FSharp.NativeInterop


module private SDLControllerNative =
    //keyboard focus
    [<DllImport(@"SDL2.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr SDL_GameControllerOpen(int joystick_index)

    [<DllImport(@"SDL2.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern int SDL_NumJoysticks()

let gameControllerOpen index  : unit =
    let n = SDLControllerNative.SDL_NumJoysticks()
    let x = SDLControllerNative.SDL_GameControllerOpen(index) 
    ()

type ControllerAxis = 
   | AXIS_LEFTX = 0
   | AXIS_LEFTY = 1
   | AXIS_RIGHTX = 2
   | AXIS_RIGHTY = 3
   | AXIS_TRIGGERLEFT  = 4   
   | AXIS_TRIGGERRIGHT = 5

type ControllerButton =
   | BUTTON_A = 0
   | BUTTON_B = 1
   | BUTTON_X = 2
   | BUTTON_Y = 3
   | BUTTON_BACK = 4
   | BUTTON_GUIDE = 5
   | BUTTON_START = 6
   | BUTTON_LEFTSTICK = 7
   | BUTTON_RIGHTSTICK = 8
   | BUTTON_LEFTSHOULDER = 9
   | BUTTON_RIGHTSHOULDER = 10
   | BUTTON_DPAD_UP = 11
   | BUTTON_DPAD_DOWN = 12
   | BUTTON_DPAD_LEFT = 13
   | BUTTON_DPAD_RIGHT = 14
