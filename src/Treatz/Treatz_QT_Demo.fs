module Treatz_QT
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

type buonds = { x : int; y : int; width : int; height : int }

let screenBounds = { x = 0; y = 0; width = int screenWidth; height = int screenHeight }

type TreatzState =
    { PressedKeys : Set<ScanCode> 
      rng : System.Random }

let treeDepth = 15
type RenderingContext =
    {Renderer:SDLRender.Renderer;
     Texture:SDLTexture.Texture;
     Surface:SDLSurface.Surface;
     mutable lastFrameTick : uint32 }

let update (state:TreatzState) : TreatzState =
    let pressed (code:ScanCode) = if state.PressedKeys.Contains code then true else false
    let update (scancode, f) state = if pressed scancode then f state else state
    let createRect() = 
        { x = state.rng.Next(0,(int screenWidth)-25) 
          y = state.rng.Next(0,(int screenHeight)-25)                  
          width = state.rng.Next(0,25)
          height = state.rng.Next(0,25) }
    state

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
    
    
    context.Renderer |> SDLRender.present 

    // delay to lock at 60fps (we could do extra work here)
    let frameTime = getTicks() - context.lastFrameTick
    if frameTime < delay_timei then delay(delay_timei - frameTime)
    context.lastFrameTick <- getTicks()    

let main() = 
    use system = new SDL.System(SDL.Init.Video ||| SDL.Init.Events)
    use mainWindow = SDLWindow.create "test" 100<px> 100<px> screenWidth screenHeight (uint32 SDLWindow.Flags.FullScreen) // FULLSCREEN!
    use mainRenderer = SDLRender.create mainWindow -1 SDLRender.Flags.Accelerated
    use surface = SDLSurface.createRGB (screenWidth,screenHeight,32<bit/px>) (0x00FF0000u,0x0000FF00u,0x000000FFu,0x00000000u)    
    use mainTexture = mainRenderer |> SDLTexture.create SDLPixel.RGB888Format SDLTexture.Access.Streaming (screenWidth,screenHeight)
    mainRenderer |> SDLRender.setLogicalSize (screenWidth,screenHeight) |> ignore

    let context =  { Renderer = mainRenderer; Texture = mainTexture; Surface = surface; lastFrameTick = getTicks() }
    let state = 
        {rng = System.Random()
         PressedKeys = Set.empty }

    eventPump (render context) handleEvent update state

main()