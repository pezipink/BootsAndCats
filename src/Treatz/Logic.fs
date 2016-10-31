module Logic
open SDLUtility
open System
open LSystem
/////
let noMoreParachuting = 450.0
let miaow =  
    let p  = new System.Media.SoundPlayer(@"..\..\..\..\images\cat_meow_x.wav")
    fun () -> p.Play()
/////
let chaos = System.Random(DateTime.Now.Millisecond)

let screenWidth = 800.0;

let screenHeight = 600.0;

let groundLevel = 600.0;
// default ground level where the boots are

let busSpeed = 2.5
let cloudSpeed = 0.8

let wind() =
    let n = chaos.NextDouble() * 3.0
    if chaos.Next(2) = 1 then -n else n
    

// default falling speeds
let freeFallSpeed = 5.0
let parachuteFallSpeed = 2.5

// default amount you can move left / right
// before wind is introduced
let veerFreefall = 5.0
let veerParachute = 8.0


// assume wind is between -5.0 to 5.0

type Button =
    | Left
    | Right
    | Fire

type Size = 
    {
        width : float
        height : float 
    }

// todo: sizes
let catbusSize = {width = 100.0; height = 50.0;}
let bootSize = {width = 80.0; height = 100.0;}
let catSize =  {width = 50.0; height = 100.0;}
let cloudSize = {width = 100.0; height = 50.0;}



type GameState =
    | Playing
    | GameOver

type PlayerState =
    | InBus
    | FreeFalling 
    | Parachute 
    | InBoot
    | Splatted

type Player =
    {
        mutable state : PlayerState
        catbus : Position * Size
        boot : Position * Size
        pos : Position
        size: Size
        mutable freeFallTime : int
        mutable parachuteTime : int
        mutable buttonsPressed : Set<Button>
        mutable justJumped : bool
        mutable score : int
    }
     with static member Create() =
            { state = PlayerState.InBus
              catbus = Position.Zero(), catbusSize
              boot = Position.Zero(), bootSize
              pos = Position.Zero()
              size = catSize
              freeFallTime = 0
              parachuteTime = 0
              buttonsPressed = Set.empty  
              justJumped = false
              score = 0
              }
           member this.busrect: SDLGeometry.Rectangle =
            let l,s = this.catbus 
            { X = (int l.x) * 1<px>; Y = (int l.y) * 1<px>; Width = (int s.width) * 1<px>; Height = (int s.height) * 1<px>}
           member this.bootrect: SDLGeometry.Rectangle =
            let l,s = this.boot 
            { X = (int l.x) * 1<px>; Y = (int l.y) * 1<px>; Width = (int s.width) * 1<px>; Height = (int s.height) * 1<px>}
           member this.prect: SDLGeometry.Rectangle =
            let l,s = this.pos, this.size
            { X = (int l.x) * 1<px>; Y = (int l.y) * 1<px>; Width = (int s.width) * 1<px>; Height = (int s.height) * 1<px>}
           
           
type Game =
    {
        Player1 : Player
        Player2 : Player
        mutable Clouds : (Position * Size) list
        mutable Trees: LineSegment list
        mutable WindFactor : float 
        mutable State : GameState
    }
let createClouds() =
  [for _ in 1..(chaos.Next(6, 10)) do
    yield ({ Position.Zero() with x = (float <| chaos.Next(0, 200)) ; y = (float <| chaos.Next(20, 200))}, cloudSize)]
let createTrees() = []
//    ferns
//      |> processLsystem 5
//      |> processTurtle turtle


let StartGame() =
    let state = 
        {
            Player1 = Player.Create()
            Player2 = Player.Create()
            Clouds = createClouds()
            Trees = createTrees()
            WindFactor = wind()
            State = Playing
        }

    (fst state.Player2.catbus).x <- screenWidth - ((snd state.Player2.catbus).width)
    (fst state.Player1.catbus).vx <- busSpeed
    (fst state.Player2.catbus).vx <- -busSpeed
    
    (fst state.Player1.boot).x <- screenWidth / 4.0
    (fst state.Player2.boot).x <- (screenWidth / 4.0) * 3.0
    (fst state.Player1.boot).y <- screenHeight - bootSize.height
    (fst state.Player2.boot).y <- screenHeight - bootSize.height

    let cloudy cloud =
        (fst cloud).vx <- state.WindFactor * cloudSpeed 
    let shakeTrees tree =
      ()
    state.Clouds |>  List.iter(cloudy)
    state.Trees |> List.iter (shakeTrees)
    state
let update (state:Game) =
    // always move the cas buses no matter what
    (fst state.Player1.catbus).Update()
    (fst state.Player2.catbus).Update()
    // always move the clouds with the wind
    state.Clouds |> List.iter(fun x -> (fst x).Update() )
    let updatePlayer p =
        let isLeft = p.buttonsPressed.Contains Button.Left
        let isRight = p.buttonsPressed.Contains Button.Right
        let isFire = p.buttonsPressed.Contains Button.Fire
        if not isFire then p.justJumped <- false
        match p.state with
        | InBus -> 
            // jump from bus
            if isFire then
                p.state <- FreeFalling
                miaow()
                // tood: need to centre this
                p.pos.x <- (fst p.catbus).x 
                p.pos.y <- (fst p.catbus).y
                p.pos.vx <- state.WindFactor 
                p.pos.vy <- freeFallSpeed
                p.justJumped <- true
            let c = fst p.catbus
            if c.x < 0.0 || c.x > screenWidth then
                p.state <- Splatted
        | FreeFalling -> 
            // open para
            // todo: only allow this to happen when higher than a certain height
            p.freeFallTime <- p.freeFallTime + 1
            if isFire && not p.justJumped && p.pos.y <= 350.0 then
                p.state <- Parachute
                p.pos.vx <- 0.0
                p.pos.vy <- parachuteFallSpeed
                
            if isLeft then
                // apply -x force + wind
                p.pos.vx <- -veerFreefall + state.WindFactor
                if p.pos.vx > 0. then p.pos.vx <- 0.0
            elif isRight then
                // apply x force + wind
                p.pos.vx <- veerFreefall + state.WindFactor
                if p.pos.vx < 0. then p.pos.vx <- 0.0
            else 
                p.pos.vx <- state.WindFactor 
            p.pos.Update()            
        | Parachute -> 
            p.parachuteTime <- p.parachuteTime + 1
            // weee
            if isLeft then
                // apply -x force + wind
                p.pos.vx <- -veerParachute + state.WindFactor
                if p.pos.vx > 0. then p.pos.vx <- 0.0
            elif isRight then
                // apply x force + wind
                p.pos.vx <- veerParachute + state.WindFactor
                if p.pos.vx < 0. then p.pos.vx <- 0.0
            else 
                p.pos.vx <- state.WindFactor 
            p.pos.Update()
        | _ -> ()

    updatePlayer state.Player1
    updatePlayer state.Player2
    
    // collision detection
    let cdet player = 
        let overlapBoot = 
            let loc,sz = player.boot
            player.pos.x >= loc.x && player.pos.x <= loc.x + sz.width
               
        match player.state with
        | FreeFalling when player.pos.y > groundLevel -> 
            // splat!
        
            player.state <- Splatted
        
        | Parachute when player.pos.y > groundLevel - bootSize.height && overlapBoot ->
            // landed in boot! (basic, this needs to take into account the boot height
            miaow()
            player.state <- InBoot
        | Parachute when player.pos.y > groundLevel ->
            player.state <- Splatted
        
        | _ -> ()

    cdet state.Player1
    cdet state.Player2

    match state.Player1.state, state.Player2.state with
    | (InBoot | Splatted), (InBoot | Splatted) -> 
        state.State <- GameOver
        let n = StartGame() 
        let p1s =  ((state.Player1.parachuteTime / 2) + state.Player1.freeFallTime )         
        let p2s =  ((state.Player2.parachuteTime / 2) + state.Player2.freeFallTime ) 
        match state.Player1.state with 
        | InBoot ->
            n.Player1.score <- state.Player1.score + p1s
        | _ -> n.Player1.score <- state.Player1.score
        
        match state.Player2.state with 
        | InBoot -> n.Player2.score <- n.Player2.score + p2s
        
        | _ -> n.Player2.score <- state.Player2.score
        
        n
    | _ -> state
    
