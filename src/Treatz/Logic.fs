module Logic
open SDLUtility
open System
/////
let noMoreParachuting = 450.0

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

type Position = 
    {
        mutable x : float
        mutable y : float
        mutable vx : float
        mutable vy : float
    } 
    with 
    static member Zero() =
        { x = 0.; y = 0.; vx=0.; vy = 0. }
    member this.Update() = 
            this.x <- this.x + this.vx
            this.y <- this.y + this.vy

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
        mutable Cloud : Position * Size
        mutable WindFactor : float 
        mutable State : GameState
    }

let StartGame() =
    let state = 
        {
            Player1 = Player.Create()
            Player2 = Player.Create()
            Cloud = { Position.Zero() with y = (float <| chaos.Next(20, 200))}, cloudSize
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
    
    (fst state.Cloud).vx <- state.WindFactor
    if state.WindFactor <= 0. then
        (fst state.Cloud).x <- screenWidth - ((snd state.Cloud).width)

    state
let update (state:Game) =
    // always move the cas buses no matter what
    (fst state.Player1.catbus).Update()
    (fst state.Player2.catbus).Update()
    // always move the clouds with the wind
    (fst state.Cloud).Update()
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
            player.state <- InBoot
        | _ -> ()

    cdet state.Player1
    cdet state.Player2

    match state.Player1.state, state.Player2.state with
    | (InBoot | Splatted), (InBoot | Splatted) -> 
        state.State <- GameOver
        StartGame() 
        
    | _ -> state
    
