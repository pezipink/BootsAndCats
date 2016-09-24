module Logic

open System

let chaos = System.Random(DateTime.Now.Millisecond)

let groundLevel = 640.0
// default ground level where the boots are


// default falling speeds
let freeFallSpeed = 10.0
let parachuteFallSpeed = 5.0

// default amount you can move left / right
// before wind is introduced
let veerFreefall = 2.0
let veerParachute = 5.0

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

type Position = 
    {
        mutable x : float
        mutable y : float
        mutable vx : float
        mutable vy : float
    } 
    with 
    member this.Update() = 
            this.x <- this.x + this.vx
            this.y <- this.y + this.vy

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
    }
    
type Game =
    {
        Player1 : Player
        Player2 : Player
        mutable WindFactor : float 
    }


let update (state:Game) =
    // always move the cas buses no matter what
    (fst state.Player1.catbus).Update()
    (fst state.Player2.catbus).Update()

    let updatePlayer p =
        let isLeft = p.buttonsPressed.Contains Button.Left
        let isRight = p.buttonsPressed.Contains Button.Right
        let isFire = p.buttonsPressed.Contains Button.Fire
        match p.state with
        | InBus -> 
            // jump from bus
            if isFire then
                p.state <- FreeFalling
                // tood: need to centre this
                p.pos.x <- p.catbus.x 
                p.pos.y <- p.catbus.y
                p.pos.vx <- 0.0
                p.pos.vy <- freeFallSpeed
        | FreeFalling -> 
            // open para
            // todo: only allow this to happen when higher than a certain height
            if isFire then
                p.state <- Parachute
                p.pos.vx <- 0.0
                p.pos.vy <- parachuteFallSpeed
            if isLeft then
                // apply -x force + wind
                p.pos.vx <- -veerFreefall + state.WindFactor
            if isRight then
                // apply x force + wind
                p.pos.vx <- veerFreefall + state.WindFactor
            p.pos.Update()            
        | Parachute -> 
            // weee
            if isLeft then
                // apply -x force + wind
                p.pos.vx <- -veerParachute + state.WindFactor
            if isRight then
                // apply x force + wind
                p.pos.vx <- veerParachute + state.WindFactor
            p.pos.Update()
        | _ -> ()

    updatePlayer state.Player1
    updatePlayer state.Player2
    
    // collision detection
    let cdet player = 
        match player.state with
        | FreeFalling when player.pos.y > groundLevel -> 
            // splat!
            player.state <- Splatted
        | Parachute when player.pos y > groundLevel & player.pos.x >= player.boot.x 

    state
