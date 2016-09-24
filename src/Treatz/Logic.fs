﻿module Logic

open System

let chaos = System.Random(DateTime.Now.Millisecond)

let screenWidth = 640.0;

let groundLevel = 480.0;
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

// todo: sizes
let catbusSize = {width = 100.0; height = 50.0;}
let bootSize = {width = 50.0; height = 100.0;}
let catSize =  {width = 50.0; height = 100.0;}

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
    }
     with static member Create() =
            { state = PlayerState.InBus
              catbus = Position.Zero(), catbusSize
              boot = Position.Zero(), bootSize
              pos = Position.Zero()
              size = catSize
              freeFallTime = 0
              parachuteTime = 0
              buttonsPressed = Set.empty  }
type Game =
    {
        Player1 : Player
        Player2 : Player
        mutable WindFactor : float 
        mutable State : GameState
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
                p.pos.x <- (fst p.catbus).x 
                p.pos.y <- (fst p.catbus).y
                p.pos.vx <- 0.0
                p.pos.vy <- freeFallSpeed
        | FreeFalling -> 
            // open para
            // todo: only allow this to happen when higher than a certain height
            p.freeFallTime <- p.freeFallTime + 1
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
            p.parachuteTime <- p.parachuteTime + 1
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
        let overlapBoot = 
            let loc,sz = player.boot
            player.pos.x >= loc.x && player.pos.x <= loc.x + sz.width
               
        match player.state with
        | FreeFalling when player.pos.y > groundLevel -> 
            // splat!
            player.state <- Splatted
        | Parachute when player.pos.y > groundLevel && overlapBoot ->
            // landed in boot! (basic, this needs to take into account the boot height
            player.state <- InBoot
        | _ -> ()

    cdet state.Player1
    cdet state.Player2

    match state.Player1.state, state.Player2.state with
    | (InBoot | Splatted), (InBoot | Splatted) -> 
        state.State <- GameOver
    | _ -> ()
    state


let StartGame() =
    let wind =
        let n = chaos.Next(6) |> double
        if chaos.Next(2) = 1 then -n else n
    {
        Player1 = Player.Create()
        Player2 = Player.Create()
        WindFactor = wind
        State = Playing
    }