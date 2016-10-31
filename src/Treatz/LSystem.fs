module LSystem
// woo ferns!

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


type Color = { r:byte; g:byte; b:byte; } 

type LineSegment = {startPoint : Position; endPoint : Position ; color : Color }    



let chaos = System.Random(System.DateTime.Now.Millisecond)

let red = { r = 255uy; g = 0uy; b = 0uy}
let blue = { r = 0uy; g = 0uy; b = 255uy}
let green = { r = 0uy; g = 255uy; b = 255uy}

let randomColor() = { r = uint8(chaos.Next 256);g = uint8(chaos.Next 256);b = uint8(chaos.Next 256) }

type LogoCommand =
    | DrawForward of float 
    | MoveForward of float
    | ChangeColor of Color
    | Turn of float
    | Push 
    | Pop
    
   
type LTurtle = 
    { angle : float
      x : float
      y : float 
      c : Color} 

let processTurtle turtle program =
    let mutable state = System.Collections.Generic.Stack<double * double * double>()

    let rec aux output turtle = function
        | [] -> output
        | ChangeColor c :: t -> aux output {turtle with c = c} t
        | DrawForward d :: t -> 
            let rads = turtle.angle * (System.Math.PI / 180.0)
            let x = turtle.x + d * cos rads
            let y = turtle.y + d * sin rads
            let newTurtle = {turtle with x = x; y= y }
            let seg = 
                {   startPoint = {x =  turtle.x; y =  turtle.y; vx=0.; vy=0.}
                    endPoint = {x =  x; y =  y; vx=0.; vy=0.}
                    color = newTurtle.c }
            aux (seg::output) newTurtle t
            
        | MoveForward d :: t -> 
            let rads = turtle.angle * (System.Math.PI / 180.0)
            let x = turtle.x + d * cos rads
            let y = turtle.y + d * sin rads
            let newTurtle = {turtle with x = x; y= y }
            aux output newTurtle t

        | Turn delta :: t -> 
            let d = turtle.angle + delta
            let d =
                // warp around logic
                if delta > 0.0 && d > 360.0 then d - 360.0
                elif delta < 0.0 && d < 0.0 then 360.0 + d
                else d
            aux output {turtle with angle = d} t
       
        | Push :: t -> 
            state.Push(turtle.x, turtle.y, turtle.angle)
            aux output turtle t
        | Pop :: t -> 
            let xx, yy, aangle = state.Pop()
            let newTurtle = {turtle with x= xx; y = yy; angle = aangle}
            aux output newTurtle t
            

    List.rev(aux [] turtle program)

    
type LSystem = {
    Axiom : string
    Productions : char -> string
    Actions : int  -> char -> (LogoCommand list) option
}

let processLsystem max lsystem =
    // first we perform the l-system generation
    // fast imperative generator
    let rec gen (current:string) iteration =
        if iteration = max then current
        else
            let sb = System.Text.StringBuilder()
            for x in current.ToCharArray() do
                sb.Append(lsystem.Productions x) |> ignore
            gen (sb.ToString()) (iteration+1)
              
    let finish = gen lsystem.Axiom 0
    System.Console.WriteLine ("Axiom: {0} -  Max :{1} ", lsystem.Axiom, max) |> ignore
    // now convert to turtle commands
    finish.ToCharArray() |> List.ofArray |> List.choose (lsystem.Actions max) |> List.collect id


// TODO 4: implement ferns
(*
F  = Move forward
X  = No op
- = Turn left 'degree'
+ = Turn right 'degree'
[ = remember current position and angle on stack... VERY IMPORTANT: remember the angle!! (A++ easy to forget) 
] = restore position and use it to draw from there.

HINT! You are probably going to need to add some new LogoCommands ...

*) 


let ferns = {
    Axiom = "X"
    Productions = 
        function
        | 'X' ->  "F−[[-X]+X]+F[+FX]−X"
        | 'F' -> "FF"
        | c -> string c
    Actions = 
        fun max c -> //i.e. this is where you do some of what is  specified in TODO 4 :D
            let lenght = 3.
            match c with
            | 'F' -> Some <| [DrawForward(lenght)]
            | '-' -> Some <| [Turn(15.)]
            | '+' -> Some <| [Turn(-15.)]
            | '[' -> Some <| [Push]
            | ']' -> Some <| [Pop]
            |  _  -> None
}


// a default turtle location
let turtle = { x = 0.0; y = 0.0; angle = 0.0; c = red }

// since some generalization has happened, creating the logo commands 
// explicty has been removed. here is an example on how to use the above 
// functions in SDL. (note that some LSystems take extra paramters, for example, for width)
//dragon 
//|> processLsystem 3 
//|> processTurtle turtle
//|> replace