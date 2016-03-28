namespace TreatzGame

open CommonData
open SDLUtility
open SDLGeometry
open SDLKeyboard
open SDLGameController


[<Struct>]
[<CustomEquality>]
[<CustomComparison>]
// used in the quadtrees and pathfinding algorithms 
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
    
type Point = {
   X: double 
   Y: double  
  } with 
  member this.lenght = sqrt(this.X * this.X + this.Y * this.Y)
     
  static member (-) (pointa, pointb) = 
    {X = pointa.X - pointb.X ; Y= pointa.Y - pointb.Y}

  member this.normalize  =        
    let len= this.lenght 
    match len with
    | x when x <> 0.0 -> { X = this.X/len; Point.Y = this.Y/len }
    | _ -> {X=0.0; Y=0.0}
  
  static member (*) (a, point: Point) =
     {X = a * point.X ; Y=  a * point.Y}
  static member (+) (pointa , pointb) = 
    {X = pointa.X + pointb.X ; Y= pointa.Y + pointb.Y}
  static member Zero = {X = 0.0; Y= 0.0}

type BehaviourState  = {
  CircleRadius : double
  CircleDistance : double
  RateOfChangeOfDirection : double

  WanderingAngle: double
  SteeringDirection : Point
  }

type PlayerData = 
    {DragonsCaught : int}
    with static member Blank = {DragonsCaught = 0}

type DragonData =
    | Nothing
    | Roam of roamingFrames : int 
    | Seek of Point list
    | Wander of BehaviourState
    | Temporary of treat : Point // no really, this is going


type MikishidaKinds =
    | Player of PlayerData
    | Dragon of DragonData
    | Treat
    | Mountainountain
    | AntiDragonFoam 
    | Squirrel
    | Cat
    | Otter
    with 
    member this.defaultSpeed =
        match this with
        | Player _ -> 1.4
        | Dragon _  -> 1.5
        | _ -> 0.9

type Mikishida = 
    { kind : MikishidaKinds; location : Point; velocity : Point }
    with 
    member this.Size =
        match this.kind with
        // presently everything must be at most the size of a cell
        | _ -> cellWidth * 1<px>,cellHeight * 1<px>
//        | Treat -> 5<px>, 5<px>
//        | _ -> 5<px>, 5<px>

    member this.AsRect = 
        let w, h = this.Size
        { 
          X = (this.location.X |> int)*1<px> 
          Y = (this.location.Y |> int)*1<px>
          Width = w
          Height = h 
        }
    member this.AsQuadBounds : QuadTree.QuadBounds = 
        let w, h = this.Size
        { 
          x = (this.location.X ) |> int
          y = (this.location.Y ) |> int
          width = int w 
          height = int h
        }
    member this.Distance(other:Mikishida) =
        let xd = other.location.X - this.location.X
        let yd = other.location.Y - this.location.Y
        sqrt(xd*xd+yd*yd)

    member this.ManhattanDistance(other:Mikishida) =
        abs(other.location.X - this.location.X) + abs(other.location.Y - this.location.Y)


type TreatzState =
    { Player1 : Mikishida
      Player2 : Mikishida
      Mikishidas : Mikishida list
      UnpassableLookup : Set<int*int> 
      TreatsLookup : Set<int*int> 
      PressedKeys : Set<ScanCode> 
      Controllers : Set<ControllerButton> * Set<ControllerButton>
      Sprites : Map<string, SDLTexture.Texture>
      TurkeyAngle : float
      Chaos : System.Random
      }
    with member this.findMikishidas pred bounds =
            this.Mikishidas |> List.filter(fun m -> pred m && overlapq(m.AsQuadBounds, bounds))
