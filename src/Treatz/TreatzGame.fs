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
    
    override __.GetHashCode() =
        x + 65536 * y

    member __.GridX = x / cellWidth
    member __.GridY = y / cellHeight
    
type Vector2 = {
   X: double 
   Y: double  
  } with 
  member this.length = sqrt(this.X * this.X + this.Y * this.Y)
     
  member this.GridX = int(this.X / cellWidthf)
  member this.GridY = int(this.Y / cellHeightf)

  static member (-) (pointa, pointb) = 
    {X = pointa.X - pointb.X ; Y= pointa.Y - pointb.Y}

  member this.normalize  =        
    let len= this.length 
    match len with
    | x when x <> 0.0 -> { X = this.X/len; Vector2.Y = this.Y/len }
    | _ -> {X=0.0; Y=0.0}
  
  static member (*) (a, point: Vector2) =
     {X = a * point.X ; Y=  a * point.Y}
  static member (+) (pointa , pointb) = 
    {X = pointa.X + pointb.X ; Y= pointa.Y + pointb.Y}
  static member Zero = {X = 0.0; Y= 0.0}

  static member Truncate max point= 
    let x =  if point.X > max.X then max.X else point.X
    let y =  if point.Y > max.Y then max.Y else point.Y
    {X=x; Y=y}
  override this.ToString() =
     this.X.ToString() + " " + this.Y.ToString()



type BehaviourState  = {
  CircleRadius : double
  CircleDistance : double
  RateOfChangeOfDirection : double

  WanderingAngle: double
  SteeringDirection : Vector2
  }

[<CustomComparison; CustomEquality>]
type Node = 
  {        
    Identity: Vector2
    mutable Cost : int 
    mutable Neighbours : Node seq       
  }
  override x.Equals(yobj) =
      match yobj with
      | :? Node as y -> (x.Identity= y.Identity)
      | _ -> false

  override x.GetHashCode() = hash x.Identity
  interface System.IComparable with
    member x.CompareTo yobj =
        match yobj with
        | :? Node as y -> compare x.Identity y.Identity
        | _ -> invalidArg "yobj" "canno"

type PlayerData = 
    {mutable DragonsCaught : int 
     mutable FoamDuration : int
     mutable Foam : Set<int*int> }
    with static member Blank = {DragonsCaught = 0; FoamDuration = 0; Foam = Set.empty}

type DragonData =
    | Nothing    
    | Seek of Vector2 list
    | Wander of BehaviourState
    | PathFind of Node
    | Temporary of treat : Vector2 // no really, this is going


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
        | Player _ -> 0.4
        | Dragon _  -> 0.9
        | _ -> 0.9

type Mikishida = 
    { kind : MikishidaKinds; location : Vector2; velocity : Vector2 }
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
      PathFindingData : Node seq option
      }
    with member this.findMikishidas pred bounds =
            this.Mikishidas |> List.filter(fun m -> pred m && overlapq(m.AsQuadBounds, bounds))
