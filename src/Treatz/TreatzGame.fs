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
    member __.CentreGridX = (x + cellWidth / 2) / cellWidth
    member __.CentreGridY = (y + cellHeight /2) / cellHeight
    
type Vector2 = {
   X: double 
   Y: double  
  } with 
  member this.length = sqrt(this.X * this.X + this.Y * this.Y)
     
  member this.GridX = int(this.X / cellWidthf)
  member this.GridY = int(this.Y / cellHeightf)
  member this.CentreGridX = int <| (this.X + cellWidthf / 2.0) / cellWidthf
  member this.CentreGridY = int <| (this.Y + cellHeightf /2.0) / cellHeightf
  member this.Grid = this.GridX, this.GridY

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

type NodeVector = 
  {X: int; Y : int}
  
  static member (+) (pointa , pointb) = 
    {X = pointa.X + pointb.X ; Y= pointa.Y + pointb.Y}
  static member (-) (pointa , pointb) = 
    {X = pointa.X - pointb.X ; Y= pointa.Y - pointb.Y}

[<CustomComparison; CustomEquality>]
type Node = 
  {        
    ///Grid coordinates
    Identity: NodeVector
    mutable Cost : int 
    mutable Neighbours : Node seq       
  }
  override this.Equals(yobj) =
      match yobj with
      | :? Node as y -> (this.Identity= y.Identity)
      | _ -> false

  override x.GetHashCode() = hash x.Identity
  interface System.IComparable with
    member x.CompareTo yobj =
        match yobj with
        | :? Node as y -> compare x.Identity y.Identity
        | _ -> invalidArg "yobj" "canno"

type NodePath = {   
   GridNode: Node
   Parent : NodePath option
   Child : NodePath option
   mutable PathCost: double
  }

type PlayerData = 
    {mutable DragonsCaught : int 
     mutable FoamDuration : int
     mutable Foam : Map<int*int,int> }
    with static member Blank = {DragonsCaught = 0; FoamDuration = 0; Foam = Map.empty}

type AlphaAngle =
    { mutable currentAngle : float
      mutable alpha : int
       }

type DragonData =
    | FollowPath of NodeVector array * Vector2
    | Wander of BehaviourState
    | PathFind of Vector2

type MikishidaKinds =
    | Player of PlayerData
    | Dragon of DragonData
    | Treat
    | Mountainountain
    | AntiDragonFoam of System.UInt32
    | Squirrel
    | Cat
    | Otter
    | CaughtDragon of AlphaAngle
    | TreatEaten of AlphaAngle
    with 
    member this.defaultSpeed =
        match this with
        | Player _ -> 8.0
        | Dragon _  -> 2.5
        | _ -> 0.9

type Mikishida = 
    { kind : MikishidaKinds; location : Vector2; velocity : Vector2 }
    with 
    member this.Size =
        match this.kind with
        // Everything must be at most the size of a cell
        | _ -> cellWidth * 1<px>,cellHeight * 1<px>
  
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

    member this.AsPlayerData =
        match this.kind with
        | Player data -> data
        | _ -> failwith "!"

type GameState =
    | TitleScreen
    | Playing

type TreatzState =
    { GameState : GameState
      Player1 : Mikishida
      Player2 : Mikishida
      Mikishidas : Mikishida list
      UnpassableLookup : Set<int*int> 
      TreatsLookup : Set<int*int> 
      PressedKeys : Set<ScanCode> 
      Controllers : Set<ControllerButton> * Set<ControllerButton>
      Sprites : Map<string, SDLTexture.Texture>
      TurkeyAngle : float
      Chaos : System.Random      
      PathFindingData : Map<int*int,Node>
      LastFrameTime: uint32      
      mutable DebugLines: SDLGeometry.Point array
      }
    with 
        member this.findMikishidas pred bounds =
            this.Mikishidas |> List.filter(fun m -> pred m && overlapq(m.AsQuadBounds, bounds))
        member this.IsCellOutofbounds (x,y) = 
            (x < 0.0 || x > mapWidthf * cellWidthf - cellWidthf ||   y < 0.0 || y > mapHeightf * cellHeightf - cellHeightf) 
        member this.IsCellUnpassable (x,y) = 
            let toCell (x,y) = (int(x/cellWidthf)),(int(y/cellHeightf))
            Set.contains (toCell (x, y)) this.UnpassableLookup
        
            