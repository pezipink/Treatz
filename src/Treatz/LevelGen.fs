module LevelGen
open System

type MountainType =
  | Top
  | TopRight
  | Right
  | BottomRight
  | Bottom
  | BottomLeft
  | Left
  | TopLeft
  | Centre

type Surface =
    | Mountain of MountainType
    | Grass

type MountainMap = Map<int * int, Surface >

let printMountains (mountain:MountainMap) =
    for x = 0 to CommonData.mapWidth do
        for y = 0 to CommonData.mapHeight do
            match mountain.[x,y] with
            | Mountain _ -> printf "#"
            | Grass -> printf " "
        printfn "\n"
    mountain
    
let chaos = System.Random(System.DateTime.Now.Millisecond)

let findNeighbours grid (x,y) =     
    [(-1, -1);(0, -1); (1,-1);
     (-1,  0);         (1, 0);
     (-1,  1);(0,  1); (1, 1)]        
    |> List.choose(fun (x',y') -> 
      let cell = x+x',y+y'
      Map.tryFind cell grid)

let findNeighboursKeys grid (x,y) =     
    [(-1, -1);(0, -1); (1,-1);
     (-1,  0);         (1, 0);
     (-1,  1);(0,  1); (1, 1)]        
    |> List.choose(fun (x',y') -> 
      let cell = x+x',y+y'
      Map.tryFind cell grid
      |> Option.map(fun _ -> cell))

let newValue grid cell =
    let count = 
      findNeighbours grid cell 
      |> List.filter( function Mountain _  -> true | _ -> false) 
      |> List.length
      
    match grid.[cell] with
    | Mountain _ when count >= 4 -> Mountain Centre
    | Grass when count >= 5 -> Mountain Centre     
    | _ -> Grass


let iteration grid = Map.map(fun k _ -> newValue grid k) grid

let findConnectedAreas (grid:MountainMap) =
    // flood fill from a Grass spot to find all connected areas. 
    // if we end up with 45% + of the map being connected then we can
    // turn all the other grass spots into mountains
    let rec findSpot() = 
        let x, y = chaos.Next(0,CommonData.mapWidth), chaos.Next(0,CommonData.mapHeight)
        if grid.[x,y] = Grass then x,y
        else findSpot()
    let start = findSpot()
        
    let set = System.Collections.Generic.HashSet<int*int>()
    let rec floodFill next =        
        set.Add next |> ignore
        findNeighboursKeys grid next        
        |> List.iter(fun v -> if set.Contains v then () else floodFill v)
        
    floodFill start
    let all = grid |> Map.toList |> List.map fst |> Set.ofList  |> System.Collections.Generic.HashSet<_>

    all.ExceptWith(set)
    set, all


let generateRandomMountains() = 
    let indexes = 
        [for x = 0 to CommonData.mapWidth do
            for y = 0 to CommonData.mapHeight do 
                yield x,y]
    let map =
        (Map.empty,indexes)
        ||> List.fold(fun map cell  -> 
            let x = if chaos.NextDouble() <= 0.45 then Mountain Centre else Grass
            Map.add cell x map)
    
    let level =
        (map,[1..3]) 
        ||> List.fold(fun acc _ -> iteration acc)
        
    let neighbours grid (x,y) =     
      [(-1, -1);(0, -1); (1,-1);
       (-1,  0);         (1, 0);
       (-1,  1);(0,  1); (1, 1)]        
      |> List.map(fun (x',y') -> 
        let cell = x+x',y+y'
        match Map.tryFind cell grid with
        | Some Grass -> false
        | _  -> true )

    let identifyMountain = function
    | [ _   ; false;  _  ;
        true;        true;
          _ ;  true;  _; ] -> Top       
  
    | [ _  ; true;   _ ;
        true;      true;
        _   ; false;  _  ; ] -> Bottom
  
    | [ false ; false;  _  ;
        false;        true;
          _ ;  true;  true; ] -> TopLeft        
    
    | [ _   ; false;  false  ;
        true;         false;
        true ;  true;   _; ] -> TopRight
  
    | [ _   ; true;   true  ;
        false;         true;
        false ;  false;   _; ] -> BottomLeft     
  
    | [  true   ; true;   _  ;
        true;           false;
         _   ;  false;   _; ] -> BottomRight     
  
    | [  _   ; true;     _  ;
        true;           false;
         _   ;  true;    _; ] -> Right
          
    | [  _   ; true;     _  ;
        false;           true;
         _   ;  true;    _; ] -> Left
         
    | _ -> Centre

    //let x = findConnectedAreas level
    (Map.empty,level)
    ||> Map.fold(fun newLevel cell v -> 
        match v with
        | Grass -> newLevel
        | Mountain _ -> Map.add cell (identifyMountain (neighbours level cell)) newLevel)


//http://www.roguebasin.com/index.php?title=Cellular_Automata_Method_for_Generating_Random_Cave-Like_Levels