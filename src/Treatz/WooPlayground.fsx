open System

let mapWidth = 64
let mapHeight = 48

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


type Level = Map<int*int,Surface>



type MountainMap = Map<int * int, Surface >

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

let generateRandomMountains() = 
    let indexes = 
        [for x = 0 to mapWidth do
            for y = 0 to mapHeight do 
                yield x,y]
    let map =
        (Map.empty,indexes)
        ||> List.fold(fun map cell  -> 
            let x = if chaos.NextDouble() <= 0.45 then (Mountain Centre) else Grass
            Map.add cell x map)
    
    let level =
        (map,[1..3]) 
        ||> List.fold(fun acc _ -> iteration acc)
        
    level
    |> Map.filter(fun _ v -> match v with Mountain _ -> true | _ -> false)
    |> Map.toList
    |> List.map fst
    |> Set.ofList

let printMountains (mountain:MountainMap) =
    for x = 0 to mapWidth do
        for y = 0 to mapHeight do
            match mountain.[x,y] with
            | Mountain _ -> printf "#"
            | Grass -> printf " "
        printfn "\n"
    mountain
    

let john grid (x,y) =     
    [(-1, -1);(0, -1); (1,-1);
     (-1,  0);         (1, 0);
     (-1,  1);(0,  1); (1, 1)]        
    |> List.map(fun (x',y') -> 
      let cell = x+x',y+y'
      match Map.tryFind cell grid with
      | Some v -> v
      | None -> Mountain Centre)


  
let level = 
  let mountains = generateRandomMountains()
  let map =
    [for x = 0 to mapWidth do
      for y = 0 to mapHeight do
        if Set.contains (x,y) mountains then yield ((x,y), (Mountain Centre))
        else yield ((x,y), Grass)]
    |> Map.ofList
  
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
        _ ;  true;  _; ] -> Mountain Top       

  | [ _  ; true;   _ ;
      true;      true;
      _   ; false;  _  ; ] -> Mountain Bottom

  | [ false ; false;  _  ;
      false;        true;
        _ ;  true;  true; ] -> Mountain TopLeft        
  
  | [ _   ; false;  false  ;
      true;         false;
      true ;  true;   _; ] -> Mountain TopRight

  | [ _   ; true;   true  ;
      false;         true;
      false ;  false;   _; ] -> Mountain BottomLeft     

  | [  true   ; true;   _  ;
      true;           false;
       _   ;  false;   _; ] -> Mountain BottomRight     

  | [  _   ; true;     _  ;
      true;           false;
       _   ;  true;    _; ] -> Mountain Right
        
  | [  _   ; true;     _  ;
      false;           true;
       _   ;  true;    _; ] -> Mountain Left
       
  | _ -> Mountain Centre

  map
  |> Map.map(fun k v -> 
    match v with
    | Grass -> v
    | Mountain _ -> identifyMountain (neighbours map k))
  |> Map.filter(fun k v -> 
    match v with 
    | Grass -> false
    | _ -> true)
  
  