open System

let mapWidth = 64
let mapHeight = 48

type Surface =
    | Mountain
    | Grass

type MountainMap = Map<int * int, Surface >

let printMountains (mountain:MountainMap) =
    for x = 0 to mapWidth do
        for y = 0 to mapHeight do
            match mountain.[x,y] with
            | Mountain -> printf "#"
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

let newValue grid cell =
    let nei = findNeighbours grid cell
    let mountainCount = nei |> List.filter(fun x -> x = Mountain) 
    match grid.[cell] with
    | Mountain -> 
          if List.length mountainCount >= 4 then Mountain
          else Grass          
    | Grass ->          
          if List.length mountainCount >= 5 then Mountain
          else Grass

let iteration grid = Map.map(fun k _ -> newValue grid k) grid

let generateRandomMountain() = 
    let indexes = 
        [for x = 0 to mapWidth do
            for y = 0 to mapHeight do 
                yield x,y]
    (Map.empty,indexes) 
    ||> List.fold(fun map cell  -> 
        let x = if chaos.Next(0,2) = 0 then Mountain else Grass
        Map.add cell x map)


generateRandomMountain()
|> printMountains
|> iteration
|> printMountains
|> iteration
|> printMountains
                
//http://www.roguebasin.com/index.php?title=Cellular_Automata_Method_for_Generating_Random_Cave-Like_Levels