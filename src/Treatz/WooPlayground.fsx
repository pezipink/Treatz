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
    
let chaos = System.Random(System.DateTime.Now.Millisecond)

let generateRandomMountain() = 
    let indexes = 
        [for x = 0 to mapWidth do
            for y = 0 to mapHeight do 
                yield x,y]
    (Map.empty,indexes) 
    ||> List.fold(fun map cell  -> 
        let x = if chaos.Next(0,2) = 0 then Mountain else Grass
        Map.add cell x map)
            
                
