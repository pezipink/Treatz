module CommonData

open SDLUtility
open SDLGeometry
open QuadTree

let screenWidth = 1024<px>
let screenHeight = 768<px>

[<Measure>] type cell
[<Measure>] type frame

let mapWidth = 64<cell>
let mapHeight = 48<cell>
let mapWidthf = (mapWidth |> float) * 1.0<cell>
let mapHeightf = (mapHeight |> float) * 1.0<cell>

let cellWidth = 16<px/cell>
let cellHeight = 16<px/cell>
let cellWidthf = (cellWidth |> float) * 1.0<px/cell>
let cellHeightf = (cellHeight |> float) * 1.0<px/cell>

let maxTreats = 400
let maxPlayerFoam = 15
let foamFrames = 60 * 3<frame>  // 5 seconds
let maxDragons = 40

let screenQuadBounds =
    { x = 0; y = 0; width = int screenWidth; height = int screenHeight }  : QuadTree.QuadBounds

let overlap(rectA, rectB) =
    let x1 = rectA.X
    let x2 = rectA.X + rectA.Width
    let y1 = rectA.Y
    let y2 = rectA.Y + rectA.Height

    let x1' = rectB.X
    let x2' = rectB.X + rectB.Width
    let y1' = rectB.Y
    let y2' = rectB.Y + rectB.Height

    x2' >= x1 && x1' <= x2 && y2' >= y1 && y1' <= y2

let overlapq(rectA, rectB) =
    let x1 = rectA.x
    let x2 = rectA.x + rectA.width
    let y1 = rectA.y
    let y2 = rectA.y + rectA.height

    let x1' = rectB.x
    let x2' = rectB.x + rectB.width
    let y1' = rectB.y
    let y2' = rectB.y + rectB.height

    x2' >= x1 && x1' <= x2 && y2' >= y1 && y1' <= y2

let randomGridLocation (chaos:System.Random) =
    (chaos.Next(mapWidth/1<cell>)) * 1<cell>,(chaos.Next(mapHeight/1<cell>)) * 1<cell>
