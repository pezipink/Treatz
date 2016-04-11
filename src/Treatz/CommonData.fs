module CommonData

open SDLUtility
open SDLGeometry
open QuadTree

let screenWidth = 1024<px>
let screenHeight = 768<px>

let mapWidth = 64
let mapHeight = 48
let mapWidthf = 64.0
let mapHeightf = 48.0

let cellWidth = 16
let cellHeight = 16
let cellWidthf = 16.0
let cellHeightf = 16.0

let maxTreats = 1
let maxPlayerFoam = 20
let foamFrames = 60 * 3  // 5 seconds
let maxDragons = 2

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
    (chaos.Next(mapWidth)),(chaos.Next(mapHeight))
