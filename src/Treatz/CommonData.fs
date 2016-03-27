module CommonData

open SDLUtility

let screenWidth = 800<px>
let screenHeight = 600<px>


let mapWidth = 160
let mapHeight = 120
let mapWidthf = 160.0
let mapHeightf = 120.0

let cellWidth = 5
let cellHeight = 5
let cellWidthf = 5.0
let cellHeightf = 5.0

let screenQuadBounds =
    { x = 0; y = 0; width = int screenWidth; height = int screenHeight }  : QuadTree.QuadBounds


