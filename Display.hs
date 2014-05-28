module Display (
   display
  ,idle
  ,reshape
  ,drawGrid
  ,drawX
  ,drawO
  ,Polygon
  ,Point
  ,CellPos
  ,points2GL
  ,point2GL
  ,glPoints2Vertexes
  ,points2Vertexes
) where


import Control.Monad (forM_)
import Graphics.UI.GLUT
import Data.IORef
import Settings
import qualified TTT.Position as Pos

--let color3f r g b = color $ Color3 r g (b :: GLfloat)
--color3f 1 1 1 -- white  

--display :: IORef [CellPos] -> DisplayCallback -- another name for IO ()
display p = do 
    clear [ColorBuffer]
    loadIdentity -- for use of preservingMatrix
    pos@(Pos.Position board ch) <- get p

    -- coords for 0,1,2,3.. positions on tictactoe grid
    let coords = [ (-(2/3),2/3)    , (0,2/3)    , (2/3,2/3),
               (-(2/3),0)      , (0,0)      , (2/3,0),
               (-(2/3),-(2/3)) , (0,-(2/3)) , (2/3,-(2/3)) ]

    -- from the current board, draw the appropriate letter
    forM_ [0..8] $ \t ->
        preservingMatrix $ do
            case (board !! t) of 
                'X' -> do 
                    drawX (coords !! t)
                'O' -> do 
                    drawO (coords !! t)
                -- here I'm unsure how to just do nothing. I want nothing drawn so I draw a point way off the board
                _   -> renderPrimitive Points $ (vertex $ (Vertex3 100 100 0 :: Vertex3 GLfloat))
    drawGrid
    swapBuffers

idle :: IdleCallback
idle = do
    postRedisplay Nothing

reshape :: ReshapeCallback
reshape size@(Size width height) = do
    viewport $= (Position 0 0, size)
    postRedisplay Nothing

drawGrid :: IO ()
drawGrid = renderPrimitive Lines $ do
    let color3f r g b = color $ Color3 r g (b :: GLfloat)
    color3f 1 1 1 -- white  
    let boardLines = [[ ((-1/3),-1,0), ((-1/3),1,0) ],
                      [ ((1/3),-1,0),  (1/3,1,0)     ],
                      [ (-1,(1/3),0),  (1,1/3,0)     ],
                      [ (-1,(-1/3),0), (1,(-1/3),0) ]
                     ]
    forM_ boardLines $ \[(a,b,c),(x,y,z)] ->
        mapM_ vertex [
            (Vertex3 a b c :: Vertex3 GLfloat),(Vertex3 x y z :: Vertex3 GLfloat) 
        ]


drawX :: Point -> IO ()
drawX (a,b) = renderPrimitive Lines $ do
    let color3f r g b = color $ Color3 r g (b :: GLfloat)
    color3f 1 1 1 -- white
    let (x,y) = point2GL (a,b)
    let t = realToFrac xLength :: GLfloat
    vertex $ (Vertex3 (x-t) (y-t) 0 :: Vertex3 GLfloat)
    vertex $ (Vertex3 (x+t) (y+t) 0 :: Vertex3 GLfloat)
                                                     
    vertex $ (Vertex3 (x-t) (y+t) 0 :: Vertex3 GLfloat)
    vertex $ (Vertex3 (x+t) (y-t) 0 :: Vertex3 GLfloat)

drawO :: Point -> IO ()
drawO pt = do
    let color3f r g b = color $ Color3 r g (b :: GLfloat)
    let ball = circle pt oRadius
    let ball' = circle pt (oRadius-0.005)

    color3f 1 1 1 -- white
    renderPrimitive Polygon (points2Vertexes ball)
    color3f 0 0 0 -- black
    renderPrimitive Polygon (points2Vertexes ball')


-- some utility functions for drawing

type Polygon = [Point]
type Point = (Float, Float)
type CellPos = (Integer,Integer)

circle :: Point -> Float -> Polygon
circle (x,y) r = map (\t -> (x+r*cos (t), y+r*sin (t))) [0,0.2..(2*pi)]

points2GL :: [Point] -> [(GLfloat,GLfloat)]
points2GL l = [ (realToFrac x, realToFrac y) | (x,y) <- l ]

point2GL :: Point -> (GLfloat, GLfloat)
point2GL (x,y) = (realToFrac x :: GLfloat, realToFrac y :: GLfloat)

glPoints2Vertexes pts = mapM_ (\(x, y) -> vertex $ Vertex2 x y) pts

points2Vertexes pts = glPoints2Vertexes (points2GL pts)

