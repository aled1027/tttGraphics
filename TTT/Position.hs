module TTT.Position (
  Position (..)
 ,dim
 ,size
 ,initPosition
 ,move
 ,choose
 ,possibleMoves
 ,isWinFor
 ,pixelCoordToCell
 ,isMoveValid
 ,opposite
 ,render
)  where

import Data.List
import Data.List.Split
import Data.Char (intToDigit)

-- String = board position
-- Char refers to whose turn it is
data Position = Position String Char deriving (Show, Eq, Ord)
dim = 3
size :: Int
size = dim^2
initPosition = Position (replicate size ' ') 'X'
opposite 'X' = 'O'
opposite 'O' = 'X'
choose 'X' x o = x
choose 'O' x o = o

-- turn is whose turn it is ie {'X','O'}
-- idx = place where player is moving
-- move simpley changes the board
move :: Position -> Int -> Position
move (Position board turn) idx = Position (update idx turn board) (other turn)
  where update 0 turn (_:xs) = turn:xs
        update idx turn (x:xs) = x:(update (idx - 1) turn xs)
        other turn = choose turn 'O' 'X'


possibleMoves (Position board turn) = ' ' `elemIndices` board

isWinFor (Position board _) turn =
  any isLineMatch rows ||                           -- check rows
  any isLineMatch (transpose rows) ||                -- check cols
  isLineMatch (map (board!!) [0,dim+1..size-1]) ||  -- check diag1
  isLineMatch (map (board!!) [dim-1,dim*2-2..size-2]) -- check diag2
  where isLineMatch = all (==turn)
        rows = chunksOf dim board
 
 -- index = indexed-position where new move is
isMoveValid :: Int -> Position -> Bool
isMoveValid index position@(Position board turn) =
    if 0 <= index && index < size && board !! index == ' '
        then True
        else False 

render (Position board _) =
  unlines . intersperse (replicate (dim*4-1) '-') . map (intercalate "|") .
  chunksOf dim . map (pad . display) $ zip [0..] board
  where display (i,' ') = intToDigit i
        display (_, c) = c
        pad c = ' ':c:" "

pixelCoordToCell (x,y) = x' + (3 * y')
    where f j   = if j < 100 then 0 else 
                    if j < 200 then 1
                    else 2
          x'    = f x
          y'    = f y






