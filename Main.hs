-- my modules
import Display 
import qualified TTT.Position as Pos
import qualified TTT.Engine as Eng

-- for graphics
import Graphics.UI.GLUT
import qualified Graphics.UI.GLUT as GUG -- for specifying which get

-- Utilities
import Data.IORef
import Control.Monad.State (evalState)
import qualified Data.Map as M (empty)

main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Hello World"
  reshapeCallback $= Just reshape
  p <- newIORef Pos.initPosition
  keyboardMouseCallback $= Just (keyboardMouse p)
  idleCallback $= Just idle 
  displayCallback $= display p
  mainLoop -- where glut takes over

-- add check for draw
-- maybe change functoin to "let x = isOver position; case x of _ -> ..."

-- only reacts if we have a up with the left mouse button
-- This is essentially our game loop, because all actions 
-- are in response to the player clicking the cell where they want to play
keyboardMouse p key keyState modifiers (Position x y) = do
  case (key,keyState) of 
    (MouseButton LeftButton, Up) -> do 
      pos@(Pos.Position board ch) <- GUG.get p
      putStrLn (Pos.render pos)
      let newMove = Pos.pixelCoordToCell (x,y) -- maybe double check if this is working
      if (Pos.isMoveValid newMove pos) then do 
        let newPos@(Pos.Position newBoard newCh) = Pos.move pos newMove -- update position
        if (newPos `Pos.isWinFor` ch) then do 
          print ch; print "wins";
        else do
            print "comp move"
            putStrLn (Pos.render newPos)
            let newPos'@(Pos.Position d e) = Pos.move newPos (evalState (Eng.bestMove newPos) M.empty)
            if (newPos' `Pos.isWinFor` (Pos.opposite ch))
              then do print (Pos.opposite ch); print "wins";
              else do 
                print "user move"
                putStrLn (Pos.render newPos')
                p $=! newPos';
      else do
          return ()
    _ -> return ()

--data Position = Position String Char deriving (Show, Eq, Ord)




