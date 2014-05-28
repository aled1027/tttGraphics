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
import Data.Char (toLower)

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
  pos@(Pos.Position board ch) <- GUG.get p
  -- lower case signifies game is over
  if ch `elem` ['-','x','o'] then do return ()
  -- else if upper case, the game goes on!
  else do
    case (key,keyState) of 
      (MouseButton LeftButton, Up) -> do 
        pos@(Pos.Position board ch) <- GUG.get p
        putStrLn (Pos.render pos)
        let newMove = Pos.pixelCoordToCell (x,y) -- maybe double check if this is working
        if (Pos.isMoveValid newMove pos) then do 
          let newPos@(Pos.Position newB newC) = Pos.move pos newMove -- update position
          let newStatus = Pos.positionStatus (Pos.Position newB ch)
          case newStatus of
            Pos.Win -> do
              p $=! Pos.Position newB (toLower ch) 
            Pos.Draw ->
              p $=! Pos.Position newB '-' 
            _ -> do
              let newPos'@(Pos.Position newB' newC') = Pos.move newPos (evalState (Eng.bestMove newPos) M.empty)
              let newStatus' = Pos.positionStatus (Pos.Position newB' newC)
              case newStatus' of
                Pos.Win -> do
                  p $=! Pos.Position newB' (toLower newC) 
                Pos.Draw ->
                  p $=! Pos.Position newB' '-' 
                _ -> do
        else do
            return ()
      _ -> return ()

--data Position = Position String Char deriving (Show, Eq, Ord)




