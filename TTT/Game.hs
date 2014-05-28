module TTT.Game (
 render
) where

 --,GameStatus (..)
 -- Player (..)
 --,GameState (..)
import Control.Monad.State
import TTT.Position
import Data.Char
import Data.List.Split
import Data.List

import TTT.Engine
import Control.Monad.State
import qualified Data.Map as M

--data Player = Computer | Human deriving (Show, Eq, Ord)
--data GameState = GameState (StateT GameState IO String) [String]
--data GameStatus = Quit | ComputerWin | HumanWin | Draw deriving (Show, Eq, Ord)

-- this should all be replaced with graphics

