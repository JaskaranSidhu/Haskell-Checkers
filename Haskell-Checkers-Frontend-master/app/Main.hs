module Main where

--import Tui
import Checkers
import GameLogic
import CheckersJaskaranSidhu
 
main :: IO ()
main = blackAi black_ai apply_move initialGameState












-- applyMove :: Move -> GameState -> GameState 
-- applyMove m s | (elementOf m (moves s)) == True = (GameState {_blackPieces = [(1,0),(3,0),(5,0),(7,0),(0,1),(2,1),(4,1),(6,1),(1,2),(3,2),(5,2),(7,2)], _redPieces = [(0,7),(2,7),(4,7),(6,7),(1,6),(3,6),(5,6),(7,6),(0,5),(2,5),(4,5),(6,5)], _blackKings = [], _redKings = [], _status = Red, _message = "FOOK"}) 
--              | otherwise = (GameState {_blackPieces = [], _redPieces = [(0,7),(2,7),(4,7),(6,7),(1,6),(3,6),(5,6),(7,6),(0,5),(2,5),(4,5),(6,5)], _blackKings = [], _redKings = [], _status = Red, _message = ""})



-- elementOf :: Move -> [Move] -> Bool
-- elementOf m [] = False 
-- elementOf m (x:xs) | (m == x) = True
--                   | otherwise = elementOf m xs

-- module Main where

-- import Checkers
-- import Moves
 
-- main :: IO ()

-- main = human apply_move g6 
-- main = redAi (ai_move_red 10) apply_move initialGameState
-- main = blackAi (ai_move_black 10) apply_move initialGameState
-- main = aiTest (ai_move_red 10)  (ai_move_black 10) apply_move initialGameState

---------------------------------------------------------
--                       YOUR CODE GOES HERE!!!!
---------------------------------------------------------
--   define apply_move and supporting functions here (or make a module)
