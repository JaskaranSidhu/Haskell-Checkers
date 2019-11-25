module CheckersJaskaranSidhu (moves
  ,apply_move
  ,red_ai
  ,black_ai)

where

import Checkers

black_ai :: GameState -> Move
black_ai s = getMoveMax (depthControl 4 (evaluate s (attachZero (moves s) 0 ) ) 2 s) 

-- Calling depthControl (depth) (list of moves) (are we generating max or min) (gamestate)
red_ai :: GameState -> Move 
red_ai s = getMoveMin (depthControl 4 (evaluate s (attachZero (moves s) 0) ) 1 s) 



depthControl :: Int -> [(Move, Int)] -> Int -> GameState -> [(Move, Int)]
depthControl i x j s | (i == 1) = x 
                     | (j == 1) = (makeChildren (i-1) x (j+1) s) 
                     | otherwise = (makeChildren (i-1) x (j-1) s) 


makeChildren :: Int -> [(Move, Int)] -> Int -> GameState -> [(Move, Int)]
makeChildren i [] j s = []
makeChildren i ((x,y):xs) j s | (j == 1) = (findMin ( depthControl i (evaluate (s) (attachZero (moves (apply_move x s)) 0)) j (apply_move x s)) (x,y)) ++ (makeChildren i xs j s) 
                              | otherwise = (findMax ( depthControl i (evaluate (s) (attachZero (moves (apply_move x s)) 0)) j (apply_move x s)) (x,y)) ++ (makeChildren i xs j s) 




evaluate :: GameState -> [(Move, Int)] -> [(Move, Int)]
evaluate s [] = []
evaluate s ((x,y):xs) = (attachNumber x y (apply_move x s)) ++ (evaluate s xs)


-- Need to do the thing [] = [] else ((x,y):xs) conditions ... the thing done in evaluate
attachNumber :: Move -> Int -> GameState -> [(Move, Int)]
attachNumber m i s = [ (m, (i + (((length (_blackPieces s)) - (length (_redPieces s))) + (2 * ( (length (_blackKings s)) - (length (_redKings s))))))) ]


attachZero :: [Move] -> Int -> [(Move, Int)]
attachZero [] i = []
attachZero (x:xs) i = [(x, i)] ++ attachZero xs i


findMin :: [(Move, Int)] -> (Move, Int) -> [(Move, Int)]
findMin ((x,y):[]) (m, i) | (m == []) = [(x, (y+i))]
findMin ((x,y):[]) (m, i) = [(m, (i+y))]
findMin ((a,b):(c,d):xs) (m, i) | (b >= d) = findMin ((c,d):xs) (m, i)
                                | otherwise = findMin ((a,b):xs) (m, i)
findMin a b = [b]

findMax :: [(Move, Int)] -> (Move, Int) -> [(Move, Int)]
findMax ((x,y):[]) (m, i) | (m == []) = [(x, (i+y))]
findMax ((x,y):[]) (m, i) = [(m, (i+y))]
findMax ((a,b):(c,d):xs) (m, i) | (b >= d) = findMax ((a,b):xs) (m, i)
                                | otherwise = findMax ((c,d):xs) (m, i)
findMax a b = [b]



getMoveMax :: [(Move, Int)] -> Move
getMoveMax ((a,b):[]) = a
getMoveMax ((a,b):(c,d):xs) | (b >= d) = getMoveMax ((a,b):xs)
                            | otherwise = getMoveMax ((c,d):xs)

getMoveMin :: [(Move, Int)] -> Move
getMoveMin ((a,b):[]) = a
getMoveMin ((a,b):(c,d):xs) | (b <= d) = getMoveMax ((a,b):xs)
                            | otherwise = getMoveMax ((c,d):xs)


apply_move :: Move -> GameState -> GameState 
apply_move m s | ((elementOf m (moves s)) == True) && (( (length (_blackKings (createGameState m s)) == 0) && (length (_blackPieces (createGameState m s)) == 0)) || ( (length (_redPieces (createGameState m s)) == 0) && (length (_redKings (createGameState m s)) == 0))) = (((createGameState m s){_message = "GameOver"}){_status = GameOver})
              | (elementOf m (moves s)) == True = ((createGameState m s){_message = ""})
              | otherwise = s{_message = "Invalid Move"}


elementOf :: Move -> [Move] -> Bool
elementOf m [] = False 
elementOf m (x:xs) | (m == x) = True
                   | otherwise = elementOf m xs


createGameState :: Move -> GameState -> GameState
createGameState m s | ((jump_moves s) == []) = simpleMoveUpdate m s
                    | otherwise = jumpMoveUpdate m s


simpleMoveUpdate :: Move -> GameState -> GameState
simpleMoveUpdate (x:y:xs) s | _status s == Red = (moveRed x y s){_status = Black}
                            | _status s == Black = (moveBlack x y s){_status = Red}
                            | otherwise = (moveBlack x y s){_status = GameOver}

jumpMoveUpdate :: Move -> GameState -> GameState
jumpMoveUpdate m s | _status s == Red = (jumpUpdateLoop m s){_status = Black}
                   | _status s == Black = (jumpUpdateLoop m s){_status = Red}
                   | otherwise = s{_status = GameOver}


jumpUpdateLoop :: Move -> GameState -> GameState 
jumpUpdateLoop (x:[]) s = s
jumpUpdateLoop (x:y:xs) s = jumpUpdateLoop (y:xs) (jumpUpdate x y s) 


jumpUpdate :: Coord -> Coord -> GameState -> GameState
jumpUpdate (a,b) (c,d) s | ((a+2, b+2) == (c,d)) = deleteCord (a,b) (a+1,b+1) (c,d) s
                         | ((a-2, b+2) == (c,d)) = deleteCord (a,b) (a-1,b+1) (c,d) s
                         | ((a+2, b-2) == (c,d)) = deleteCord (a,b) (a+1,b-1) (c,d) s
                         | otherwise = deleteCord (a,b) (a-1,b-1) (c,d) s


-- This is where I can implement my simple_moves and jump_moves. 


moves :: GameState -> [Move]
moves s | ((jump_moves s) ==  []) = simple_moves s
        | otherwise = jump_moves s


jump_moves :: GameState -> [Move]
jump_moves s | (_status s == GameOver) = []
             | (_status s == Red) = red_jump_moves (_redPieces s) (_redKings s) s
             | otherwise = black_jump_moves (_blackPieces s) (_blackKings s) s



red_jump_moves :: [Coord] -> [Coord] -> GameState -> [Move]
red_jump_moves rp rk s = (red_generate_jump rp s) ++ (red_generate_king_jump rk s)

black_jump_moves :: [Coord] -> [Coord] -> GameState -> [Move]
black_jump_moves bp bk s = (black_generate_jump bp s) ++ (black_generate_king_jump bk s)

-- This recurses through all the pieces in the list. 
red_generate_jump :: [Coord] -> GameState -> [Move]
red_generate_jump [] s = []
red_generate_jump (x:xs) s = (red_generate_jump_state x s) ++ (red_generate_jump xs s)

-- This recurses through all the pieces in the list. 
black_generate_jump :: [Coord] -> GameState -> [Move]
black_generate_jump [] s = []
black_generate_jump (x:xs) s = (black_generate_jump_state x s) ++ (black_generate_jump xs s)

-- This recurses through all the King pieces in the list. 
red_generate_king_jump :: [Coord] -> GameState -> [Move]
red_generate_king_jump [] s = []
red_generate_king_jump (x:xs) s = (generate_king_jump_state x s) ++ (red_generate_king_jump xs s)

-- This recurses through all the King pieces in the list. 
black_generate_king_jump :: [Coord] -> GameState -> [Move]
black_generate_king_jump [] s = []
black_generate_king_jump (x:xs) s = (generate_king_jump_state x s) ++ (black_generate_king_jump xs s)


-- Checks to see if a jump is avalible: 
  -- If not return []
  -- Else return the jumps.
red_generate_jump_state :: Coord -> GameState -> [Move]
red_generate_jump_state (x,y) s | ( ((jumpUpRight (x,y) s) ++ (jumpUpLeft (x,y) s)) == [] ) = []
                                | otherwise = ((jumpUpRight (x,y) s) ++ (jumpUpLeft (x,y) s))


-- Checks to see if a jump is avalible: 
  -- If not return []
  -- Else return the jumps.
black_generate_jump_state :: Coord -> GameState -> [Move]
black_generate_jump_state (x,y) s | ( ((jumpDownRight (x,y) s) ++ (jumpDownLeft (x,y) s)) == [] ) = [] 
                                  | otherwise = ((jumpDownRight (x,y) s) ++ (jumpDownLeft (x,y) s))


generate_king_jump_state :: Coord -> GameState -> [Move]
generate_king_jump_state (x,y) s | ( ( (kingJumpUpRight [] (x,y) s) ++ (kingJumpUpLeft [] (x,y) s) ++ (kingJumpDownLeft [] (x,y) s) ++ (kingJumpDownRight [] (x,y) s) ) == [] ) = []
                                 | otherwise = ( (kingJumpUpRight [(x,y)] (x,y) s) ++ (kingJumpUpLeft [(x,y)] (x,y) s) ++ (kingJumpDownLeft [(x,y)] (x,y) s) ++ (kingJumpDownRight [(x,y)] (x,y) s) )


kingJumpUpRight :: [Coord] -> Coord -> GameState -> [Move]
kingJumpUpRight a (x,y) s | ((x + 2) <= 7) && ((y - 2) >= 0) && (_status s == Red) && ( (isBlack ((x+1),(y-1)) s) == True ) && (isEmpty ((x+2),(y-2)) s == True) = checkAgainKing (a ++ [ (x+2,y-2) ]) (x+2,y-2) (deleteCord (x,y) (x+1,y-1) (x+2,y-2) s)
                          | ((x + 2) <= 7) && ((y - 2) >= 0) && (_status s == Black) && ( (isRed ((x+1),(y-1)) s) == True ) && (isEmpty ((x+2),(y-2)) s == True) = checkAgainKing (a ++ [ (x+2,y-2) ]) (x+2,y-2) (deleteCord (x,y) (x+1,y-1) (x+2,y-2) s)
                          | otherwise = []

kingJumpUpLeft :: [Coord] -> Coord -> GameState -> [Move]
kingJumpUpLeft a (x,y) s | ((x - 2) >= 0) && ((y - 2) >= 0) && (_status s == Red) && ( (isBlack ((x-1),(y-1)) s) == True ) && (isEmpty ((x-2),(y-2)) s == True) = checkAgainKing (a ++ [ (x-2,y-2) ]) (x-2,y-2) (deleteCord (x,y) (x-1,y-1) (x-2,y-2) s)
                         | ((x - 2) >= 0) && ((y - 2) >= 0) && (_status s == Black) && ( (isRed ((x-1),(y-1)) s) == True ) && (isEmpty ((x-2),(y-2)) s == True) = checkAgainKing (a ++ [ (x-2,y-2) ]) (x-2,y-2) (deleteCord (x,y) (x-1,y-1) (x-2,y-2) s)
                         | otherwise = []

kingJumpDownRight :: [Coord] -> Coord -> GameState -> [Move]
kingJumpDownRight a (x,y) s | ((x + 2) <= 7) && ((y + 2) <= 7) && (_status s == Red) && ( (isBlack ((x+1),(y+1)) s) == True ) && (isEmpty ((x+2),(y+2)) s == True) = checkAgainKing (a ++ [ (x+2,y+2) ]) (x+2,y+2) (deleteCord (x,y) (x+1,y+1) (x+2,y+2) s)
                            | ((x + 2) <= 7) && ((y + 2) <= 7) && (_status s == Black) && ( (isRed ((x+1),(y+1)) s) == True ) && (isEmpty ((x+2),(y+2)) s == True) = checkAgainKing (a ++ [ (x+2,y+2) ]) (x+2,y+2) (deleteCord (x,y) (x+1,y+1) (x+2,y+2) s)
                            | otherwise = []


kingJumpDownLeft :: [Coord] -> Coord -> GameState -> [Move]
kingJumpDownLeft a (x,y) s | ((x - 2) >= 0) && ((y + 2) <= 7) && (_status s == Red) && ( (isBlack ((x-1),(y+1)) s) == True ) && (isEmpty ((x-2),(y+2)) s == True) = checkAgainKing (a ++ [ (x-2,y+2) ]) (x-2,y+2) (deleteCord (x,y) (x-1,y+1) (x-2,y+2) s)
                           | ((x - 2) >= 0) && ((y + 2) <= 7) && (_status s == Black) && ( (isRed ((x-1),(y+1)) s) == True ) && (isEmpty ((x-2),(y+2)) s == True) = checkAgainKing (a ++ [ (x-2,y+2) ]) (x-2,y+2) (deleteCord (x,y) (x-1,y+1) (x-2,y+2) s)
                           | otherwise = [] 


checkAgainKing :: [Coord] -> Coord -> GameState -> [Move]
checkAgainKing a (x,y) s | ((kingJumpUpRight a (x,y) s) == [] ) && ((kingJumpUpLeft a (x,y) s) == [] ) && ((kingJumpDownRight a (x,y) s) == [] ) && ((kingJumpDownLeft a (x,y) s) == [] ) = [a] 
                         | otherwise = (kingJumpUpRight a (x,y) s) ++ (kingJumpUpLeft a (x,y) s) ++ (kingJumpDownRight a (x,y) s) ++ (kingJumpDownLeft a (x,y) s) 


deleteCord :: Coord -> Coord -> Coord -> GameState -> GameState
deleteCord (a,b) (x,y) (c,d) s | (isPiece (x,y) (_blackPieces s) == True ) = moveRed (a,b) (c,d) (deleteBP (x,y) s)
                               | (isPiece (x,y) (_blackKings s) == True ) = moveRed (a,b) (c,d) (deleteBK (x,y) s)
                               | (isPiece (x,y) (_redPieces s) == True ) = moveBlack (a,b) (c,d) (deleteRP (x,y) s)
                               | otherwise = moveBlack (a,b) (c,d) (deleteRK (x,y) s)

deleteBP :: Coord -> GameState -> GameState
deleteBP c s = s{_blackPieces = [x | x <- (_blackPieces s), x /= c]}

deleteBK :: Coord -> GameState -> GameState
deleteBK c s = s{_blackKings = [x | x <- (_blackKings s), x /= c]}

deleteRP :: Coord -> GameState -> GameState
deleteRP c s = s{_redPieces = [x | x <- (_redPieces s), x /= c]}

deleteRK :: Coord -> GameState -> GameState
deleteRK c s = s{_redKings = [x | x <- (_redKings s), x /= c]}

moveRed :: Coord -> Coord -> GameState -> GameState
moveRed (a,b) (c,d) s | (d == 0) && (isPiece (a,b) (_redPieces s) == True) = delUpdateRed (a,b) (c,d) (deleteRP (a,b) s) -- delete (a,b) from redPieces add (c,d) to redKings 
                      | (isPiece (a,b) (_redKings s) == True) = updateRK (a,b) (c,d) (deleteRK (a,b) s) -- Update (a,b) to (c,d) in redKings
                      | otherwise = updateRP (a,b) (c,d) (deleteRP (a,b) s) -- Update (a,b) to (c,d) in redPieces


moveBlack :: Coord -> Coord -> GameState -> GameState
moveBlack (a,b) (c,d) s | (d == 7) && (isPiece (a,b) (_blackPieces s) == True) = delUpdateBlack (a,b) (c,d) (deleteBP (a,b) s) -- delete (a,b) from blackPieces add (c,d) to blackKings 
                        | (isPiece (a,b) (_blackKings s) == True) = updateBK (a,b) (c,d) (deleteBK (a,b) s) -- Update (a,b) to (c,d) in blackKings
                        | otherwise = updateBP (a,b) (c,d) (deleteBP (a,b) s) -- Update (a,b) to (c,d) in blackPieces


updateRK :: Coord -> Coord -> GameState -> GameState
updateRK (a,b) (c,d) s = s{_redKings = ((_redKings s) ++ [(c,d)])}

updateRP :: Coord -> Coord -> GameState -> GameState
updateRP (a,b) (c,d) s = s{_redPieces = ((_redPieces s) ++ [(c,d)])}

updateBK :: Coord -> Coord -> GameState -> GameState
updateBK (a,b) (c,d) s = s{_blackKings = ((_blackKings s) ++ [(c,d)])}

updateBP :: Coord -> Coord -> GameState -> GameState
updateBP (a,b) (c,d) s = s{_blackPieces = ((_blackPieces s) ++ [(c,d)])}

delUpdateRed :: Coord -> Coord -> GameState -> GameState
delUpdateRed (a,b) (c,d) s = s{_redKings = ((_redKings s) ++ [(c,d)])}

delUpdateBlack :: Coord -> Coord -> GameState -> GameState
delUpdateBlack (a,b) (c,d) s = s{_blackKings = ((_blackKings s) ++ [(c,d)])}


jumpUpRight :: Coord -> GameState -> [Move]
jumpUpRight (x,y) s | ((x + 2) <= 7) && ((y - 2) == 0) && ( (isBlack ((x+1),(y-1)) s) == True) && (isEmpty ((x+2),(y-2)) s == True) = checkAgainKing ( [ (x,y),(x+2,y-2) ] ) (x+2,y-2) (deleteCord (x,y) (x+1,y-1) (x+2,y-2) s)
                    | ((x + 2) <= 7) && ((y - 2) >= 0) && (_status s == Red) && ( (isBlack ((x+1),(y-1)) s) == True ) && (isEmpty ((x+2),(y-2)) s == True) = checkAgainRed [ (x,y), (x+2,y-2) ] (x+2,y-2) (deleteCord (x,y) (x+1,y-1) (x+2,y-2) s)
                    | otherwise = []


jumpUpLeft :: Coord -> GameState -> [Move]
jumpUpLeft (x,y) s | ((x - 2) >= 7) && ((y - 2) == 0) && ( (isBlack ((x-1),(y-1)) s) == True) && (isEmpty ((x-2),(y-2)) s == True) = checkAgainKing ( [ (x,y),(x-2,y-2) ] ) (x-2,y-2) (deleteCord (x,y) (x-1,y-1) (x-2,y-2) s)
                   | ((x - 2) >= 0) && ((y - 2) >= 0) && (_status s == Red) && ( (isBlack ((x-1),(y-1)) s) == True ) && (isEmpty ((x-2),(y-2)) s == True) = checkAgainRed [ (x,y), (x-2,y-2) ] (x-2,y-2) (deleteCord (x,y) (x-1,y-1) (x-2,y-2) s)
                   | otherwise = []


jumpDownRight :: Coord -> GameState -> [Move]
jumpDownRight (x,y) s | ((x + 2) <= 7) && ((y + 2) == 7) && ( (isRed ((x+1),(y+1)) s) == True) && (isEmpty ((x+2),(y+2)) s == True) = checkAgainKing ( [ (x,y),(x+2,y+2) ] ) (x+2,y+2) (deleteCord (x,y) (x+1,y+1) (x+2,y+2) s)
                      | ((x + 2) <= 7) && ((y + 2) <= 7) && (_status s == Black) && ( (isRed ((x+1),(y+1)) s) == True ) && (isEmpty ((x+2),(y+2)) s == True) = checkAgainBlack [ (x,y), (x+2,y+2) ] (x+2,y+2) (deleteCord (x,y) (x+1,y+1) (x+2,y+2) s)
                      | otherwise = []


jumpDownLeft :: Coord -> GameState -> [Move]
jumpDownLeft (x,y) s | ((x - 2) >= 0) && ((y + 2) == 7) && ( (isRed ((x-1),(y+1)) s) == True) && (isEmpty ((x-2),(y+2)) s == True) = checkAgainKing ( [ (x,y),(x-2,y+2) ] ) (x-2,y+2) (deleteCord (x,y) (x-1,y+1) (x-2,y+2) s)
                     | ((x - 2) >= 0) && ((y + 2) <= 7) && (_status s == Black) && ( (isRed ((x-1),(y+1)) s) == True ) && (isEmpty ((x-2),(y+2)) s == True) = checkAgainBlack [ (x,y), (x-2,y+2) ] (x-2,y+2) (deleteCord (x,y) (x-1,y+1) (x-2,y+2) s)
                     | otherwise = []



checkAgainRed :: [Coord] -> Coord -> GameState -> [Move]
checkAgainRed a (x,y) s | ((newJumpUpRight a (x,y) s) == [] ) && ((newJumpUpLeft a (x,y) s) == [] ) = [a] 
                        | otherwise = (newJumpUpRight a (x,y) s) ++ (newJumpUpLeft a (x,y) s) 

checkAgainBlack :: [Coord] -> Coord -> GameState -> [Move]
checkAgainBlack a (x,y) s | ((newJumpDownRight a (x,y) s) == [] ) && ((newJumpDownLeft a (x,y) s) == [] ) = [a] 
                          | otherwise = (newJumpDownRight a (x,y) s) ++ (newJumpDownLeft a (x,y) s) 


newJumpUpRight :: [Coord] -> Coord -> GameState -> [Move]
newJumpUpRight a (x,y) s | ((x + 2) <= 7) && ((y - 2) == 0) && ( (isBlack ((x+1),(y-1)) s) == True) && (isEmpty ((x+2),(y-2)) s == True) = checkAgainKing (a ++ [ (x+2,y-2) ] ) (x+2,y-2) (deleteCord (x,y) (x+1,y-1) (x+2,y-2) s)
                         | ((x + 2) <= 7) && ((y - 2) >= 0) && (_status s == Red) && ( (isBlack ((x+1),(y-1)) s) == True ) && (isEmpty ((x+2),(y-2)) s == True) = checkAgainRed (a ++ [ (x+2,y-2) ]) (x+2,y-2) (deleteCord (x,y) (x+1,y-1) (x+2,y-2) s)
                         | otherwise = []

newJumpUpLeft :: [Coord] -> Coord -> GameState -> [Move]
newJumpUpLeft a (x,y) s | ((x - 2) >= 7) && ((y - 2) == 0) && ( (isBlack ((x-1),(y-1)) s) == True) && (isEmpty ((x-2),(y-2)) s == True) = checkAgainKing (a ++ [ (x-2,y-2) ] ) (x-2,y-2) (deleteCord (x,y) (x-1,y-1) (x-2,y-2) s)
                        | ((x - 2) >= 0) && ((y - 2) >= 0) && (_status s == Red) && ( (isBlack ((x-1),(y-1)) s) == True ) && (isEmpty ((x-2),(y-2)) s == True) = checkAgainRed (a ++ [ (x-2,y-2) ]) (x-2,y-2) (deleteCord (x,y) (x-1,y-1) (x-2,y-2) s)
                        | otherwise = []

newJumpDownRight :: [Coord] -> Coord -> GameState -> [Move]
newJumpDownRight a (x,y) s | ((x + 2) <= 7) && ((y + 2) == 7) && ( (isRed ((x+1),(y+1)) s) == True) && (isEmpty ((x+2),(y+2)) s == True) = checkAgainKing (a ++ [ (x+2,y+2) ] ) (x+2,y+2) (deleteCord (x,y) (x+1,y+1) (x+2,y+2) s)
                           | ((x + 2) <= 7) && ((y + 2) <= 7) && (_status s == Black) && ( (isRed ((x+1),(y+1)) s) == True ) && (isEmpty ((x+2),(y+2)) s == True) = checkAgainBlack (a ++ [ (x+2,y+2) ]) (x+2,y+2) (deleteCord (x,y) (x+1,y+1) (x+2,y+2) s)
                           | otherwise = []


newJumpDownLeft :: [Coord] -> Coord -> GameState -> [Move]
newJumpDownLeft a (x,y) s | ((x - 2) >= 0) && ((y + 2) == 7) && ( (isRed ((x-1),(y+1)) s) == True) && (isEmpty ((x-2),(y+2)) s == True) = checkAgainKing (a ++ [ (x-2,y+2) ] ) (x-2,y+2) (deleteCord (x,y) (x-1,y+1) (x-2,y+2) s)
                          | ((x - 2) >= 0) && ((y + 2) <= 7) && (_status s == Black) && ( (isRed ((x-1),(y+1)) s) == True ) && (isEmpty ((x-2),(y+2)) s == True) = checkAgainBlack (a ++ [ (x-2,y+2) ]) (x-2,y+2) (deleteCord (x,y) (x-1,y+1) (x-2,y+2) s)
                          | otherwise = []                    




isBlack :: Coord -> GameState -> Bool
isBlack c s | ((isPiece c (_blackPieces s)) == False) && ((isPiece c (_blackKings s)) == False) = False
            | otherwise = True

isRed :: Coord -> GameState -> Bool
isRed c s | ((isPiece c (_redPieces s)) == False) && ((isPiece c (_redKings s)) == False) = False
          | otherwise = True 

simple_moves :: GameState -> [Move]
simple_moves s | (_status s == GameOver) = [] 
               | (_status s == Red) = red_simple_moves (_redPieces s) (_redKings s) s
               | otherwise = black_simple_moves (_blackPieces s) (_blackKings s) s


red_simple_moves :: [Coord] -> [Coord] -> GameState -> [Move]
red_simple_moves rp rk s = (red_generate_moves rp s) ++ (red_generate_king_moves rk s)

black_simple_moves :: [Coord] -> [Coord] -> GameState -> [Move]
black_simple_moves bp bk s = (black_generate_moves bp s) ++ (black_generate_king_moves bk s)


red_generate_king_moves :: [Coord] -> GameState -> [Move]
red_generate_king_moves [] s = []
red_generate_king_moves (x:xs) s = (red_generate_king_state x s) ++ (red_generate_king_moves xs s)

black_generate_king_moves :: [Coord] -> GameState -> [Move]
black_generate_king_moves [] s = []
black_generate_king_moves (x:xs) s = (black_generate_king_state x s) ++ (red_generate_king_moves xs s)


red_generate_moves :: [Coord] -> GameState -> [Move]
red_generate_moves [] s = [] 
red_generate_moves (x:xs) s = (red_generate_state x s) ++ (red_generate_moves xs s)

black_generate_moves :: [Coord] -> GameState -> [Move]
black_generate_moves [] s = []
black_generate_moves (x:xs) s = (black_generate_state x s) ++ (black_generate_moves xs s)


red_generate_king_state :: Coord -> GameState -> [Move]
red_generate_king_state (x,y) s = ( (upRight (x,y) s) ++ (upLeft (x,y) s) ++ (downRight (x,y) s) ++ (downLeft (x,y) s) )

black_generate_king_state :: Coord -> GameState -> [Move]
black_generate_king_state (x,y) s = ( (upRight (x,y) s) ++ (upLeft (x,y) s) ++ (downRight (x,y) s) ++ (downLeft (x,y) s) )


red_generate_state :: Coord -> GameState -> [Move]
red_generate_state (x,y) s = ( (upRight (x,y) s) ++ (upLeft (x,y) s) )

black_generate_state :: Coord -> GameState -> [Move]
black_generate_state (x,y) s = ( (downRight (x,y) s) ++ (downLeft (x,y) s) )


upRight :: Coord -> GameState -> [Move]
upRight (x,y) s | ( ((x + 1) <= 7) && ((y - 1) >= 0) && ((isEmpty ((x+1), (y-1)) s) == True) ) = [[ (x,y),((x+1),(y-1)) ]]
                | otherwise = []

upLeft :: Coord -> GameState -> [Move]
upLeft (x,y) s | ( ((x - 1) >= 0) && ((y - 1) >= 0) && ((isEmpty ((x-1), (y-1)) s) == True) ) = [[ (x,y),((x-1),(y-1)) ]]
               | otherwise = []

downLeft :: Coord -> GameState -> [Move]
downLeft (x,y) s | ( ((x - 1) >= 0) && ((y + 1) <= 7) && ((isEmpty ((x-1), (y+1)) s) == True) ) = [[ (x,y),((x-1),(y+1)) ]]
                 | otherwise = []

downRight :: Coord -> GameState -> [Move]
downRight (x,y) s | ( ((x + 1) <= 7) && ((y + 1) <= 7) && ((isEmpty ((x+1), (y+1)) s) == True) ) = [[ (x,y),((x+1),(y+1)) ]]
                  | otherwise = []


isEmpty :: Coord -> GameState -> Bool
isEmpty c s | (((isPiece c (_blackPieces s)) == False) && ((isPiece c (_blackKings s)) == False) && ((isPiece c (_redPieces s)) == False) && ((isPiece c (_redKings s)) == False)) = True
            | otherwise = False


isPiece :: Coord -> [Coord] -> Bool
isPiece (x,y) [] = False
isPiece (x,y) ((a,b):xs) | ((x == a) && (y == b)) = True
                         | otherwise = isPiece (x,y) (xs)

