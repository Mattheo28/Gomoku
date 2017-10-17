{-# LANGUAGE DeriveGeneric #-}

module Board where

import Data.Bool
import Data.Maybe
import Data.Ix
import Data.List
import Debug.Trace
import Graphics.Gloss
import GHC.Generics

data Col = Black | White
  deriving (Read, Show, Eq)

{-| Return the opposite colour to the one given. -}
other :: Col -> Col
other Black = White
other White = Black

type Position = (Int, Int)


{-| A Board is a record containing the board size, the target number in a row and 
all the location and colour of pieces on the board-}
data Board = Board { size :: Int,
                     target :: Int,
                     pieces :: [(Position, Col)],
		             -- the difficulty of the AI
		             difficulty :: Int,
                     -- whether the three and three rule is applied
                     threeRule :: Bool,
                     -- whether the four and four rule is applied
                     fourRule :: Bool,
                     capturingRule :: Bool,
                     capturedPairsBlack :: Int,
                     capturedPairsWhite :: Int
                   }
  deriving (Read, Show)

{-| Contains information as to who's turn it is, the current state of the board, and other variables that
control the game play-}
data World = World { board :: Board,
                     turn :: Col,
                     -- whether the AI is playing
		             ai :: Bool,
		             -- the colour of the AI
		             aicol :: Col,
		             -- whether a hint should be displayed
		             hint :: Bool
                     }
  deriving (Read, Show)

{-| All of the directions that you can check the board in -}
data Direction = NW | N | NE | E | W | SE | SW | S
  deriving (Show, Eq)

{-| All of the potential directions to check the game in -}
northwest, north, northeast, east, west, southeast, southwest, south :: Dir

{-| The directions with the values that must be applied to the x and y values to follow the given direction -}
northwest = Dir NW (-1) (-1)
north = Dir N 0 (-1)
northeast = Dir NE 1 (-1)
east = Dir E 1 0
west = Dir W (-1) 0
southeast = Dir SE 1 1
southwest = Dir SW (-1) 1
south  = Dir S 0 1

-- A list of all the possible directions which can be used by any function
directions = [northwest, north, northeast, east, west, southeast, southwest, south]
filepath = "Savefile.txt"

{-| Dir data type, containing a direction, and two values representing what must be done to the x and y values
in order to go in the given direction. -}
data Dir = Dir {direction :: Direction,
                  xCoordDisplacement :: Int,
                  yCoordDisplacement :: Int}
  deriving (Show, Eq)

{-| Recursive function to merge two lists that contain the same type-}
merge2Lists :: [a] -> [a] -> [a]
merge2Lists [] ys = ys
merge2Lists (x:xs) ys = x:merge2Lists ys xs

{-| Recursive function to convert a list of ints to a list of tuples containing two ints
E.g. [1, 2, 3, 4, 5, 6] -> [(1, 2), (3, 4), (5, 6)] -}
listToTuple :: [Int] -> [(Int, Int)]
listToTuple [] = []
listToTuple (a:b:c) = (a, b) : listToTuple c

{-| Function to get a list of tuples, where the first value is a tuple containing two integers (position), and the
second value is a direction. This is used to check the whole board efficiently (nothing is checked more than once).
The Boolean given is to indicate which list is needed, as 2 are possible (search the whole board from the left
going down, and from the right going up). The integer given is the size of the board-}
getListOfCoords :: Bool -> Int -> [((Int, Int), Dir)]
-- generate the list to search from the left and going down, using the map function, the range function, and the
-- above function to merge 2 lists
getListOfCoords True max = nub (merge2Lists (map (\x->(x,southwest)) (range ((max, 0), (max, max))))
                               (merge2Lists (map (\x->(x,southwest)) (range ((0, 0), (max, 0))))
                               (merge2Lists (merge2Lists (map (\x->(x,south)) (range ((0, 0), (max, 0))))
                               (map (\x->(x,southeast)) (range ((0, 0), (max, 0)))))
                               (merge2Lists (map (\x->(x,east)) (range ((0, 0), (0, max))))
                               (map (\x->(x,southeast)) (range ((0, 0), (0, max))))))))
-- generate the list to search from the right and going up, using the map function, the range function, and the
-- above function to merge 2 lists
getListOfCoords False max = nub (merge2Lists (map (\x->(x,northeast)) (range ((max, 0), (max, max))))
                                (merge2Lists (map (\x->(x,northeast)) (range ((0, 0), (0, max))))
                                (merge2Lists (merge2Lists (map (\x->(x,north)) (range ((0, max), (max, max))))
                                (map (\x->(x,northwest)) (range ((0, max), (max, max)))))
                                (merge2Lists (map (\x->(x,west)) (range ((max, 0), (max, max))))
                                (map (\x->(x,northwest)) (range ((max, 0), (max, max))))))))


{-| Play a move on the board; return 'Nothing' if the move is invalid or the game is finished-}
makeMove :: World -> Col -> Position -> Maybe Board
makeMove world col pos
                          -- if someone has won, return Nothing
                         | isJust (checkWon (target (board world)) (board world)) = Nothing
                          -- if the move is valid (in range and not taken), and the three and three rule
                          -- and the four and four rule return True, return a new board with the added piece
                         | (pieceAlreadyInPosition pos (pieces (board world)) == False) &&
                           (positionInRange pos (board world) == True) = Just removedCaptures
                           -- if invalid, return nothing
                         | otherwise = Nothing
        where
            -- add the piece to the board
            newBoard = (board world) {pieces = (pieces (board world)) ++ [(pos,col)]}
            removedCaptures = case (capturingRule newBoard) of
                                   True      -> removeCapturedPieces newBoard
                                   otherwise -> newBoard

{-| Function to check for the 'four and four' rule -}
checkFourRule :: Board -> Bool
checkFourRule board
                 -- if the rule is being applied to the game, check that the player who last played doesn't have
                 -- more than 1 row with 4 pieces, using the generated list of coordinates and directions to check
                 -- the whole board
                | (fourRule board) == True = if sum (map (checkCellDir 4 board Black 0 0) coordsAndDirs) > 1 then False
                                             else True
			    | otherwise = True
  where
    listToCheckBoard = getListOfCoords True (size board)
    coordsAndDirs = zip (listToTuple (merge2Lists (map fromIntegral (map fst (map fst listToCheckBoard)))
                        (map fromIntegral (map snd (map fst listToCheckBoard))))) (map snd listToCheckBoard)

{-| Function to check for the 'three and three' rule -}
checkThreeRule :: Board -> Bool
checkThreeRule board
                 -- if the rule is being applied, check using the first generated list of coords and directions
                 -- that the last player who played doesn't have more than 1 open row of 3 pieces
                | sum (map (getNumOfOpenRows board Black 0 3 0) (coordsAndDirs)) > 1 && threeRule board = False
                 -- if the above search failed, try again using the other list of coords and dirs, to check
                 -- the board for the same thing in the other direction, because the first list will not find
                 -- an open row which is open from the back
                | sum (map (getNumOfOpenRows board Black 0 3 0) (oppCoordsAndDirs)) > 1 && threeRule board = False
			    | otherwise = True
  where
    -- list to check from left and down
    listToCheckBoard = getListOfCoords True (size board)
    -- list to check from right and up
    oppListToCheckBoard = getListOfCoords False (size board)
    -- coords and dirs to check from left and down
    coordsAndDirs = zip (listToTuple (merge2Lists (map fromIntegral (map fst (map fst listToCheckBoard)))
                        (map fromIntegral (map snd (map fst listToCheckBoard))))) (map snd listToCheckBoard)
    -- coords and dirs to check from right and up
    oppCoordsAndDirs = zip (listToTuple (merge2Lists (map fromIntegral (map fst (map fst oppListToCheckBoard)))
                            (map fromIntegral (map snd (map fst oppListToCheckBoard))))) (map snd oppListToCheckBoard)

{-| Check for amount of times Black has a row that exceeds the target amount. -}
checkForMoreThanTarget :: Int -> Board -> Int -> Int
checkForMoreThanTarget target board total
                                    | target == ((size board) + 1) = total
                                    | otherwise = checkForMoreThanTarget (target + 1) board
                                                  (total + (sum (map (checkCellDir target board Black 0 0)
                                                  coordsAndDirs)))
  where
    -- generated list of coords and directions to check the board
    listToCheckBoard = getListOfCoords True (size board)
    coordsAndDirs = zip (listToTuple (merge2Lists (map fromIntegral (map fst (map fst listToCheckBoard)))
                        (map fromIntegral (map snd (map fst listToCheckBoard))))) (map snd listToCheckBoard)

{-| Function to undo the last move. -}
undoMove :: World -> World
undoMove world
                 -- if it is player vs player, undo the last move (remove the last piece in the pieces array), and
                 -- switch the turn to the other player
                | (ai world) == False = world{board = (board world){pieces = take ((length (pieces (board world))) - 1)
                                             (pieces (board world))}, turn = other (turn world)}
                 -- if it is player vs AI, undo the last 2 moves (the AI's and the players) because there is no
                 -- point just removing the AI's move, as it will just make the same move again.
                | otherwise = world{board = (board world){pieces = take ((length (pieces (board world))) - 2)
                                                                        (pieces (board world))}}


{-| takes a position and checks if it is in the list of positions -}
pieceAlreadyInPosition :: Position -> [(Position, Col)] -> Bool
pieceAlreadyInPosition pos [] = False
pieceAlreadyInPosition pos (x:xs)
                                  | pos == fst x = True
                                  | otherwise    = pieceAlreadyInPosition pos xs

{-| Determines if the position is in the range of values (0...board size) -}
positionInRange :: Position -> Board -> Bool
positionInRange pos board
                            | (fst pos <= size board) && (fst pos >= 0)
                              && (snd pos <= size board) && (snd pos >= 0) = True
                            | otherwise = False


{-| Checks whether the White or Black player has won the game. Returns Just (winner) or Nothing if nobody has won.
It takes an Int representing the target of the game (this is done so it can be used by other functions to check for
other numbers of pieces in a row). -}
checkWon :: Int -> Board -> Maybe Col
checkWon target board
                | (checkThreeRule board) == False || (checkFourRule board) == False ||
                  (((checkForMoreThanTarget (target + 1) board 0)/= 0) && (threeRule board == True))
                  = Just White
                | (capturingRule board) == True && (capturedPairsBlack board) >= 5 = Just Black
                | (capturingRule board) == True && (capturedPairsWhite board) >= 5 = Just White
                 -- map the checkCellDir function on a list of coords and directions, generated using above functions.
                 -- If the function returns anything above 0, the person who last played has won.
                | sum (map (checkCellDir target board col 0 0) coordsAndDirs) > 0 = Just col
                | otherwise = Nothing
  where
    -- the player who last played
    col = (snd (last (pieces board)))
    -- generated list of coords and directions to check the board
    listToCheckBoard = getListOfCoords True (size board)
    coordsAndDirs = zip (listToTuple (merge2Lists (map fromIntegral (map fst (map fst listToCheckBoard)))
                        (map fromIntegral (map snd (map fst listToCheckBoard))))) (map snd listToCheckBoard)

{-| Remove pieces that are captured from a board -}
removeCapturedPieces :: Board -> Board
removeCapturedPieces board = case listOfCaptures of
                                  []        -> board
                                  otherwise ->
                                    case col of
                                         -- change the board and add to the captured Pairs for the right colour
                                         Black     -> board{pieces =
                                                      (filter (\pos -> pos `notElem` listOfCaptures)
                                                      (pieces board)),
                                                      capturedPairsWhite = succ (capturedPairsWhite board)}
                                         otherwise -> board{pieces =
                                                      (filter (\pos -> pos `notElem` listOfCaptures)
                                                      (pieces board)),
                                                      capturedPairsBlack = succ (capturedPairsBlack board)}


  where
    -- get the list of captured pieces
    listOfCaptures = concat (map (getCapturedPieces board) (pieces board))
    -- get the colour of the captured pieces
    col = case listOfCaptures of
               []        -> Black
               otherwise -> snd (listOfCaptures !! 0)

{-| Get a list of captured pieces on a board starting from 1 piece -}
getCapturedPieces :: Board -> (Position, Col) -> [(Position, Col)]
getCapturedPieces board pos = concat (map (getCapturedPiecesInDir board (pieces board) pos)
                                     [north, east, southeast, northeast])

{-| Get a list of captured pieces on a board from 1 piece in 1 direction -}
getCapturedPiecesInDir :: Board -> [(Position, Col)] -> (Position, Col) -> Dir ->
                     [(Position, Col)]
getCapturedPiecesInDir board pieces (pos, col) dir
                 | checkCellDir 2 board col 0 0 (pos, dir) > 0 = if previousCoords `elem` pieces &&
                                                                    next2Coords `elem` pieces then newList
                                                                  else []
                 | otherwise = []
  where
    -- next captured piece
    nextCoords = (((fst pos + xCoordDisplacement dir), (snd pos + yCoordDisplacement dir)), col)
    -- previous piece
    previousCoords = (((fst pos - xCoordDisplacement dir), (snd pos - yCoordDisplacement dir)), other col)
    -- piece 2 positions away
    next2Coords = (((fst pos + ((xCoordDisplacement dir) * 2)),
                    (snd pos + ((yCoordDisplacement dir) * 2))), other col)
    newList = [(pos, col)] ++ [nextCoords]

{-| Recursive function to check how many times a target of pieces in a row has been reached, given a starting position
-- and a direction in which to check. If the row exceeds the target, it isn't added to the total. -}
checkCellDir :: Int -> Board -> Col -> Int -> Int -> (Position, Dir) -> Int
checkCellDir target board col count total (pos, dir)
                                         -- if the count has reached the target and the position isn't on the board,
                                         -- increment the total and return it, as this is valid. If the given position
                                         -- is valid, of the same initial colour and on the board, the target has been
                                         -- reached, so keep looking for more in the same direction and increment total
                                        | count == target = if (positionInRange pos board == False) then
                                                                (total + 1)
                                                            else if ((pos, col) `notElem` (pieces board)) then
                                                                      checkNextAndIncrementTotal
                                                                 else checkNext
                                         -- if position not on board, we reached the end, return total. If position
                                         -- has a piece of the right colour, increment counter and check next, otherwise
                                         -- check next with counter set to 0
                                        | otherwise = if (positionInRange pos board == False) then total
                                                      else if ((pos, col) `elem` (pieces board)) then
                                                                checkNextAndIncrementCounter
                                                           else checkNext
  where
    newCoords = ((fst pos + xCoordDisplacement dir), (snd pos + yCoordDisplacement dir))
    checkNext = checkCellDir target board col 0 total (newCoords, dir)
    checkNextAndIncrementCounter = checkCellDir target board col (count + 1) total (newCoords, dir)
    checkNextAndIncrementTotal = checkCellDir target board col 0 (total + 1) (newCoords, dir)

{-| Recursive function to get the number of open rows (either both ends or just one) for a player, works
in a similar way to checkCellDir. -}
getNumOfOpenRows :: Board -> Col -> Int -> Int -> Int -> (Position, Dir) -> Int
getNumOfOpenRows board col count target total (pos, dir)
                                         -- if the count has reached the target and the given position isn't valid,
                                         -- we have reached the end so return the total. If the given position
                                         -- is valid, of the same initial colour and on the board, the target has been
                                         -- reached, so keep looking for more in the same direction and increment total
                                        | count == target = if (positionInRange pos board == False) then total
                                                            else if (pos `notElem` (map fst (pieces board))) then
                                                                     checkNextAndIncrementTotal
                                                                 else checkNext
                                         -- if position not on board, we reached the end, return total. If position
                                         -- has a piece of the right colour, increment counter and check next, otherwise
                                         -- check next with counter set to 0
                                        | otherwise = if (positionInRange pos board == False) then total
                                                      else if ((pos, col) `elem` (pieces board)) then
                                                                checkNextAndIncrementCounter
                                                           else checkNext
  where
    newCoords = ((fst pos + xCoordDisplacement dir), (snd pos + yCoordDisplacement dir))
    checkNext = getNumOfOpenRows board col 0 target total (newCoords, dir)
    checkNextAndIncrementCounter = getNumOfOpenRows board col (count + 1) target total (newCoords, dir)
    checkNextAndIncrementTotal = getNumOfOpenRows board col 0 target (total + 1) (newCoords, dir)

{-| Check if a given piece has any piece adjacent to it, given a list of directions -}
checkIfAdj :: [Dir] -> Position -> [(Position, Col)] -> Bool
checkIfAdj [] pos pieces = False
checkIfAdj dirs pos pieces
                                 -- if there is a piece in the first direction, return True
                                | newPos `elem` listOfPos = True
                                 -- recursive call with the next directions
                                | otherwise = checkIfAdj (tail dirs) pos pieces
  where
    -- new position in the first direction
    newPos = ((fst pos + xCoordDisplacement (head dirs)), (snd pos + yCoordDisplacement (head dirs)))
    listOfPos = map fst pieces

{-| Check for an open row in a specific direction for a specific piece, given the number of pieces there should
be in the row. -}
checkForOpenInDir :: Board -> Col -> (Position, Col) -> [(Position, Col)] -> (Dir, Int) -> Int
checkForOpenInDir board col (a, b) pieces (dir, target)
                                     -- check the next position after target is reached, this shouldn't be in the pieces
                                     -- of the board for the row to be open
                                    | target == 0 = if (a, b) `elem` pieces then 0
                                                    -- check if the current position belongs to the other player
                                                    else if (a, (other b)) `elem` pieces then 0
                                                         -- check that the position is on the board
                                                         else if positionInRange a board == False then 0
							                                  else 1
							         -- check that the piece in the current position is on the board and the right
							         -- player's, and if so, make a recursive call with the target reduced by 1
                                    | (a, b) `elem` pieces && b == col = checkForOpenInDir board col (newCoords, col)
                                                                                           pieces (dir, (target - 1))
                                    | otherwise = 0
  where
      newCoords = ((fst a + xCoordDisplacement dir), (snd a + yCoordDisplacement dir))

{-| Find the number of open rows that starts from each piece for a given player,
given the number of pieces there should be in the row. -}
findNumOfOpensPerPiece :: Board -> [Dir] -> Col -> Int -> [(Position, Col)] -> (Position, Col) -> Int -> Int
-- if checked all directions, return total
findNumOfOpensPerPiece board [] col target pieces loc total = total
-- make a recursive call, and add to the current total the number of opens in the given direction using checkForOpenInDir
findNumOfOpensPerPiece board dirs col target pieces loc total = findNumOfOpensPerPiece board (tail dirs) col target
                                                                pieces loc (total + (checkForOpenInDir board col loc
                                                                pieces ((head dirs), target)))

{-| Find the number of open rows that a player has (if opened on both ends, coutns as 2, as this is even better than
open on 1 end for the AI's decision). The 'target' given is the number of pieces in the row. -}
findNumOfOpens :: Board -> [Dir] -> Col -> Int -> [(Position, Col)] -> Int -> Int
-- if checked all positions, return total
findNumOfOpens board dirs col target [] total = total
findNumOfOpens board dirs col target locs total
                                     -- if the piece is the right colour, make a recursive call, adding to the total
                                     -- the number returned when calling findNumOfOpens with the current target
                                    | (col == snd (head locs)) = findNumOfOpens board dirs col target (tail locs)
                                                                 (findNumOfOpensPerPiece board dirs col target
                                                                 (pieces board) (head locs) total)
                                     -- make a recursive call without changing the total
                                    | otherwise        = findNumOfOpens board dirs col target (tail locs) total

{-| Get a score for a board in terms of number of open rows, the more pieces in each row, the higher the score. -}
evaluateNumOfOpens :: Col -> Board -> Int -> Int -> Int
evaluateNumOfOpens c b num total
                                 -- lowest row has been searched for, return the total
                                | num == (target b) = total
                                 -- make recursive call and increment the number which decrements the number of pieces
                                 -- in the row we are searching for. Add to the total the number of open rows with the
                                 -- current number using findNumOfOpens
                                | otherwise = evaluateNumOfOpens c b (num + 1)
                                              (total + ((findNumOfOpens b directions c
                                              num (pieces b) 0)))

{-| Get the number of open rows on BOTH sides with a given number of pieces, for a specific player -}
getNumOfOpensBothSides :: Col -> Board -> [(Position, Col)] -> [(Position, Col)] -> Int -> Int -> Int
getNumOfOpensBothSides col board [] pieces target total = total
getNumOfOpensBothSides col board ((a, b) : rest) pieces target total
                                                    -- check the board with the 4 different lists
                                                    | b == col &&
                                                      (map (checkForOpenInDir board col (a, b) pieces) list1) == [1, 1]
                                                      = checkNextIncrement
                                                    | b == col &&
                                                      (map (checkForOpenInDir board col (a, b) pieces) list2) == [1, 1]
                                                      = checkNextIncrement
                                                    | b == col &&
                                                      (map (checkForOpenInDir board col (a, b) pieces) list3) == [1, 1]
                                                      = checkNextIncrement
                                                    | b == col &&
                                                      (map (checkForOpenInDir board col (a, b) pieces) list4) == [1, 1]
                                                      = checkNextIncrement
                                                    | otherwise = getNumOfOpensBothSides col board rest pieces target
                                                                  total
  where
    -- 4 different lists with a direction to check in and the opposite of the direction to check behind the given row
    list1 = [(south, target), (north, 1)]
    list2 = [(east, target), (west, 1)]
    list3 = [(southeast, target), (northwest, 1)]
    list4 = [(northeast, target), (southwest, 1)]
    checkNextIncrement = getNumOfOpensBothSides col board rest pieces target (total + 1)

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Col -> Board -> Int
evaluate c b
                -- Level 1 - if the colour of the player has won, the board is a very high score
                | checkWon (target b) b == Just c = 50000000000
                -- if the other player has won, the board is a very low score
                | checkWon (target b) b == Just (other c) = -50000000000
                -- Level 2 - check if the player has more number of rows with both sides open of length (target - 1)
                | (getNumOfOpensBothSides c b (pieces b) (pieces b) ((target b) - 1) 0) -
                  (getNumOfOpensBothSides (other c) b (pieces b) (pieces b) ((target b) - 1) 0) > 0
                  && (difficulty b) == 2 = 100000
                | (getNumOfOpensBothSides c b (pieces b) (pieces b) ((target b) - 1) 0) -
                  (getNumOfOpensBothSides (other c) b (pieces b) (pieces b) ((target b) - 1) 0) < 0
                  && (difficulty b) == 2 = -100000
                -- Level 3 - check how many open rows (either side) a player has of length (target - 1)
                | (evaluateNumOfOpens c b ((target b) - 1) 0) -
                  (evaluateNumOfOpens (other c) b ((target b) - 1) 0) > 0 = 10000
                | (evaluateNumOfOpens c b ((target b) - 1) 0) -
                  (evaluateNumOfOpens (other c) b ((target b) - 1) 0) < 0 = -10000
                -- Level 4 - check if the player has more number of rows with both sides open of length (target - 2)
                | (getNumOfOpensBothSides c b (pieces b) (pieces b) ((target b) - 2) 0) -
                  (getNumOfOpensBothSides (other c) b (pieces b) (pieces b) ((target b) - 2) 0) > 0
                  && (difficulty b) == 2 = 1000
                | (getNumOfOpensBothSides c b (pieces b) (pieces b) ((target b) - 2) 0) -
                  (getNumOfOpensBothSides (other c) b (pieces b) (pieces b) ((target b) - 2) 0) < 0
                  && (difficulty b) == 2 = -1000
                -- Level 3 - check how many open rows (either side) a player has of length (target - 2)
                | (evaluateNumOfOpens c b ((target b) - 2) 0) -
                  (evaluateNumOfOpens (other c) b ((target b) - 1) 0) > 0 = 100
                | (evaluateNumOfOpens c b ((target b) - 2) 0) -
                  (evaluateNumOfOpens (other c) b ((target b) - 1) 0) < 0 = -100
                | otherwise = 0
                -- otherwise, evaluate the score by number of open rows, the higher the pieces in each row
                -- the higher the score

{-| Save the current world to a file called Savefile.txt. -}
save :: World -> IO ()
save world = writeFile filepath (show world)
