module AI where

import Board
import Data.List
import Data.Ord
import Data.Ix
import Debug.Trace

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }
  deriving Show

-- Given a function to generate plausible moves (i.e. board positions)
-- for a player (Col) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.
buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> World -- ^ board state
             -> Col -- ^ player to play next
             -> GameTree
buildTree gen w c = let moves = gen (board w) c in -- generated moves
                        GameTree (board w) c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove w c pos of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, buildTree gen (w{board = b'}) (other c)) : mkNextStates xs
                             -- successful, make move and build tree from 
                             -- here for opposite player

{-| Generate a list of possible moves sensibly by first trying to create a list of moves that have an
adjacent piece. If this list is empty, the board is either empty or full, in which case the list returned
contains 1 position - the middle of the board (we assume it's empty and this is the first position). The board full
case is handled elsewhere. -}
generateMoves :: Board -> Col -> [Position]
generateMoves board col
                         -- list of moves adjacent to pieces isn't empty, so return that
                        | length listOfAdjacent > 0 = listOfAdjacent
                         -- if it is empty, return only the position of the middle of the board
                        | otherwise = [(((size board) `div` 2), ((size board) `div` 2))]
  where
    -- list of all possible moves
    listOfPossible = filter (\pos -> pos `notElem` (map fst (pieces board))) (range ((0, 0), ((size board), (size board))))
    -- using above list, create list of all moves adjacent to pieces
    listOfAdjacent = filter (\pos -> checkIfAdj directions
                            pos (pieces board)) listOfPossible

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove :: World -- Initial world
               -> Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove w depth tree
             -- depth is 0, so simply get the next_moves, and get the index of the best next move by calling evaluate on
             -- the board of each game tree associated to each move, and finding the index of the best board using
             -- maximumBy
			| depth == 0 = (map fst (next_moves tree)) !! (snd (maximumBy (comparing fst) (zip (map (evaluate
			                                              (game_turn tree)) (map game_board (map snd (next_moves tree))))
			                                              [0..])))
			 -- call evaluateTree on all the trees within the given tree. If it returns -500000 for any tree, that
			 -- is a winning move, so make the move at the same index as the -500000. Otherwise, do the same as above
			 -- to get the next best move, but call evaluateTree with the given depth rather than just evaluate
			| even depth = case elemIndex (5000000000000) (map (evaluateTree w depth) (map snd (next_moves tree))) of
                                Just index -> (map fst (next_moves tree)) !! index
                                otherwise  -> (map fst (next_moves tree)) !! (snd (maximumBy (comparing fst)
                                                                             (zip (map (evaluateTree w depth)
                                                                             (map snd (next_moves tree))) [0..])))
			| otherwise  =  case elemIndex (-5000000000000) (map (evaluateTree w depth) (map snd (next_moves tree))) of
                                 Just index -> (map fst (next_moves tree)) !! index
                                 otherwise  -> (map fst (next_moves tree)) !! (snd (maximumBy (comparing fst)
                                                                              (zip (map (evaluateTree w depth)
                                                                              (map snd (next_moves tree))) [0..])))
  --where

{-| Evaluate a tree, based on the scores of all of its inner trees, which is found using a recursive call (depth times) -}
evaluateTree :: World -> Int -> GameTree -> Int
evaluateTree w depth tree
                       -- if depth is 1, this is the final call, so evaluate the current tree based on the sum of the
                       -- scores given by the evaluate function on the boards of its trees. If the turn is the other
                       -- player's in these trees, the sum should be taken away. After the sum is found, add the evaluation
                       -- of the board of the current tree, as that value shouldn't be ignored.
                       | depth == 1 && ((game_turn tree) == (turn w)) = sum (map (evaluate (game_turn tree))
                                                                        (map game_board (map snd (next_moves tree))))
                                                                        + ((evaluate (game_turn tree) (game_board tree))
                                                                        * 100)
			           | depth == 1 && ((game_turn tree) == (other (turn w))) = -(sum (map (evaluate (game_turn tree))
			                                                                     (map game_board (map snd (next_moves tree)
			                                                                     ))) + ((evaluate (game_turn tree)
			                                                                     (game_board tree)) * 100))
			           -- depth is greater than one, so find the sum of the recursive call on this function mapped
			           -- to every tree within the given tree, and add the evaluation of the current board. Multiply
			           -- this sum by depth * 10 to give more weight on game trees closer to the current one.
			           | otherwise =  (sum (map (evaluateTree w (depth - 1)) (map snd (next_moves tree))) *
			                          (depth * 10)) + ((evaluate (game_turn tree) (game_board tree)) * 100)

-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> IO World
updateWorld t w
           -- if it is the AI's turn and the AI is part of the game, call makeMove using getBestMove at a given
           -- depth, and return the world with the new board
		  | turn w == (aicol w) && (ai w) == True && (difficulty (board w) == 2) =
		              case makeMove w (aicol w) (getBestMove w 1
		                            (buildTree generateMoves w (aicol w))) of
		                   Just b  -> return (w{board = b, turn = other (turn w)})
		                   Nothing -> return w
		  | turn w == (aicol w) && (ai w) == True && (difficulty (board w) < 2) =
		              case makeMove w (aicol w) (getBestMove w (difficulty (board w))
		                            (buildTree generateMoves w (aicol w))) of
		                   Just b  -> return  (w{board = b, turn = other (turn w)})
		                   Nothing -> return w
		  | otherwise = return w

