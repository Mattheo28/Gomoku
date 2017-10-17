module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI
import Data.Maybe

import Debug.Trace

-- Update the world state given an input event.
handleInput :: Event -> World -> IO World
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) world
    = case makeMove world (turn world) (translateX x world, translateY y world) of
           Just b  -> return (world{board = b, turn = other (turn world)})
--                      if isJust (checkWon (target b) b) then world{board = b}
--                      else world{board = b, turn = other (turn world)}
           Nothing -> if isJust (checkWon (target (board world)) (board world)) ||
                         (generateMoves (board world) (turn world) == []) then return world
                      else trace ("Invalid move!") (return world)
-- Backspace - undo last move
handleInput (EventKey (Char '\b') Down _ _) world
    = return (undoMove world)
-- 'h' - show hint
handleInput (EventKey (Char 'h') Down _ _) world
    = return world{hint = True}
-- remove hint
handleInput (EventKey (Char 'h') Up _ _) world
    = return world{hint = False}
-- 's' - save game
handleInput (EventKey (Char 's') Down _ _) world
    = do
         save world
         trace ("Game saved!") (return world)
-- 'r' - restart game
handleInput (EventKey (Char 'r') Down _ _) world
    = return world{board = (board world){pieces = [], capturedPairsBlack = 0, capturedPairsWhite = 0},
                   turn = Black}
handleInput e b = return b


{-| Translates a gloss X coordinate to the corresponding grid point on the board-}
translateX :: Float -> World -> Int
translateX x w = round (x/50) + round (boardSize / 2)

    where
        b = board w
        boardSize = fromIntegral(size b)

{-| Translates a gloss Y coordinate to the corresponding grid point on the board-}
translateY :: Float -> World -> Int
translateY y w = round (y/50) + round (boardSize / 2)

    where
        b = board w
        boardSize = fromIntegral(size b)
