module Draw(drawWorld) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Board
import AI
import System.Environment
import System.IO.Unsafe
import Codec.BMP

-- Given a world state, return a Picture which will render the world state.
drawWorld :: [Picture] -> World -> IO Picture
--drawWorld w = Color blue $ Circle 100
drawWorld pics w = return (pictures[drawBackground (pics !! 0) (size (board w)) (size(board w)),
                    drawHLines (blank) (size (board w)) (size (board w)),
                     drawVLines (blank) (size (board w)) (size (board w)),
                     drawPieces pics (blank) w (pieces (board w)),
                     displayWinMessage (board w),
                     displayHint w])

{-| Draws the appropriately sized and placed background image -}
drawBackground :: Picture -> Int -> Int -> Picture
drawBackground pic x y = (scale (fromIntegral (x)*0.05)(fromIntegral(y)*0.05) $ pic)

{-| Draws the horizontal lines of the grid -}
drawHLines :: Picture -> Int -> Int -> Picture
drawHLines pic x (-1) = pic
drawHLines pic x y = drawHLines (pictures[pic, line [((-(newX * 25)), ((newY * 50) - ((newX * 50) / 2))),
                                                    (newX * 25,((newY * 50) - ((newX * 50) / 2)))]])
                                                    (x) (y-1)
  where
    newY = (fromIntegral(y))
    newX = (fromIntegral(x))

{-| Draws the vertical lines of the grid -}
drawVLines :: Picture -> Int -> Int -> Picture
drawVLines pic (-1) y = pic
drawVLines pic x y = drawVLines (pictures[pic, line [((newX * 50) - ((newY * 50) / 2), newY * 25),
                                                    ((newX * 50) - ((newY * 50) / 2), -(newY * 25))]]) (x-1) (y)
  where
    newY = (fromIntegral(y))
    newX = (fromIntegral(x))

{-| Draws all of the pieces onto the board-}
drawPieces :: [Picture] -> Picture -> World -> [(Position, Col)] -> Picture
drawPieces pics pic w [] = pic
drawPieces pics pic w ((a, b):xs)
                              | b == Black = drawPieces pics (pictures[pic, translate ((50 * x) - (boardSize * 25))
                                                                   ((50 * y) - (boardSize * 25))
                                                                   (pics !! 1)]) w xs
                              | otherwise  = drawPieces pics (pictures[pic, translate ((50 * x) - (boardSize * 25))
                                                                   ((50 * y) - (boardSize * 25))
                                                                   (pics !! 2)]) w xs
    where
      x = fromIntegral(fst a)
      y = fromIntegral(snd a)
      bo = board w
      boardSize = fromIntegral(size bo)

{-| Disaplys the appropiate win message in case of a win, and draw message if no player can win-}
displayWinMessage :: Board -> Picture
displayWinMessage b = case checkWon (target b) b of
                           Just col -> pictures[color red (polygon [ (-300, -100), (350, -100), (350, 200), (-300, 200) ]),
                                       translate (-300) (0) $ text  (show col ++ " won!")]
                           Nothing  -> if (generateMoves b Black) == [(((size b) `div` 2), ((size b) `div` 2))]
                                           && (length (pieces b) > 0) then
                                               pictures[color red (polygon [ (-300, -100), (400, -100), (400, 200), (-300, 200) ]),
                                               translate (-300) (0) $ text ("It's a draw!")]
                                       else blank

{-| Displays a hint to the player as to what move to do next-}
displayHint :: World -> Picture
displayHint world
                    | (hint world) == True = pictures[color green (polygon [ (-150, -80), (130, -80), (130, 150), (-150, 150) ]), translate (-150) (0) $ text (show pos)]
                    | otherwise            = blank
  where
    pos = getBestMove world 0 (buildTree generateMoves world (other (aicol world)))
