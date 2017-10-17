module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Environment
import Data.Generics
import Debug.Trace

import Board
import Draw
import Input
import AI

-- 'play' starts up a graphics window and sets up handlers for dealing
-- with inputs and updating the world state.
--
-- 'drawWorld' converts the world state into a gloss Picture
--
-- 'handleInput' is called whenever there is an input event, and if it is
-- a human player's turn should update the board with the move indicated by
-- the event
--
-- 'updateWorld' is called 10 times per second (that's the "10" parameter)
-- and, if it is an AI's turn, should update the board with an AI generated
-- move
main :: IO ()
main = do
        x <- getArgs
        -- should be 3 arguments
        case length x of
             3         -> do
                             -- assign the arguments
                             let size = read (x!!0) :: Int
                             let target = read (x!!1) :: Int
                             let variant = read (x!!2) :: Int
                             case variant of
                                  1 -> do
                                          putStrLn "AI? (Black always starts)\n1. White - Easy\n2. White - Medium\n3. White - Hard\n4. Black - Easy\n5. Black - Medium\n6. Black - Hard\n7. No AI"
                                          option <- getLine
                                          let choice = case option of
                                                            "1"       -> 1
                                                            "2"       -> 2
                                                            "3"       -> 3
                                                            "4"       -> 4
                                                            "5"       -> 5
                                                            "6"       -> 6
                                                            -- default: No AI
                                                            otherwise -> 7
                                          startGame size target variant choice
                                  otherwise -> startGame size target variant 1
             otherwise -> do
                            -- wrong num of arguments
                             putStrLn "usage: ./gomoku, followed by size of board, target for the game, 1/2 (1 for Gomoku, 2 for Renju, 3 for Pente)\nexample: ./gomoku 6 3 1"

{-| Create a world according to the user's options. First int is whether we should load the world, the 2nd is
the loaded world, the 3rd is the size of the board, the 4th is the target, the 5th is whether the AI should play,
the 6th is who starts. -}
createWorld :: Int -> World -> Int -> Int -> Int -> Int -> World
createWorld 1 w size target variant choice  = w
-- AI is White - Easy
createWorld noLoad w size target 1 1 =
                        World (Board size target [] 0 False False False 0 0) Black True White False
-- AI is White - Medium
createWorld noLoad w size target 1 2 =
                        World (Board size target [] 1 False False False 0 0) Black True White False
-- AI is White - Hard
createWorld noLoad w size target 1 3 =
                        World (Board size target [] 2 False False False 0 0) Black True White False
-- AI is Black - Easy
createWorld noLoad w size target 1 4 =
                        World (Board size target [] 0 False False False 0 0) Black True Black False
-- AI is Black - Medium
createWorld noLoad w size target 1 5 =
                        World (Board size target [] 1 False False False 0 0) Black True Black False
-- AI is Black - Hard
createWorld noLoad w size target 1 6 =
                        World (Board size target [] 2 False False False 0 0) Black True Black False
-- AI is not playing
createWorld noLoad w size target 1 7 =
                        World (Board size target [] 0 False False False 0 0) Black False Black False
-- Renju
createWorld noLoad w size target 2 b =
                        World (Board size target [] 0 True True False 0 0) Black False White False
--Pente
createWorld noLoad w size target a b =
                        World (Board size target [] 0 False False True 0 0) Black False White False

{-| Start a game using playIO with the given options -}
startGame :: Int -> Int -> Int -> Int -> IO ()
startGame size target variant choice = do
                     putStrLn "Load game?\n1. Yes\n2. No"
                     loadOption <- getLine
                     let loadChoice = case loadOption of
                                           "1"       -> 1
                                           -- default: Don't load
                                           otherwise -> 2
                     contents <- readFile filepath
                     background <- loadBMP "board.bmp"
                     blackpiece <- loadBMP "black.bmp"
                     whitepiece <- loadBMP "white.bmp"
                     let pictures = [background, blackpiece, whitepiece]
                     let world = read contents
                     -- start the game, using createWorld to create the right world
                     -- according to options
                     playIO (InWindow "Gomoku" (2000, 1000) (0, 0)) blue 10
                                               (createWorld loadChoice world size target
                                               variant choice)
                                               (drawWorld pictures)-- in Draw.hs
                                               handleInput -- in Input.hs
                                               updateWorld -- in AI.hs