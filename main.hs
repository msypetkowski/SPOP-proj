{-# LANGUAGE FlexibleInstances #-}
-- TODO: consider using newtype instead of FlexibleInstances
import qualified Data.Map as Map

import Graphics
import Game

-- TODO: checkout constants defining/naming convention
boardMaxIndex = 7


class Show s => GameState s where
    nextStates :: s -> [s]
    currentPlayer :: s -> Bool


getWolfPos ((pos, True):xs) = pos
getWolfPos ((pos, False):xs) = getWolfPos xs

sheepCollides wolfPos pawnsMap = not ((Map.lookup wolfPos pawnsMap) == Nothing)
checkWolfMove (x,y) pawnsMap = x >= 0 && y >= 0 && x <= boardMaxIndex && y <= boardMaxIndex && not (sheepCollides (x,y) pawnsMap)

wolfMove :: (Int, Int) -> (Map.Map (Int, Int) Bool) -> [(Int, Int)]
wolfMove (x,y) pawnsMap = [(x+i, y+j) | i <- [negate 1, 1], j <- [negate 1, 1], checkWolfMove (x+i, y+j) pawnsMap]

-- First element of tuple is a Map.
-- Keys in map are pawns positions, value is whether a pawn is a wolf.
-- Second element of tuple is current player id.
instance GameState ((Map.Map (Int, Int) Bool), Bool) where
    -- TODO: check if there is a way to unpack/break Map - without conversion to list
    nextStates (m, True) = [((Map.insert key True (Map.delete wp m)), False) | key <- (wolfMove wp m)]
                                where wp = getWolfPos (Map.toList m)
    nextStates (m, False) = error "Sheep movement not Implemented"

    currentPlayer (_, curr) = curr

--see https://pl.wikipedia.org/wiki/Wilk_i_owce#/media/File:Foxandhounds.png
initialState :: ((Map.Map (Int, Int) Bool), Bool)
initialState = (Map.fromList (((0,0), True):[((x, boardMaxIndex), False) | x <- [1,3 .. boardMaxIndex]]), True)


main = Graphics.with_SDL
     $ Graphics.with_window "Kappa" (800, 640)
     $ Graphics.with_renderer
     $ Game.main_loop
