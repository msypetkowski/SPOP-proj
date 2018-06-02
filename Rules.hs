module Rules where

import qualified Data.Map as Map

boardMaxIndex = 7

data Player = Sheep | Wolf deriving (Show)


data GameState = GameState {
    board1 :: Map.Map (Int, Int) Bool,
    currentPlayer1 :: Player
} deriving (Show)


getWolfPos ((pos, True):xs) = pos
getWolfPos ((pos, False):xs) = getWolfPos xs

sheepCollides :: (Int, Int) -> Map.Map (Int, Int) Bool -> Bool
sheepCollides wolfPos pawnsMap = not ((Map.lookup wolfPos pawnsMap) == Nothing)
checkWolfMove :: (Int, Int) -> Map.Map (Int, Int) Bool -> Bool
checkWolfMove (x,y) pawnsMap = x >= 0 && y >= 0 && x <= boardMaxIndex && y <= boardMaxIndex && not (sheepCollides (x,y) pawnsMap)

wolfMove :: (Int, Int) -> Map.Map (Int, Int) Bool -> [(Int, Int)]
wolfMove (x,y) pawnsMap = [(x+i, y+j) | i <- [negate 1, 1], j <- [negate 1, 1], checkWolfMove (x+i, y+j) pawnsMap]


nextStates :: GameState -> [GameState]
nextStates (GameState m Wolf) = [(GameState (Map.insert key True (Map.delete wp m)) Sheep) | key <- (wolfMove wp m)]
                            where wp = getWolfPos (Map.toList m)

-- TODO: Implement sheep movement. Now, only wolf moves.
-- nextStates (GameState m Sheep) = error "Sheep movement not Implemented"
nextStates (GameState m Sheep) = (nextStates (GameState m Wolf))


--see https://pl.wikipedia.org/wiki/Wilk_i_owce#/media/File:Foxandhounds.png
initial_state :: Player -> GameState
initial_state starting = GameState {
    board1 = (Map.fromList (((0,0), True):[((x, boardMaxIndex), False) | x <- [1,3 .. boardMaxIndex]])),
    currentPlayer1 = Wolf
}
