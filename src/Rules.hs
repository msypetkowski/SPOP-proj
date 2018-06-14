module Rules where

import qualified Data.Map as Map

boardMaxIndex = 7
humanPlaying = Sheep
aiPlaying = opposite humanPlaying
startingPlayer = Wolf

type Position = (Int, Int)

data Player = Sheep | Wolf deriving (Read, Show, Eq)

opposite Sheep = Wolf
opposite Wolf = Sheep

data GameState = GameState {
    board :: Map.Map Position Player,
    currentPlayer :: Player
} deriving (Read, Show)


get_next_states :: GameState -> [GameState]
get_next_states (GameState board Wolf) = [(GameState (Map.insert key Wolf (Map.delete wp board)) Sheep) | key <- (wolfMove wp board)]
                            where wp = get_wolf_pos (Map.toList board)
get_next_states (GameState map Sheep) = concat [(get_all_sheep_moves sheep_pos map) | sheep_pos <- sheep_positions map]


get_wolf_pos ((pos, Wolf):xs) = pos
get_wolf_pos ((pos, Sheep):xs) = get_wolf_pos xs


wolfMove :: Position -> Map.Map Position Player -> [Position]
wolfMove (x,y) pawnsMap = [(x+i, y+j) | i <- [negate 1, 1], j <- [negate 1, 1], is_move_valid (x+i, y+j) pawnsMap]


is_move_valid :: Position -> Map.Map Position Player -> Bool
is_move_valid (x,y) pawnsMap = x >= 0 && y >= 0 && x <= boardMaxIndex && y <= boardMaxIndex && not (is_field_taken (x,y) pawnsMap)


is_field_taken :: Position -> Map.Map Position Player -> Bool
is_field_taken position pawns_map = (Map.lookup position pawns_map) /= Nothing


sheep_positions :: Map.Map Position Player -> [Position]
sheep_positions map = [position | (position, pawn) <- Map.toList map, pawn == Sheep]


get_all_sheep_moves :: Position -> Map.Map Position Player -> [GameState]
get_all_sheep_moves sheep map =
    [(GameState (Map.insert position Sheep (Map.delete sheep map)) Wolf)
     | position <- (new_sheep_positions sheep map)]


new_sheep_positions :: Position -> Map.Map Position Player -> [Position]
new_sheep_positions (x, y) map = [(x + i, y + j) | i <- [-1, 1], j <- [-1], is_move_valid (x + i, y + j) map]


--see https://pl.wikipedia.org/wiki/Wilk_i_owce#/media/File:Foxandhounds.png
--TODO: Wilk moze zaczynac z dowolnego czarnego pola w pierwszym rzedzie
initial_state :: Player -> GameState
initial_state starting = GameState {
    board = (Map.fromList (((0,0), Wolf):[((x, boardMaxIndex), Sheep) | x <- [1,3 .. boardMaxIndex]])),
    currentPlayer = starting
}
