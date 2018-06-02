module DrawState where
import Rules
import qualified Data.Map as Map

data Pawn = None | Black | White deriving (Show)


data DrawGameState = DrawGameState {
    board :: [[Pawn]],
    current_player :: Player
} deriving (Show)

sparse_to_draw_state :: GameState -> DrawGameState
sparse_to_draw_state b = DrawGameState {
        board=(sparse_to_draw_state' (board1 b) (0,0) []),
        current_player=Wolf
}

sparse_to_draw_state' :: Map.Map (Int, Int) Bool -> (Int, Int) -> [[Pawn]] -> [[Pawn]]
sparse_to_draw_state' m (x, y) acc = case x <= boardMaxIndex of
                                        True -> sparse_to_draw_state' m (x + 1, y) (row:acc)
                                            where 
                                                row = (sparse_to_draw_state'' m (x, y) [])
                                        False -> acc

sparse_to_draw_state'' :: Map.Map (Int, Int) Bool -> (Int, Int) -> [Pawn] -> [Pawn]
sparse_to_draw_state'' m (x, y) acc = case y <= boardMaxIndex of 
                                        True -> sparse_to_draw_state'' m (x, y+1) (pawn:acc)
                                            where
                                                pawn = case (Map.lookup (x, y) m ) of
                                                    Just True -> Black
                                                    Just False -> White
                                                    Nothing -> None
                                        False -> acc
