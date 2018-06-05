module DrawState where
import qualified Rules
import Rules (Player(Sheep, Wolf))
import qualified Data.Map as Map

data Pawn = None | Black | White deriving (Show)


data DrawGameState = DrawGameState {
    board :: [[Pawn]],
    current_player :: Rules.Player
} deriving (Show)

sparse_to_draw_state :: Rules.GameState -> DrawGameState
sparse_to_draw_state b = DrawGameState {
        board = (sparse_to_draw_state' (Rules.board b) (0,0) []),
        current_player = Wolf
}

sparse_to_draw_state' :: Map.Map Rules.Position Rules.Player -> Rules.Position -> [[Pawn]] -> [[Pawn]]
sparse_to_draw_state' m (x, y) acc = case x <= Rules.boardMaxIndex of
                                        True -> sparse_to_draw_state' m (x + 1, y) (row:acc)
                                            where
                                                row = (sparse_to_draw_state'' m (x, y) [])
                                        False -> acc

sparse_to_draw_state'' :: Map.Map Rules.Position Rules.Player -> (Int, Int) -> [Pawn] -> [Pawn]
sparse_to_draw_state'' m (x, y) acc = case y <= Rules.boardMaxIndex of
                                        True -> sparse_to_draw_state'' m (x, y+1) (pawn:acc)
                                            where
                                                pawn = case (Map.lookup (x, y) m ) of
                                                    Just Wolf -> Black
                                                    Just Sheep -> White
                                                    Nothing -> None
                                        False -> acc
