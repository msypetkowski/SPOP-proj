module Game where

import qualified SDL
import Draw (drawGame)
import qualified Data.Map as Map
import Rules (Player(Sheep, Wolf), initial_state, GameState(GameState, currentPlayer), get_next_states, humanPlaying, opposite, startingPlayer, get_wolf_pos, Position)
import DrawState(sparse_to_draw_state)
import Input(get_human_move)


data Winner = Human | AI | None deriving (Show, Eq)

minimaxDepth = 5

start :: SDL.Renderer -> IO ()
start renderer = do
    let state = initial_state $ startingPlayer
    drawGame renderer (sparse_to_draw_state state)
    winner <- main_loop renderer state
    putStrLn ("Winner is " ++ (show winner))


main_loop :: SDL.Renderer -> GameState -> IO Winner
main_loop renderer current_state = do
    events <- SDL.pollEvents
    if find_quit_event events then
        return None
    else do
        next_state <- get_next_state renderer current_state
        case next_state of
            (Just state) -> do
                drawGame renderer (sparse_to_draw_state state)
                main_loop renderer state
            Nothing      -> return $ toWinner $ opposite $ currentPlayer current_state

get_next_state :: SDL.Renderer -> GameState -> IO (Maybe GameState)
get_next_state renderer state@(GameState _ player)
    | humanPlaying == player = get_human_move renderer state
    | otherwise              = return $ get_ai_move state


get_ai_move state = minimax state

minimax state@(GameState board currentPlayer)
    | currentPlayer == opposite humanPlaying = Just (head $ filter (\state -> evaluate state == maxStateValue) (get_next_states state))
    | currentPlayer == humanPlaying = Just (head $ filter (\state -> evaluate state == minStateValue) (get_next_states state))
        where maxStateValue = maximum $ map evaluate (get_next_states state)
              minStateValue = minimum $ map evaluate (get_next_states state)

evaluate :: GameState -> Int
evaluate state@(GameState board player)
    | player == Wolf = evaluate_wolf_heuristic board
    | player == Sheep = negate $ evaluate_wolf_heuristic board

evaluate_wolf_heuristic board = get_y $ get_wolf_pos (Map.toList board)

get_y :: Position -> Int
get_y (x, y) = y

find_quit_event :: [SDL.Event] -> Bool
find_quit_event (event:rest)
    | is_quit event = True
    | otherwise     = find_quit_event rest

find_quit_event [] = False


is_quit :: SDL.Event -> Bool
is_quit (SDL.Event _ SDL.QuitEvent) = True
is_quit _                           = False


toWinner :: Player -> Winner
toWinner player | player == humanPlaying = Human
                | otherwise              = AI
