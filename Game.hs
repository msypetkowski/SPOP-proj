module Game where

import qualified SDL
import Draw (drawGame)
import Rules (Player(Sheep, Wolf), initial_state, GameState(GameState, currentPlayer), nextStates, humanPlaying, opposite)
import DrawState(sparse_to_draw_state)


data Winner = Human | AI | None deriving (Show, Eq)

-- TODO: implement AI or player movement
-- now, there are only first possible move choices
get_next_state :: GameState -> IO (Maybe GameState)
get_next_state state@(GameState board player)
    | humanPlaying == player = get_human_move state
    | otherwise              = get_ai_move state


get_ai_move state = do
    return (Just $ head (nextStates state))


get_human_move state = do
    putStrLn "Human"
    SDL.delay 1234
    let next_states = nextStates state
    if null next_states then
        return Nothing
    else
        return $ Just $ last next_states


start :: SDL.Renderer -> IO ()
start renderer = do
    winner <- main_loop renderer (initial_state Sheep)
    putStrLn ("Winner is " ++ (show winner))


main_loop :: SDL.Renderer -> GameState -> IO Winner
main_loop renderer current_state = do
    next_state <- get_next_state current_state
    evaluate_state renderer current_state next_state


evaluate_state :: SDL.Renderer -> GameState -> Maybe GameState -> IO Winner
evaluate_state renderer old_state (Just new_state) = do
    events <- SDL.pollEvents
    drawGame renderer (sparse_to_draw_state new_state)
    if not (find_quit_event events) then
        main_loop renderer new_state
    else
        return None

evaluate_state renderer old_state Nothing =
    return $ toWinner $ opposite $ currentPlayer old_state


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
