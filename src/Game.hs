module Game where

import qualified SDL
import Draw (drawGame)
import qualified Data.Map as Map
import Rules (Player(Sheep, Wolf), sheep_positions, initial_state, GameState(GameState, currentPlayer), get_next_states, humanPlaying, aiPlaying, opposite, startingPlayer, get_wolf_pos, Position)
import DrawState(sparse_to_draw_state)
import Input(get_human_move)


data Winner = Human | AI | None deriving (Show, Eq)

depthLimit = 7

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
        if is_terminal current_state then
            return winner
        else do
            next_state <- get_next_state renderer current_state
            case next_state of
                (Just state) -> do
                    drawGame renderer (sparse_to_draw_state state)
                    main_loop renderer state
                Nothing      -> return winner
    where
        winner = toWinner $ opposite $ currentPlayer current_state


get_next_state :: SDL.Renderer -> GameState -> IO (Maybe GameState)
get_next_state renderer state@(GameState _ player)
    | humanPlaying == player = get_human_move renderer state
    | otherwise              = return $ get_ai_move state


get_ai_move state =
    if null $ get_next_states state then
        Nothing
    else
        Just $ snd $ minimax depthLimit state

minimax :: Int -> GameState -> (Int, GameState)
minimax depth state@(GameState board currentPlayer)
    | is_terminal state = (1000+depth, state)
    | currentPlayer == aiPlaying && depth == depthLimit = head $ filter (\(value, s) -> value == maxValue) childrenMinmaxResults
    | currentPlayer == humanPlaying && depth == depthLimit = head $ filter (\(value, s) -> value == minValue) childrenMinmaxResults
    | currentPlayer == aiPlaying && depth > 0 = (maxValue, state)
    | currentPlayer == humanPlaying && depth > 0 = (minValue, state)
    | currentPlayer == aiPlaying && depth == 0 = (maxStateValue+depth, state)
    | currentPlayer == humanPlaying && depth == 0 = (minStateValue, state)
        where nextStates = get_next_states state
              maxStateValue = if length nextStates > 0 then maximum $ map evaluate nextStates else -1000
              minStateValue = if length nextStates > 0 then minimum $ map evaluate nextStates else -1000
              childrenMinmaxResults = map (minimax (depth-1)) nextStates
              maxValue = if length childrenMinmaxResults > 0 then maximum $ map fst childrenMinmaxResults else -1000
              minValue = if length childrenMinmaxResults > 0 then minimum $ map fst childrenMinmaxResults else -1000


is_terminal (GameState board currentPlayer) =
    (snd $ get_wolf_pos (Map.toList board)) == 7
    -- (maximum $ map snd (sheep_positions board))


evaluate :: GameState -> Int
evaluate state@(GameState board _) = evaluate_wolf_heuristic board

evaluate_wolf_heuristic board = (snd $ get_wolf_pos (Map.toList board)) * 100

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
