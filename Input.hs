module Input where

import qualified SDL
import Rules(GameState(GameState), Player(Wolf, Sheep), Position, wolfMove, new_sheep_positions, nextStates, opposite)
import qualified Data.Map as Map
import Draw(highlightField, highlightFields, drawGame, fieldSize)
import DrawState(sparse_to_draw_state)

get_human_move :: SDL.Renderer -> GameState -> IO (Maybe GameState)
get_human_move renderer state = do
    if null $ nextStates state then
        return Nothing
    else do
        handle_event renderer state first_mouse_press


handle_event :: SDL.Renderer -> GameState -> (SDL.Renderer -> Position -> GameState -> IO (Maybe GameState)) -> IO (Maybe GameState)
handle_event renderer state handler = do
    event <- SDL.pollEvent
    case event of
        (Just (SDL.Event _ (SDL.MouseButtonEvent event_data@(SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonLeft _ coords)))) -> do
            handler renderer clicked_tile state
            where
                clicked_tile = get_index_from_coords coords
        (Just (SDL.Event _ (SDL.QuitEvent))) -> return Nothing
        (Just _)                             -> handle_event renderer state handler
        Nothing                              -> handle_event renderer state handler


get_index_from_coords (SDL.P (SDL.V2 x y)) = (floor $ (fromIntegral x) / (fromIntegral fieldSize),
                                              floor $ (fromIntegral y) / (fromIntegral fieldSize))


first_mouse_press :: SDL.Renderer -> Position -> GameState -> IO (Maybe GameState)
first_mouse_press renderer clicked_tile state@(GameState board player) = do
    if (Map.lookup clicked_tile board) /= Just player then
        handle_event renderer state first_mouse_press
    else do
        Draw.highlightField renderer clicked_tile
        Draw.highlightFields renderer possible_moves
        handle_event renderer state (second_mouse_press possible_moves clicked_tile)
    where
        possible_moves = case player of
            Wolf -> wolfMove clicked_tile board
            Sheep -> new_sheep_positions clicked_tile board


second_mouse_press :: [(Int, Int)] -> (Int, Int) -> SDL.Renderer -> Position-> GameState -> IO (Maybe GameState)
second_mouse_press moves pawn renderer clicked_tile state@(GameState board player) = do
    if clicked_tile `elem` moves then
        return $ Just $ (GameState (Map.insert clicked_tile player (Map.delete pawn board)) (opposite player))
    else do
        Draw.drawGame renderer (sparse_to_draw_state state)
        handle_event renderer state first_mouse_press
