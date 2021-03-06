module Input where

import qualified SDL
import Rules(GameState(GameState), Player(Wolf, Sheep), Position, wolfMove, new_sheep_positions, get_next_states, opposite)
import qualified Data.Map as Map
import Draw(highlightField, highlightFields, drawGame, fieldSize)
import DrawState(sparse_to_draw_state)
import Dialog(get_file_name_dialog_save, get_file_name_dialog_load)

import Text.Read(readMaybe)

get_human_move :: SDL.Renderer -> GameState -> IO (Maybe GameState)
get_human_move renderer state = do
    if null $ get_next_states state then
        return Nothing
    else do
        handle_event renderer state first_mouse_press


handle_event :: SDL.Renderer -> GameState -> (SDL.Renderer -> Position -> GameState -> IO (Maybe GameState)) -> IO (Maybe GameState)
handle_event renderer state handler = do
    event <- SDL.pollEvent
    case event of
        (Just (SDL.Event _ (SDL.KeyboardEvent event_data))) -> do
            new_state <- handle_keyboard_event event_data state
            Draw.drawGame renderer (sparse_to_draw_state state)
            case new_state of
                Nothing -> handle_event renderer state handler
                Just state -> return (Just state)
        (Just (SDL.Event _ (SDL.MouseButtonEvent event_data@(SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonLeft _ coords)))) -> do
            handler renderer clicked_tile state
            where
                clicked_tile = get_index_from_coords coords
        (Just (SDL.Event _ (SDL.QuitEvent))) -> return Nothing
        _                                    -> handle_event renderer state handler

load_state :: FilePath -> IO (Maybe GameState)
load_state f = do s <- readFile f
                  return (readMaybe s)

handle_keyboard_event :: SDL.KeyboardEventData -> GameState -> IO (Maybe GameState)
handle_keyboard_event event_data@(SDL.KeyboardEventData _ SDL.Pressed _ keysym) game_state = do
    case SDL.keysymKeycode keysym of
        SDL.KeycodeS -> do
            file_path <- get_file_name_dialog_save
            case file_path of
                Just path -> do
                    print ("Saving game state to: " ++ path)
                    writeFile path (show game_state)
                    return Nothing
                Nothing -> return Nothing
        SDL.KeycodeL -> do
            file_path <- get_file_name_dialog_load
            case file_path of
                Just path -> do
                    print ("Loading game state from: " ++ path)
                    loaded_state <- load_state path
                    case loaded_state of
                        Just state -> return loaded_state
                        _ -> do
                            print "Failed to load."
                            return $ Just game_state
                Nothing -> return Nothing
        _ -> return Nothing
handle_keyboard_event _ _ = do
    return Nothing

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
