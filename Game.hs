module Game where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Extra    (whileM)


import qualified SDL
import Draw (drawGame)
import Rules (Player(Sheep, Wolf), initial_state, GameState, nextStates)
import DrawState(sparse_to_draw_state)

-- TODO: implement AI or player movement
-- now, there are only first possible move choices
get_next_state :: GameState -> GameState
get_next_state s = head (nextStates s)

main_loop :: SDL.Renderer -> IO ()
main_loop renderer = do
    let cur_state = (initial_state Wolf)
    whileM $
        -- TODO: update somehow cur_state (using get_next_state)
        fmap should_run SDL.pollEvent
        >>= conditional_run (drawGame renderer
                    (sparse_to_draw_state cur_state))


should_run :: Maybe SDL.Event -> Bool
should_run = maybe True (not . is_quit)


is_quit :: SDL.Event -> Bool
is_quit (SDL.Event _ SDL.QuitEvent) = True
is_quit _                           = False


conditional_run :: (Monad m) => m a -> Bool -> m Bool
conditional_run f True  = True <$ f
conditional_run _ False = pure False
