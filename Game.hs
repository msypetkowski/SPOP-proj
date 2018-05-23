module Game where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Extra    (whileM)


import qualified SDL
import Draw (drawGame)

import GameState (initial_state, Player(Sheep, Wolf))


main_loop :: SDL.Renderer -> IO ()
main_loop renderer = do
    whileM $
        fmap should_run SDL.pollEvent
        >>= conditional_run (drawGame renderer (initial_state Sheep))


should_run :: Maybe SDL.Event -> Bool
should_run = maybe True (not . is_quit)


is_quit :: SDL.Event -> Bool
is_quit (SDL.Event _ SDL.QuitEvent) = True
is_quit _                           = False


conditional_run :: (Monad m) => m a -> Bool -> m Bool
conditional_run f True  = True <$ f
conditional_run _ False = pure False
