module Graphics where

import qualified SDL

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Text              (pack)
import Foreign.C.Types        (CInt)

import SDL


data Color = FieldDark | FieldBright | PawnWhite | PawnBlack | FieldHighlight | PawnHighlight | Background deriving (Show)


with_SDL :: (MonadIO m) => m a -> m ()
with_SDL action = do
    SDL.initializeAll
    void action
    SDL.quit


with_window :: (MonadIO m) => String -> (CInt, CInt) -> (SDL.Window -> m a) -> m ()
with_window title (size_x, size_y) action = do
    window <- SDL.createWindow (Data.Text.pack title) window_config
    SDL.showWindow window
    void $ action window
    SDL.destroyWindow window

    where
        window_config = SDL.defaultWindow { SDL.windowInitialSize = window_size }
        window_size = SDL.V2 size_x size_y


with_renderer :: (MonadIO m) => (SDL.Renderer -> m a) -> SDL.Window -> m ()
with_renderer action window = do
    renderer <- SDL.createRenderer window (-1) renderer_config
    void $ action renderer
    SDL.destroyRenderer renderer
    where
        renderer_config = SDL.RendererConfig {
            SDL.rendererType = SDL.AcceleratedVSyncRenderer,
            SDL.rendererTargetTexture = False
        }


set_color :: (MonadIO m) => SDL.Renderer -> Color -> m ()
set_color r FieldDark      = SDL.rendererDrawColor r $= V4 160 82  45  255
set_color r FieldBright    = SDL.rendererDrawColor r $= V4 240 200 100 255
set_color r PawnBlack      = SDL.rendererDrawColor r $= V4 10  10  10  255
set_color r PawnWhite      = SDL.rendererDrawColor r $= V4 255 255 254 255
set_color r FieldHighlight = SDL.rendererDrawColor r $= V4 0   0   255 255
set_color r PawnHighlight  = SDL.rendererDrawColor r $= V4 255 0   0   255
set_color r Background     = SDL.rendererDrawColor r $= V4 0   0   0   255


clear_screen :: (MonadIO m) => SDL.Renderer -> m ()
clear_screen renderer = do
    set_color renderer Background
    SDL.clear renderer
