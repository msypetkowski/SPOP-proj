import Graphics
import Game

main = Graphics.with_SDL
     $ Graphics.with_window "Kappa" (800, 640)
     $ Graphics.with_renderer
     $ Game.main_loop
