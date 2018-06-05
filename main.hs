import Graphics
import Game

import Draw (fieldSize)

main = Graphics.with_SDL
     $ Graphics.with_window "Kappa" (fieldSize * 8, fieldSize * 8)
     $ Graphics.with_renderer
     $ Game.start
