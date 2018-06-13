module Draw where

import DrawState
import qualified Graphics

import qualified SDL
import Foreign.C.Types        (CInt)

mkRect :: t -> t -> t -> t -> SDL.Rectangle t
mkRect x y w h = SDL.Rectangle o s
  where o = SDL.P (SDL.V2 x y)
        s = SDL.V2 w h


fieldSize :: CInt
fieldSize = 64


drawBoard :: SDL.Renderer -> [[Pawn]] -> IO ()
drawBoard r b = drawBoard' r b (0, 0)


drawBoard' :: SDL.Renderer -> [[Pawn]] -> (CInt, CInt) -> IO ()
drawBoard' _ [] _ = return ()
drawBoard' r (b:bs) (x, y) = do
    drawBoard'' r b (x, y)
    drawBoard' r bs (x, y + 1)


drawBoard'' :: SDL.Renderer -> [Pawn] -> (CInt, CInt) -> IO ()
drawBoard'' _ [] _ = return ()
drawBoard'' r (b:bs) (x, y) = do
    Graphics.set_color r color
    SDL.fillRect r (Just (mkRect (x*fieldSize) (y*fieldSize) fieldSize fieldSize))
    drawBoard'' r bs (x + 1, y)
    drawPawn r b (x, y)
    where
        color = case (mod (x + y) 2) of
                    0 -> Graphics.FieldDark
                    1 -> Graphics.FieldBright


drawPawn :: SDL.Renderer -> Pawn -> (CInt, CInt) -> IO()
drawPawn r None _ = return ()
drawPawn r pawn (x, y) = do
    Graphics.set_color r color
    SDL.fillRect r (Just (mkRect newx newy neww newh))
    where
        newx = x*fieldSize + (div fieldSize 4)
        newy = y*fieldSize + (div fieldSize 4)
        neww = (div fieldSize 2)
        newh = (div fieldSize 2)
        color = case pawn of
            Black -> Graphics.PawnBlack
            White -> Graphics.PawnWhite


drawGame :: SDL.Renderer -> DrawGameState -> IO ()
drawGame renderer (DrawGameState {board = b, current_player = c}) = do
    Graphics.clear_screen renderer
    -- SDL.present renderer
    Graphics.set_color renderer Graphics.FieldBright
    drawBoard renderer b
    SDL.present renderer


highlightFields :: SDL.Renderer -> [(Int, Int)] -> IO ()
highlightFields renderer [] = return ()
highlightFields renderer (field:fields) = do
    highlightField' renderer field Graphics.FieldHighlight
    highlightFields renderer fields


highlightField :: SDL.Renderer -> (Int, Int) -> IO ()
highlightField renderer (x, y) = do
    highlightField' renderer (x, y) Graphics.PawnHighlight


highlightField' renderer (x, y) color = do
    Graphics.set_color renderer color
    SDL.drawRect renderer (Just (mkRect ((fromIntegral x)*fieldSize) ((fromIntegral y)*fieldSize) fieldSize fieldSize))
    SDL.present renderer
