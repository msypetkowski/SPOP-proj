module GameState where


data Pawn = None | Black | White deriving (Show)
data Player = Sheep | Wolf deriving (Show)


data GameState = GameState {
    board :: [[Pawn]],
    current_player :: Player
} deriving (Show)


initial_state :: Player -> GameState
initial_state starting = GameState {
    board = initial_board,
    current_player = starting
}

initial_board :: [[Pawn]]
initial_board =
    [[None , White, None , White, None , White, None , White],
     [None , None , None , None , None , None , None , None ],
     [None , None , None , None , None , None , None , None ],
     [None , None , None , None , None , None , None , None ],
     [None , None , None , None , None , None , None , None ],
     [None , None , None , None , None , None , None , None ],
     [None , None , None , None , None , None , None , None ],
     [Black, None , None , None , None , None , None , None ]]
