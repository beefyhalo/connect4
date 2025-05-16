module Types (module Types) where

import Data.Matrix (Matrix, matrix, toLists)
import Data.Maybe (isJust)

data Player = Red | Yellow deriving (Eq, Show)

-- Toggle between players
nextPlayer :: Player -> Player
nextPlayer Red = Yellow
nextPlayer Yellow = Red

type Row = Int

type Column = Int

type Position = (Row, Column)

type Grid = Matrix (Maybe Player)

-- Game state data type
data GameState = GameState
  { grid :: Grid,
    status :: GameStatus
  }
  deriving (Show)

data GameStatus
  = InProgress Player -- The current player whose turn it is
  | Victory Player -- The player who won
  | Draw -- The game ended in a draw
  deriving (Eq, Show)

-- Initial game state
initialState :: GameState
initialState = GameState {grid = emptyGrid, status = InProgress Red}
  where
    -- Initialize an empty grid
    emptyGrid :: Grid
    emptyGrid = matrix 6 7 (const Nothing)

-- Check if a grid is full (draw)
isFull :: Grid -> Bool
isFull = all (all isJust) . toLists
