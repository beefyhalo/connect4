{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Game (GameStatus, initialState, applyMove) where

import Data.Foldable (find)
import Data.Matrix (Matrix (..), getElem, safeGet, setElem)
import Data.Maybe (isNothing)
import Types

-- Apply a move, return new game state
applyMove :: Column -> GameState -> GameState
applyMove col state =
  case status state of
    Victory _ -> state -- Do nothing if the game is already over
    Draw -> state -- Do nothing if it's a draw
    InProgress player ->
      case dropPiece player col state.grid of
        Just (grid, pos) ->
          let won = checkVictory player pos grid
              draw = isFull grid
           in if
                | won -> state {grid, status = Victory player}
                | draw -> state {grid, status = Draw}
                | otherwise -> state {grid, status = InProgress (nextPlayer player)}
        Nothing -> state -- Invalid move (e.g., full column)

-- Drop a piece in the chosen column
dropPiece :: Player -> Column -> Grid -> Maybe (Grid, Position)
dropPiece player col grid =
  case find (isNothing . snd) rows of
    Just (row, _) -> let pos = (row, col) in Just (setElem (Just player) pos grid, pos)
    Nothing -> Nothing
  where
    rows = [(row, getElem row col grid) | row <- [nrows grid, nrows grid - 1 .. 1]]

-- Check if the given player has a line of 4
checkVictory :: Player -> Position -> Grid -> Bool
checkVictory player (row, col) grid =
  let directions =
        [ (1, 0), -- Vertical
          (0, 1), -- Horizontal
          (1, 1), -- Diagonal /
          (1, -1) -- Diagonal \
        ]
      checkDirection (dr, dc) =
        let line = [safeGet (row + i * dr) (col + i * dc) grid | i <- [0 .. 3]]
         in all (== Just (Just player)) line
   in any checkDirection directions