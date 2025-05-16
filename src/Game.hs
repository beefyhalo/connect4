{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Game (GameStatus, initialState, applyMove) where

import Data.Foldable (find)
import Data.Matrix (Matrix (..), getElem, safeGet, setElem)
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Types

-- Apply a move, return new game state
applyMove :: Column -> GameState -> GameState
applyMove col state =
  case status state of
    Victory _ _ -> state -- Do nothing if the game is already over
    Draw -> state -- Do nothing if it's a draw
    InProgress player ->
      case dropPiece player col state.grid of
        Just (grid, pos) ->
          if
            | Just line <- checkVictory player pos grid -> state {grid, status = Victory player line}
            | isFull grid -> state {grid, status = Draw}
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

-- Check if the given player has a line of 4, return the winning line if found
checkVictory :: Player -> Position -> Grid -> Maybe WinningLine
checkVictory player (row, col) grid =
  listToMaybe (mapMaybe checkDir dirs)
  where
    dirs = [(1, 0), (0, 1), (1, 1), (1, -1)]
    collect (dr, dc) sign =
      takeWhile
        (\(r, c) -> safeGet r c grid == Just (Just player))
        [(row + i * sign * dr, col + i * sign * dc) | i <- [1 .. 3]]
    checkDir dir =
      let neg = collect dir (-1)
          pos = collect dir 1
          line = reverse neg ++ [(row, col)] ++ pos
       in if length line >= 4 then Just line else Nothing
