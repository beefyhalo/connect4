{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Control.Monad (void)
import Data.Matrix (toLists)
import Game
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (grid)
import Types

main :: IO ()
main = startGUI config setupGui
  where
    config =
      defaultConfig
        { jsStatic = Just "static",
          jsCustomHTML = Just "index.html"
        }

setupGui :: Window -> UI ()
setupGui window = void mdo
  -- Message element and grid rendering
  message <- UI.div #. "message" # sink UI.text (statusMessage . status <$> bGameState)
  gridEl <- UI.div #. "grid" # sink items bRenderedGrid
  restartBtn <- UI.button #+ [string "Restart"]

  -- Reactive game state
  (eventMove, triggerMove) <- liftIO newEvent
  let bRenderedGrid =
        renderGrid triggerMove
          <$> fmap (winningLine . status) bGameState
          <*> fmap grid bGameState
  bGameState <-
    accumB initialState $
      concatenate
        <$> unions
          [ applyMove <$> eventMove,
            const initialState <$ UI.click restartBtn
          ]

  getBody window # set UI.children [message, gridEl, restartBtn]

-- Status message based on game status
statusMessage :: GameStatus -> String
statusMessage (InProgress player) = "Player " ++ show player ++ "'s turn."
statusMessage (Victory player _) = "Player " ++ show player ++ " wins!"
statusMessage Draw = "It's a draw!"

-- Render a cell in the grid
createCell :: (Column -> IO ()) -> Column -> Maybe Player -> UI Element
createCell triggerMove col player = do
  cell <- UI.div #. ("cell " ++ playerClass player)
  on UI.click cell \_ -> liftIO $ triggerMove col
  pure cell

playerClass :: Maybe Player -> String
playerClass (Just Red) = "red"
playerClass (Just Yellow) = "yellow"
playerClass Nothing = "empty"

-- Render the entire grid
renderGrid :: (Column -> IO ()) -> Maybe WinningLine -> Grid -> [UI Element]
renderGrid triggerMove mWinningLine grid = [renderRow row r | (row, r) <- zip (toLists grid) [1 ..]]
  where
    renderRow :: [Maybe Player] -> Row -> UI Element
    renderRow rowData r =
      UI.div
        #. "row"
        #+ [createCell triggerMove (r, c) player | (player, c) <- zip rowData [1 ..]]

    -- Create a cell, adding a highlight class if it's in the winning line
    createCell :: (Column -> IO ()) -> Position -> Maybe Player -> UI Element
    createCell triggerMove (r, c) player = do
      let baseClass = "cell " ++ playerClass player
          highlightClass = if isWinningCell mWinningLine then " highlight" else ""
          -- Helper to check if a cell is in the winning line
          isWinningCell :: Maybe WinningLine -> Bool
          isWinningCell (Just line) = (r, c) `elem` line
          isWinningCell Nothing = False
      cell <- UI.div #. (baseClass ++ highlightClass)
      on UI.click cell \_ -> liftIO $ triggerMove c
      pure cell

items :: WriteAttr Element [UI Element]
items = mkWriteAttr $ \i el -> void do element el # set children [] #+ i
