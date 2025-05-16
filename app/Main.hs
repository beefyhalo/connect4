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
  gridEl <- UI.div #. "grid" # sink items (renderGrid triggerMove . grid <$> bGameState)
  restartBtn <- UI.button #+ [string "Restart"]

  -- Reactive game state
  (eventMove, triggerMove) <- liftIO newEvent
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
statusMessage (Victory player) = "Player " ++ show player ++ " wins!"
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
renderGrid :: (Column -> IO ()) -> Grid -> [UI Element]
renderGrid triggerMove grid = [renderRow row r | (row, r) <- zip (toLists grid) [1 ..]]
  where
    renderRow :: [Maybe Player] -> Row -> UI Element
    renderRow rowData _ =
      UI.div
        #. "row"
        #+ [createCell triggerMove c player | (player, c) <- zip rowData [1 ..]]

items :: WriteAttr Element [UI Element]
items = mkWriteAttr $ \i el -> void do element el # set children [] #+ i
