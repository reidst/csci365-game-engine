module Main where

import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Game (mkLevel, World, Player, Level, levelStart)

main :: IO ()
main = do
    vty <- mkVty V.defaultConfig
    level0 <- mkLevel 1
    let world0 = World (Player (levelStart level0)) level0
    (_finalWorld, ()) <- execRWST play vty world0
    V.shutdown vty
