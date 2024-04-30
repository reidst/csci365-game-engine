module Main where

-- import qualified MyLib (someFunc)

import Graphics.Vty
import Graphics.Vty.CrossPlatform (mkVty)

main :: IO ()
main = do
    vty <- mkVty defaultConfig
    let line0 = string (defAttr `withForeColor` green) "first line"
        line1 = string (defAttr `withBackColor` blue) "second line"
        img = line0 <-> line1
        pic = picForImage img
    update vty pic
    e <- nextEvent vty
    shutdown vty
    print ("Last event was: " ++ show e)
