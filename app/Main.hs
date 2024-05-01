{-# LANGUAGE GADTSyntax #-}

module Main where

import Graphics.Vty
import Graphics.Vty.CrossPlatform (mkVty)

data GameState where
    Waiting :: GameState
    PrintingChar :: Char -> GameState
    PrintingUnknown :: GameState
    Exiting :: GameState
    deriving (Show, Eq)

printScreen :: Vty -> GameState -> Picture
printScreen vty Waiting =
    let text = string (defAttr `withForeColor` green) "Press a key..."
    in picForImage text
printScreen vty (PrintingChar c) =
    let text = string (defAttr `withForeColor` green) "You pressed: "
        char = string (defAttr `withForeColor` white `withBackColor` black) [c]
        end = string (defAttr `withForeColor` green) "."
        img = text <|> char <|> end
    in picForImage img
printScreen vty PrintingUnknown =
    let text = string (defAttr `withForeColor` red) "I don't know that key!"
    in picForImage text

updateState :: Vty -> GameState -> IO GameState
updateState vty _ = do
    e <- nextEvent vty
    return $ case e of
        EvKey (KChar 'c') [MCtrl] -> Exiting
        EvKey (KChar c) _ -> PrintingChar c
        _ -> PrintingUnknown

mainLoop :: Vty -> GameState -> IO ()
mainLoop vty state = do
    update vty $ printScreen vty state
    newState <- updateState vty state
    if newState == Exiting
        then return ()
        else mainLoop vty newState

main :: IO ()
main = do
    let state = Waiting
    vty <- mkVty defaultConfig
    mainLoop vty state
    shutdown vty
