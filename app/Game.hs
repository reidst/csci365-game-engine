module Main where

import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)

import Data.Array

import Control.Monad
import Control.Monad.RWS

import System.Random

data Player = Player
    { playerCoord :: Coord
    , playerHealth :: Int
    , playerPotions :: Int
    } deriving (Show,Eq)

data World = World
    { player :: Player
    , level :: Level
    }
    deriving (Show,Eq)

data Level = Level
    { levelStart :: Coord
    , levelEnd :: Coord
    , levelGeo :: Geo
    -- building the geo image is expensive. Cache it. Though VTY should go
    -- through greater lengths to avoid the need to cache images.
    , levelGeoImage :: V.Image
    }
    deriving (Show,Eq)

data LevelPiece
    = EmptySpace
    | Rock
    deriving (Show, Eq)

type Game = RWST V.Vty () World IO
type Geo = Array Coord LevelPiece
type Coord = (Int, Int)

initialPlayerHealth :: Int
initialPlayerHealth = 100

initialPlayerPotions :: Int
initialPlayerPotions = 3

main :: IO ()
main = do
    vty <- mkVty V.defaultConfig
    level0 <- mkLevel 1
    let world0 = World (Player (levelStart level0) initialPlayerHealth initialPlayerPotions) level0
    (_finalWorld, ()) <- execRWST play vty world0
    V.shutdown vty

-- |Generate a level randomly using the specified difficulty.  Higher
-- difficulty means the level will have more rooms and cover a larger area.
mkLevel :: Int -> IO Level
mkLevel difficulty = do
    let size = 80 * difficulty
    [levelWidth, levelHeight] <- replicateM 2 $ randomRIO (size,size)
    let randomP = (,) <$> randomRIO (2, levelWidth-3) <*> randomRIO (2, levelHeight-3)
    start <- randomP
    end <- randomP
    -- first the base geography: all rocks
    let baseGeo = array ((0,0), (levelWidth-1, levelHeight-1))
                        [((x,y),Rock) | x <- [0..levelWidth-1], y <- [0..levelHeight-1]]
    -- next the empty spaces that make the rooms
    -- for this we generate a number of center points
    centers <- replicateM (2 ^ difficulty + difficulty) randomP
    -- generate rooms for all those points, plus the start and end
    geo <- foldM (addRoom levelWidth levelHeight) baseGeo (start : end : centers)
    return $ Level start end geo (buildGeoImage geo)

-- |Add a room to a geography and return a new geography.  Adds a
-- randomly-sized room centered at the specified coordinates.
addRoom :: Int
        -> Int
        -- ^The width and height of the geographical area
        -> Geo
        -- ^The geographical area to which a new room should be added
        -> Coord
        -- ^The desired center of the new room.
        -> IO Geo
addRoom levelWidth levelHeight geo (centerX, centerY) = do
    size <- randomRIO (5,15)
    let xMin = max 1 (centerX - size)
        xMax = min (levelWidth - 1) (centerX + size)
        yMin = max 1 (centerY - size)
        yMax = min (levelHeight - 1) (centerY + size)
    let room = [((x,y), EmptySpace) | x <- [xMin..xMax - 1], y <- [yMin..yMax - 1]]
    return (geo // room)

pieceA, dumpA :: V.Attr
pieceA = V.defAttr `V.withForeColor` V.blue `V.withBackColor` V.green
dumpA = V.defAttr `V.withStyle` V.reverseVideo

play :: Game ()
play = do
    updateDisplay
    done <- processEvent
    unless done play

processEvent :: Game Bool
processEvent = do
    k <- ask >>= liftIO . V.nextEvent
    if k == V.EvKey V.KEsc []
        then return True
        else do
            case k of
                V.EvKey (V.KChar 'r') [V.MCtrl] -> ask >>= liftIO . V.refresh
                V.EvKey V.KLeft  []             -> movePlayer (-1) 0
                V.EvKey V.KRight []             -> movePlayer 1 0
                V.EvKey V.KUp    []             -> movePlayer 0 (-1)
                V.EvKey V.KDown  []             -> movePlayer 0 1
                V.EvKey (V.KChar 'h') []        -> usePotion
                _                               -> return ()
            return False

movePlayer :: Int -> Int -> Game ()
movePlayer dx dy = do
    world <- get
    let Player (x, y) health potions = player world
    let x' = x + dx
        y' = y + dy
    -- this is only valid because the level generation assures the border is
    -- always Rock
    case levelGeo (level world) ! (x',y') of
        EmptySpace -> put $ world { player = Player (x',y') health potions}
        _          -> return ()


updateDisplay :: Game ()
updateDisplay = do
    let info = V.string V.defAttr "Move with the arrows keys. Press ESC to exit."
    -- determine offsets to place the player in the center of the level.
    (w,h) <- asks V.outputIface >>= liftIO . V.displayBounds
    thePlayer <- gets player
    let ox = (w `div` 2) - playerX thePlayer
        oy = (h `div` 2) - playerY thePlayer
    -- translate the world images to place the player in the center of the
    -- level.
    world' <- map (V.translate ox oy) <$> worldImages
    let playerInfo = V.translate 0 (h-1) (playerInfoImage thePlayer)
    let pic = V.picForLayers $ info : playerInfo : world'
    vty <- ask
    liftIO $ V.update vty pic

--
-- Image-generation functions
--

worldImages :: Game [V.Image]
worldImages = do
    thePlayer <- gets player
    theLevel <- gets level
    let playerImage = V.translate (playerX thePlayer) (playerY thePlayer) (V.char pieceA '@')
    return [playerImage, levelGeoImage theLevel]

imageForGeo :: LevelPiece -> V.Image
imageForGeo EmptySpace = V.char (V.defAttr `V.withBackColor` V.green) ' '
imageForGeo Rock = V.char V.defAttr 'X'

buildGeoImage :: Geo -> V.Image
buildGeoImage geo =
    let (geoWidth, geoHeight) = snd $ bounds geo
    -- seems like a the repeated index operation should be removable. This is
    -- not performing random access but (presumably) access in order of index.
    in V.vertCat [ geoRow
                 | y <- [0..geoHeight-1]
                 , let geoRow = V.horizCat [ i
                                           | x <- [0..geoWidth-1]
                                           , let i = imageForGeo (geo ! (x,y))
                                           ]
                 ]

--
-- Miscellaneous
--
usePotion :: Game ()
usePotion = do
    world <- get
    let Player (x, y) health potions = player world
    when (potions > 0) $ put $ world { player = Player (x, y) (health + 5) (potions - 1) }

playerX :: Player -> Int
playerX = fst . playerCoord

playerY :: Player -> Int
playerY = snd . playerCoord

playerInfoImage :: Player -> V.Image
playerInfoImage player = V.string V.defAttr ("Health: " ++ show (playerHealth player) ++ "  Potions: " ++ show (playerPotions player) ++ "  Power: 0   Key: X" )
