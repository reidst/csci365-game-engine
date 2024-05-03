{-# LANGUAGE GADTSyntax #-}

module Main where

import qualified Control.Concurrent as C
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Prelude hiding (Right, Left)

import Data.Array

import Control.Monad
import Control.Monad.RWS

import System.Random

data Player = Player
    { playerCoord :: Coord
    , playerHealth :: Int
    , currentWeapon :: Weapon
    , playerPotions :: Int
    , playerHasKey :: Bool
    , playerAttackCounter :: Int
    , score :: Int
    , playerDirection :: Direction
    } deriving (Show,Eq)

data Monster = Monster
    { monsterCoord :: Coord
    , monsterStats :: MonsterStats
    , monsterHasKey :: Bool
    } deriving (Show, Eq)

data MonsterStats = MonsterStats
    { monsterName :: String
    , monsterHealth :: Int
    , monsterDamage :: Int
    } deriving (Show, Eq)

data Weapon = Weapon
    { weaponName :: String
    , weaponAttack :: Int
    } deriving (Show, Eq)

data World = World
    { player :: Player
    , level :: Level
    }
    deriving (Show,Eq)

data Level = Level
    { levelStart :: Coord
    , levelEnd :: Coord
    , levelGeo :: Geo
    , doorCoord :: Coord
    -- building the geo image is expensive. Cache it. Though VTY should go
    -- through greater lengths to avoid the need to cache images.
    , levelGeoImage :: V.Image
    }
    deriving (Show,Eq)

newtype Door = Door Bool
    deriving (Show,Eq)

data LevelPiece where
    EmptySpace :: LevelPiece
    Rock       :: LevelPiece
    Chest      :: ChestContents -> LevelPiece
    DoorPiece :: Door -> LevelPiece
    deriving (Show, Eq)

data ChestContents where
    ChestPotion :: Int -> ChestContents
    ChestWeapon :: Weapon -> ChestContents
    ChestEmpty  :: ChestContents
    deriving (Show, Eq)

data Direction where
    Up :: Direction
    Down :: Direction
    Left :: Direction
    Right :: Direction
    deriving (Show, Eq)

type Game = RWST V.Vty () World IO
type Geo = Array Coord LevelPiece
type Coord = (Int, Int)

chestFrequency :: Int
chestFrequency = 15

monsterFrequency :: Int
monsterFrequency = 50

possibleMonsters :: [MonsterStats]
possibleMonsters = [MonsterStats "Goblin" 20 5,
                    MonsterStats "Sentient Chair" 10 2,
                    MonsterStats "Troll" 40 10,
                    MonsterStats "Witch" 30 8]

possibleWeapons :: [Weapon]
possibleWeapons = [Weapon "Oak Staff" 8,
                   Weapon "Dagger" 12,
                   Weapon "Magic Staff" 18,
                   Weapon "Claymore" 24,
                   Weapon "Orb of Disassembly" 50,
                   Weapon "Coughing Baby" 2,
                   Weapon "Hydrogen Bomb" 75]

initialPlayerHealth :: Int
initialPlayerHealth = 100

initialPlayerPotions :: Int
initialPlayerPotions = 3

initialPlayerWeapon :: Weapon
initialPlayerWeapon = Weapon "Hand" 5

animationConstant :: Int
animationConstant = 15

main :: IO ()
main = do
    vty <- mkVty V.defaultConfig
    level0 <- mkLevel 8
    let world0 = World (Player (levelStart level0) initialPlayerHealth initialPlayerWeapon initialPlayerPotions False (animationConstant) 0 Right) level0
    (_finalWorld, ()) <- execRWST play vty world0
    V.shutdown vty

-- |Generate a level randomly using the specified difficulty.  Higher
-- difficulty means the level will have more rooms and cover a larger area.
mkLevel :: Int -> IO Level
mkLevel difficulty = do
    let size = 15 * difficulty
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
    let emptySpaces = [(x, y) | x <- [0..levelWidth-1], y <- [0..levelHeight-1], geo ! (x, y) == EmptySpace]
    doorCoord <- (emptySpaces !!) <$> randomRIO (0, length emptySpaces - 1)
    let door = [(doorCoord, DoorPiece (Door False))]

    return $ Level start end (geo // door) doorCoord (buildGeoImage (geo // door))


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
    size <- randomRIO (5,8)
    let xMin = max 1 (centerX - size)
        xMax = min (levelWidth - 2) (centerX + size)
        yMin = max 1 (centerY - size)
        yMax = min (levelHeight - 2) (centerY + size)
    hasChest <- (< chestFrequency) <$> randomRIO (0,99)
    chestX <- randomRIO (xMin, xMax - 1)
    chestY <- randomRIO (yMin, yMax - 1)
    chestContents <- generateChestContents
    let room = [((x,y), EmptySpace) | x <- [xMin..xMax - 1], y <- [yMin..yMax - 1]]
        chest = [((chestX, chestY), Chest chestContents)]
    return $ geo
          // room
          // (if hasChest then chest else [])

generateChestContents :: IO ChestContents
generateChestContents = do
    potionWeapon <- randomRIO (0, 2) :: IO Int
    if even potionWeapon
    then ChestPotion <$> randomRIO (1, 3)
    else ChestWeapon <$> getRandomWeapon

playerA, rockA, monsterA, chestA :: V.Attr
playerA   = V.defAttr `V.withBackColor` V.black `V.withForeColor` V.blue
rockA    = V.defAttr `V.withBackColor` V.black `V.withForeColor` V.white
monsterA = V.defAttr `V.withBackColor` V.black `V.withForeColor` V.red
chestA   = V.defAttr `V.withBackColor` V.black `V.withForeColor` V.yellow
swordA = V.defAttr `V.withBackColor` V.black `V.withForeColor` V.yellow

play :: Game ()
play = do
    liftIO $ C.threadDelay 1000
    thePlayer <- gets player
    incrementAttack thePlayer
    updateDisplay
    done <- processEvent
    unless done play

processEvent :: Game Bool
processEvent = do
    thePlayer <- gets player
    k <- ask >>= liftIO . V.nextEventNonblocking
    case k of
        Nothing -> return False
        Just k2 ->
            if k2 == V.EvKey V.KEsc []
                then return True
                else do
                    case k2 of
                        V.EvKey (V.KChar 'r') [V.MCtrl] -> ask >>= liftIO . V.refresh
                        V.EvKey V.KLeft  []             -> movePlayer (-1) 0
                        V.EvKey V.KRight []             -> movePlayer 1 0
                        V.EvKey V.KUp    []             -> movePlayer 0 (-1)
                        V.EvKey V.KDown  []             -> movePlayer 0 1
                        V.EvKey (V.KChar 'h') []        -> usePotion
                        V.EvKey (V.KChar 'j') []        -> addPotion
                        V.EvKey (V.KChar 'k') []        -> givePlayerKey
                        V.EvKey (V.KChar ' ') []        -> playerBeginAttack thePlayer
                        _                               -> return ()
                    return False

movePlayer :: Int -> Int -> Game ()
movePlayer dx dy = do
    world <- get
    let Player (x, y) health weapon potions haskey ani score dir = player world
    let x' = x + dx
        y' = y + dy
    -- this is only valid because the level generation assures the border is
    -- always Rock
    case levelGeo (level world) ! (x',y') of
        EmptySpace -> put $ world { player = Player (x',y') health weapon potions haskey ani score (getDirection dx dy) }
        Chest (ChestPotion potionCount) -> let
            newPlayer = Player (x, y) health weapon (potions + potionCount) haskey ani (score+25) (getDirection dx dy)
            newGeo = levelGeo (level world) // [((x', y'), Chest ChestEmpty)]
            newLevel = Level {
                levelStart = levelStart $ level world,
                levelEnd = levelEnd $ level world,
                levelGeo = newGeo,
                doorCoord = doorCoord $ level world,
                levelGeoImage = buildGeoImage newGeo }
            --put $ world { player = Player (x,y) health (potions + potionCount), level = _ }
            in put $ world { player = newPlayer, level = newLevel }
        Chest (ChestWeapon newWeapon) -> let
            newPlayer = Player (x, y) health newWeapon potions haskey ani (score+25) (getDirection dx dy)
            newGeo = levelGeo (level world) // [((x', y'), Chest ChestEmpty)]
            newLevel = Level {
                levelStart = levelStart $ level world,
                levelEnd = levelEnd $ level world,
                levelGeo = newGeo,
                doorCoord = doorCoord $ level world,
                levelGeoImage = buildGeoImage newGeo }
            in put $ world { player = newPlayer, level = newLevel }
        DoorPiece (Door False) -> when haskey $ do
            let newGeo = levelGeo (level world) // [((x', y'), DoorPiece (Door True))]
            let newLevel = Level {
                levelStart = levelStart $ level world,
                levelEnd = levelEnd $ level world,
                levelGeo = newGeo,
                doorCoord = doorCoord $ level world,
                levelGeoImage = buildGeoImage newGeo }
            put $ world { player = Player (x, y) health weapon potions haskey ani score (getDirection dx dy), level = newLevel }
        DoorPiece (Door True) -> do
            newLevel <- liftIO $ mkLevel 8
            put $ world { player = Player (levelStart newLevel) health weapon potions False ani (score+100) (getDirection dx dy), level = newLevel}
        _ -> return ()


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
    -- add monsterInfo for the monsters in the room
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
    let playerImage = V.translate (playerX thePlayer) (playerY thePlayer) (V.char playerA '@')
    let swordImage = generateSword thePlayer
    return [playerImage, swordImage, levelGeoImage theLevel]

imageForGeo :: LevelPiece -> V.Image
imageForGeo EmptySpace = V.char (V.defAttr `V.withBackColor` V.black) ' '
imageForGeo Rock = V.char rockA 'X'
imageForGeo (Chest ChestEmpty) =
    V.char chestA 'X'
imageForGeo (Chest _) =
    V.char chestA '?'
imageForGeo (DoorPiece (Door False)) = V.char playerA 'D'
imageForGeo (DoorPiece  (Door True)) = V.char playerA ' '


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

getRandomMonster :: IO MonsterStats
getRandomMonster = do
    ri <- randomRIO (0, length possibleMonsters - 1)
    return $ (possibleMonsters !! ri)

getMonsterName :: Monster -> String
getMonsterName (Monster _ (MonsterStats name _ _) _) = name

getRandomWeapon :: IO Weapon
getRandomWeapon = do
    wi <- randomRIO (0, length possibleWeapons - 1)
    return $ possibleWeapons !! wi

--
-- Miscellaneous
--

usePotion :: Game ()
usePotion = do
    world <- get
    let Player (x, y) health weapon potions key ani score dir = player world
    when (potions > 0) $ put $ world { player = Player (x, y) (health + 5) weapon (potions - 1) key ani score dir}

addPotion :: Game ()
addPotion = do
    world <- get
    let Player (x, y) health weapon potions key ani score dir = player world
    put $ world { player = Player (x, y) health weapon (potions + 1) key ani score dir}

givePlayerKey :: Game ()
givePlayerKey = do
    world <- get
    let Player (x, y) health weapon potions _ ani score dir = player world
    put $ world { player = Player (x, y) health weapon (potions + 1) True ani score dir}

playerX :: Player -> Int
playerX = fst . playerCoord

playerY :: Player -> Int
playerY = snd . playerCoord

playerAttacking :: Player -> Bool
playerAttacking (Player _ _ _ _ _ a _ _)
    | a < (3 * animationConstant) = True
    | otherwise = False

incrementAttack :: Player -> Game ()
incrementAttack (Player coord health weapon potions haskey a score dir) = do
    world <- get
    let Player (x, y) health weapon potions haskey ani score dir = player world
    put $ world { player = Player (x, y) health weapon potions haskey (ani + 1) score dir}

playerBeginAttack :: Player -> Game ()
playerBeginAttack (Player coords health weapon potions haskey ani score dir) = do
    world <- get
    let Player (x, y) health weapon potions haskey _ score dir = player world
    put $ world { player = Player (x, y) health weapon potions haskey 0 score dir}

getDirection :: Int -> Int -> Direction
getDirection x y
    | (x == -1) && (y == 0) = Left
    | (x == 1) && (y == 0)  = Right
    | (x == 0) && (y == -1) = Up
    | (x == 0) && (y == 1)  = Down
    | otherwise = Right

generateSword :: Player -> V.Image
generateSword (Player (x, y) _ _ _ _ a _ dir)
    | (a >= 150) = V.emptyImage
    | (dir == Right) && (a > 100) && (a < 150)  = V.translate (x + 1) (y + 1) (V.char swordA '\\')
    | (dir == Right) && (a > 50) && (a <= 100) = V.translate (x + 1) (y) (V.char swordA '-')
    | (dir == Right) && (a > 0) && (a <= 50)   = V.translate (x + 1) (y - 1) (V.char swordA '/')
    | (dir == Left) && (a > 100) && (a < 150)   = V.translate (x - 1) (y + 1) (V.char swordA '/')
    | (dir == Left) && (a > 50) && (a <= 100)  = V.translate (x - 1) (y) (V.char swordA '-')
    | (dir == Left) && (a > 0) && (a <= 50)    = V.translate (x - 1) (y - 1) (V.char swordA '\\')
    | (dir == Down) && (a > 100) && (a < 150)   = V.translate (x + 1) (y + 1) (V.char swordA '\\')
    | (dir == Down) && (a > 50) && (a <= 100)  = V.translate (x) (y + 1) (V.char swordA '|')
    | (dir == Down) && (a > 0) && (a <= 50)    = V.translate (x - 1) (y + 1) (V.char swordA '/')
    | (dir == Up) && (a > 100) && (a < 150)     = V.translate (x + 1) (y - 1) (V.char swordA '/')
    | (dir == Up) && (a > 50) && (a <= 100)    = V.translate (x) (y - 1) (V.char swordA '|')
    | (dir == Up) && (a > 0) && (a <= 50)      = V.translate (x - 1) (y - 1) (V.char swordA '\\')
    | otherwise = V.emptyImage

monstersX :: Monster -> Int
monstersX = fst . monsterCoord

monstersY :: Monster -> Int
monstersY = snd . monsterCoord

playerInfoImage :: Player -> V.Image
playerInfoImage player = V.string V.defAttr ("Health: " ++ show (playerHealth player) ++ "  Potions: " ++ show (playerPotions player) ++ "  Weapon: " ++ weaponName (currentWeapon player) ++ "  Power: " ++ show (weaponAttack $ currentWeapon player) ++ "  Key: " ++ (if playerHasKey player then "✓  " else "X  ") ++ "Score: " ++ show (score player))
