{-# LANGUAGE GADTSyntax #-}
{-# OPTIONS_GHC -Wall #-}

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
    , monsters :: [Monster]
    }
    deriving (Show,Eq)

data Level = Level
    { levelDifficulty :: Int
    , levelStart :: Coord
    , levelEnd :: Coord
    , levelGeo :: Geo
    , doorCoord :: Coord
    , levelGeoImage :: V.Image
    }
    deriving (Show,Eq)

data LevelPiece where
    EmptySpace :: LevelPiece
    Rock       :: LevelPiece
    Chest      :: ChestContents -> LevelPiece
    Door       :: LevelPiece
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

-- These count/frequency variables are all per room
chestFrequency :: Int
chestFrequency = 15

monsterCount :: Int -> Int
monsterCount difficulty = 2 * difficulty ^ (2 :: Int) - 15

roomCount :: Int -> Int
roomCount difficulty = 2 ^ difficulty + 20 * difficulty

monsterSlowness :: Int
monsterSlowness = 60

possibleMonsters :: [MonsterStats]
possibleMonsters = [MonsterStats "Goblin" 20 10,
                    MonsterStats "Sentient Chair" 10 5,
                    MonsterStats "Troll" 40 32,
                    MonsterStats "Witch" 30 25]

possibleWeapons :: [Weapon]
possibleWeapons = [Weapon "Oak Staff" 8,
                   Weapon "Dagger" 12,
                   Weapon "Magic Staff" 18,
                   Weapon "Claymore" 24,
                   Weapon "Orb of Disassembly" 50,
                   Weapon "Coughing Baby" 2,
                   Weapon "Hydrogen Bomb" 75]

initialPlayerHealth :: Int
initialPlayerHealth = 1000

initialPlayerPotions :: Int
initialPlayerPotions = 3

initialPlayerWeapon :: Weapon
initialPlayerWeapon = Weapon "Hand" 5

potionHealing :: Int
potionHealing = 10

animationConstant :: Int
animationConstant = 15

playerDamageConstant :: Int
playerDamageConstant = 5

main :: IO ()
main = do
    vty <- mkVty V.defaultConfig
    level0 <- mkLevel 3
    monsters0 <- spawnMonsters level0
    let player0 = Player (levelStart level0)
                  initialPlayerHealth initialPlayerWeapon initialPlayerPotions
                  False (animationConstant * 3) 0 Right
        world0 = World player0 level0 monsters0
    (finalWorld, ()) <- execRWST (play 0) vty world0
    let finalScore = score $ player finalWorld
        finalLevel = levelDifficulty (level finalWorld) - 3
    V.shutdown vty
    putStrLn $ "Dead! :,(\nYou descended " ++ show finalLevel ++
                " floors and scored " ++ show finalScore ++
                " points.\nThanks for playing!"

-- Generate a level randomly using the specified difficulty.  Higher
-- difficulty means the level will have more rooms and cover a larger area.
mkLevel :: Int -> IO Level
mkLevel difficulty = do
    let size = 15 * difficulty
        levelWidth = size * 2
        levelHeight = size
        randomP = (,) <$> randomRIO (2, levelWidth-3)
                      <*> randomRIO (2, levelHeight-3)
    start <- randomP
    end <- randomP
    let baseGeo = array ((0,0), (levelWidth-1, levelHeight-1))
                        [((x,y),Rock) | x <- [0..levelWidth-1],
                                        y <- [0..levelHeight-1]]
    centers <- replicateM (roomCount difficulty) randomP
    geo <- foldM (addRoom levelWidth levelHeight) baseGeo
                 (start : end : centers)
    let edgeSpaces = [(x, y) | x <- [0..levelWidth-1],
                            y <- [0..levelHeight-1],
                            geo ! (x, y) == Rock,
                            x == 0 || y == 0 || x == levelWidth-2 || y == levelHeight-2]
    
    newDoorCoord <- getDoorCoord geo edgeSpaces levelWidth levelHeight
    let door = [(newDoorCoord, Door)]

    return $ Level difficulty start end (geo // door) newDoorCoord
                   (buildGeoImage (geo // door))

getDoorCoord :: Geo -> [(Int, Int)] -> Int -> Int -> IO Coord
getDoorCoord geo edgeSpaces levelWidth levelHeight = do
    let randomIndex = randomRIO (0, length edgeSpaces - 1)
    index <- randomIndex
    let possDoorCoord = edgeSpaces !! index
    let neighbor = case possDoorCoord of
            (0,y) -> (1,y)
            (x,0) -> (x,1)
            (w,y) | w == levelWidth-2 -> (w-1, y)
            (x,h) | h == levelHeight-2 -> (x,h-1)
            _ -> (0,0)
    if geo ! neighbor == EmptySpace
        then return possDoorCoord
        else getDoorCoord geo edgeSpaces levelWidth levelHeight
        

    

spawnMonsters :: Level -> IO [Monster]
spawnMonsters l = do
    let allSpawnLocations = monsterSpawnLocations $ levelGeo l
        difficulty = levelDifficulty l
        numMonsters = monsterCount difficulty
    spawnLocations <- sequence $ replicate numMonsters $ (allSpawnLocations !!)
                      <$> randomRIO (0, length allSpawnLocations - 1)
    stats <- sequence $ replicate numMonsters $ (possibleMonsters !!)
                      <$> randomRIO (0, length possibleMonsters - 1)
    let monstersNoKey = zipWith (\loc stat -> Monster loc stat False) 
                        spawnLocations stats
        firstMonster = (head monstersNoKey) { monsterHasKey = True }
    return $ firstMonster : tail monstersNoKey

-- Add a room to a geography and return a new geography.  Adds a
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
    let room = [((x,y), EmptySpace) | x <- [xMin..xMax - 1],
                                      y <- [yMin..yMax - 1]]
        chest = [((chestX, chestY), Chest chestContents)]
    return $ geo
          // room
          // (if hasChest then chest else [])

-- Randomly generates ChestContents to be put in a Chest
generateChestContents :: IO ChestContents
generateChestContents = do
    potionWeapon <- randomRIO (0, 2) :: IO Int
    if even potionWeapon
    then ChestPotion <$> randomRIO (1, 3)
    else ChestWeapon <$> getRandomWeapon

-- What everything looks like!
playerA, rockA, monsterA, chestA, swordA :: V.Attr
playerA  = V.defAttr `V.withBackColor` V.black `V.withForeColor` V.blue
rockA    = V.defAttr `V.withBackColor` V.black `V.withForeColor` V.white
monsterA = V.defAttr `V.withBackColor` V.black `V.withForeColor` V.red
chestA   = V.defAttr `V.withBackColor` V.black `V.withForeColor` V.yellow
swordA   = V.defAttr `V.withBackColor` V.black `V.withForeColor` V.yellow

-- The gameplay loop
play :: Int -> Game ()
play frame = do
    liftIO $ C.threadDelay 1000
    incrementAttack
    when (frame `mod` animationConstant == 0) checkAttack
    when (frame `mod` playerDamageConstant == 0) checkPlayer
    moveMonsters
    updateDisplay
    done <- processEvent
    isDead <- isPlayerDead
    unless (done || isDead) (play (frame + 1))

-- Processes events (key presses, in this case)
processEvent :: Game Bool
processEvent = do
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
                        V.EvKey (V.KChar ' ') []        -> playerBeginAttack
                        _                               -> return ()
                    return False

-- Moves the map around the player, but it looks like it moves the player!
movePlayer :: Int -> Int -> Game ()
movePlayer dx dy = do
    world <- get
    thePlayer <- gets player
    let x = playerX thePlayer
        y = playerY thePlayer
        x' = x + dx
        y' = y + dy
    case levelGeo (level world) ! (x',y') of
        EmptySpace -> put $ world {
            player = thePlayer { playerCoord = (x',y') 
                               , playerDirection = (getDirection dx dy) }
        }
        Chest (ChestPotion potionCount) -> let
            newPlayer = thePlayer {
                playerCoord = (x, y),
                playerPotions = playerPotions thePlayer + potionCount,
                score = score thePlayer + 25,
                playerDirection = getDirection dx dy
            }
            newGeo = levelGeo (level world) // [((x', y'), Chest ChestEmpty)]
            newLevel = (level world) { levelGeo = newGeo, 
                                       levelGeoImage = buildGeoImage newGeo }
            in put $ world { player = newPlayer, level = newLevel }
        Chest (ChestWeapon newWeapon) -> let
            newPlayer = thePlayer { currentWeapon = newWeapon,
                                    score = (score thePlayer + 25), 
                                    playerDirection = (getDirection dx dy)}
            newGeo = levelGeo (level world) // [((x', y'), Chest ChestEmpty)]
            newLevel = (level world) { levelGeo = newGeo,
                                       levelGeoImage = buildGeoImage newGeo }
            in put $ world { player = newPlayer, level = newLevel }
        Door -> when (playerHasKey thePlayer) $ do
            let newDifficulty = ((+1) . levelDifficulty . level) world
            newLevel <- liftIO $ mkLevel newDifficulty
            newMonsters <- liftIO $ spawnMonsters newLevel
            let newPlayer = thePlayer {
                playerCoord = levelStart newLevel,
                playerHasKey = False,
                score = score thePlayer + 100,
                playerDirection = getDirection dx dy
            }
            put $ World newPlayer newLevel newMonsters

        _ -> return ()

-- Moves all monsters in the level
moveMonsters :: Game ()
moveMonsters = do
    world <- get
    theMonsters <- gets monsters
    newMonsters <- sequence $ map moveMonster theMonsters
    put $ world { monsters = newMonsters }

-- Moves a single monster randomly around the map
moveMonster :: Monster -> Game Monster
moveMonster m = do
    let oldX = monsterX m
        oldY = monsterY m
    ri <- randomRIO (0, 3)
    shouldMove <- (== 0) <$> randomRIO (0, monsterSlowness)
    let (dx, dy) = [(0, 1), (0, -1), (1, 0), (-1, 0)] !! ri
        newPos = (oldX + dx, oldY + dy)
    geo <- gets $ levelGeo . level
    return $ m {
        monsterCoord = if geo ! newPos == EmptySpace && shouldMove
            then newPos
            else (oldX, oldY)
    }


-- Checks if a monster is should be dead
-- Updates the hasKey variable for the player if the monster killed has the key
checkAttack :: Game ()
checkAttack = do
    world <- get
    thePlayer <- gets player
    theMonsters <- gets monsters
    let newMonsters = map (\m -> checkMonsterAttacked m thePlayer) theMonsters
        validMonsters = 
            filter (\m -> monsterHealth (monsterStats m) > 0) newMonsters
        keyMonster = 
            filter (\m -> (monsterHealth (monsterStats m) <= 0 && monsterHasKey m))
                   newMonsters
        newPlayer = if null keyMonster
                    then thePlayer
                    else thePlayer { playerHasKey = True }
    put $ world { monsters = validMonsters, player = newPlayer }

-- Checks if the player is being attacked by a monster
checkPlayerAttacked :: Monster -> Player -> Player
checkPlayerAttacked m p
    | (mx == px) && (my == py) = p { playerHealth = pHealth - mDamage}
    | otherwise = p
    where
        mx = monsterX m
        my = monsterY m
        px = playerX p
        py = playerY p
        pHealth = playerHealth p
        mDamage = monsterDamage $ monsterStats m


-- Checks if the player is being attacked by *any* of the monsters
checkPlayer :: Game ()
checkPlayer = do
    world <- get
    thePlayer <- gets player
    theMonsters <- gets monsters
    let newPlayer = foldr checkPlayerAttacked thePlayer theMonsters
    put $ world { player = newPlayer }

-- Checks if the player is dead
isPlayerDead :: Game Bool
isPlayerDead = do
    thePlayer <- gets player
    if (playerHealth thePlayer) <= 0 then return True else return False

-- Checks if a monster is being attacked by the player
checkMonsterAttacked :: Monster -> Player -> Monster
checkMonsterAttacked m p
    | (mx == (fst (getSwordCoords p))) && (my == (snd (getSwordCoords p))) =
        m {monsterStats = stats {monsterHealth = newMonsterHealth}}
    | otherwise = m
    where
        mx = monsterX m
        my = monsterY m
        stats = monsterStats m
        newMonsterHealth = monsterHealth stats - weaponAttack (currentWeapon p)

-- Updates the game's display
updateDisplay :: Game ()
updateDisplay = do
    let info = V.string V.defAttr "Move with the arrows keys. Press ESC to exit."
    (w,h) <- asks V.outputIface >>= liftIO . V.displayBounds
    thePlayer <- gets player
    let ox = (w `div` 2) - playerX thePlayer
        oy = (h `div` 2) - playerY thePlayer
    world' <- map (V.translate ox oy) <$> worldImages
    let playerInfo = V.translate 0 (h-1) (playerInfoImage thePlayer)
    let pic = V.picForLayers $ info : playerInfo : world'
    vty <- ask
    liftIO $ V.update vty pic

-- Generates the constant images in the world
-- (player, monsters, sword, etc.)
worldImages :: Game [V.Image]
worldImages = do
    thePlayer <- gets player
    theLevel <- gets level
    theMonsters <- gets monsters
    let playerImage   = V.translate (playerX thePlayer) (playerY thePlayer)
                        (V.char playerA '@')
    let monsterImages = map (\m -> V.translate (monsterX m) (monsterY m)
                        (V.char monsterA $ monsterChar m)) theMonsters
    let swordImage = generateSword thePlayer
    return $ monsterImages
          ++ [playerImage, swordImage]
          ++ [levelGeoImage theLevel]

-- Gets the monster's character
monsterChar :: Monster -> Char
monsterChar = head . monsterName . monsterStats

-- Sets the characters for each LevelPiece
imageForGeo :: LevelPiece -> V.Image
imageForGeo EmptySpace = V.char (V.defAttr `V.withBackColor` V.black) ' '
imageForGeo Rock = V.char rockA 'X'
imageForGeo (Chest ChestEmpty) =
    V.char chestA 'X'
imageForGeo (Chest _) =
    V.char chestA '?'
imageForGeo Door = V.char playerA 'D'

-- Builds the level's geography
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

-- Retrieves a random monster from the possibleMonsters
getRandomMonster :: IO MonsterStats
getRandomMonster = do
    ri <- randomRIO (0, length possibleMonsters - 1)
    return $ (possibleMonsters !! ri)

-- Retrieves the monster's name
getMonsterName :: Monster -> String
getMonsterName (Monster _ (MonsterStats name _ _) _) = name

-- Retrieves a random weapon from the possibleWeapons
getRandomWeapon :: IO Weapon
getRandomWeapon = do
    wi <- randomRIO (0, length possibleWeapons - 1)
    return $ possibleWeapons !! wi

-- Let's the player health via potion
usePotion :: Game ()
usePotion = do
    world <- get
    thePlayer <- gets player
    let oldHealth = playerHealth thePlayer
        newHealth = min initialPlayerHealth (oldHealth + potionHealing)
        newPotionCount = playerPotions thePlayer - 1
    when (playerPotions thePlayer > 0) $ put $ world { player =
        thePlayer { playerHealth = newHealth, playerPotions = newPotionCount }}

-- Adds a potion to the player's potion count
addPotion :: Game ()
addPotion = do
    world <- get
    thePlayer <- gets player
    put $ world { player =
        thePlayer {playerPotions = playerPotions thePlayer + 1}}

-- Retrieves the player's x coordinate
playerX :: Player -> Int
playerX = fst . playerCoord

-- Retrives the player's y coordinate
playerY :: Player -> Int
playerY = snd . playerCoord

-- Checks to see if player is attacking
playerAttacking :: Player -> Bool
playerAttacking (Player _ _ _ _ _ a _ _)
    | a < (3 * animationConstant) = True
    | otherwise = False

-- Increments the player's attack animation counter accordingly
incrementAttack :: Game ()
incrementAttack = do
    world <- get
    thePlayer <- gets player
    let ani = playerAttackCounter thePlayer + 1
    put $ world { player = thePlayer { playerAttackCounter = ani }}

-- Begins the player's attack animation counter
playerBeginAttack :: Game ()
playerBeginAttack = do
    world <- get
    thePlayer <- gets player
    put $ world { player = thePlayer { playerAttackCounter = 0 }}

-- Retrieve the direction the player is facing depending on coordinates given
getDirection :: Int -> Int -> Direction
getDirection x y
    | (x == -1) && (y == 0) = Left
    | (x == 1) && (y == 0)  = Right
    | (x == 0) && (y == -1) = Up
    | (x == 0) && (y == 1)  = Down
    | otherwise = Right

-- Create the sword depending on direction and animation counter
generateSword :: Player -> V.Image
generateSword (Player (x, y) _ _ _ _ a _ dir)
    | (a >= 3 * animationConstant) = V.emptyImage
    | (dir == Right) && (a > 2 * animationConstant) && (a < 3 * animationConstant)
        = V.translate (x + 1) (y + 1) (V.char swordA '\\')
    | (dir == Right) && (a > animationConstant) && (a <= 2 * animationConstant)
        = V.translate (x + 1) (y) (V.char swordA '-')
    | (dir == Right) && (a > 0) && (a <= animationConstant)
        = V.translate (x + 1) (y - 1) (V.char swordA '/')
    | (dir == Left) && (a > 2 * animationConstant) && (a < 3 * animationConstant)
        = V.translate (x - 1) (y + 1) (V.char swordA '/')
    | (dir == Left) && (a > animationConstant) && (a <= 2 * animationConstant)
        = V.translate (x - 1) (y) (V.char swordA '-')
    | (dir == Left) && (a > 0) && (a <= animationConstant)
        = V.translate (x - 1) (y - 1) (V.char swordA '\\')
    | (dir == Down) && (a > 2 * animationConstant) && (a < 3 * animationConstant)
        = V.translate (x + 1) (y + 1) (V.char swordA '\\')
    | (dir == Down) && (a > animationConstant) && (a <= 2 * animationConstant)
        = V.translate (x) (y + 1) (V.char swordA '|')
    | (dir == Down) && (a > 0) && (a <= animationConstant)
        = V.translate (x - 1) (y + 1) (V.char swordA '/')
    | (dir == Up) && (a > 2 * animationConstant) && (a < 3 * animationConstant)
        = V.translate (x + 1) (y - 1) (V.char swordA '/')
    | (dir == Up) && (a > animationConstant) && (a <= 2 * animationConstant)
        = V.translate (x) (y - 1) (V.char swordA '|')
    | (dir == Up) && (a > 0) && (a <= animationConstant)
        = V.translate (x - 1) (y - 1) (V.char swordA '\\')
    | otherwise = V.emptyImage

-- Retrieve appropriate coordinates for the sword
-- Relative to player's current coordinates
getSwordCoords :: Player -> Coord
getSwordCoords (Player (x, y) _ _ _ _ a _ dir)
    | (dir == Right) && (a > 2 * animationConstant) && (a < 3 * animationConstant)
        = ((x + 1), (y + 1))
    | (dir == Right) && (a > animationConstant) && (a <= 2 * animationConstant)
        = ((x + 1), y)
    | (dir == Right) && (a > 0) && (a <= animationConstant)
        = ((x + 1), (y - 1))
    | (dir == Left) && (a > 2 * animationConstant) && (a < 3 * animationConstant)
        = ((x - 1), (y - 1))
    | (dir == Left) && (a > animationConstant) && (a <= 2 * animationConstant)
        = ((x - 1), y)
    | (dir == Left) && (a > 0) && (a <= animationConstant)
        = ((x - 1), (y - 1))
    | (dir == Down) && (a > 2 * animationConstant) && (a < 3 * animationConstant)
        = ((x + 1), (y + 1))
    | (dir == Down) && (a > animationConstant) && (a <= 2 * animationConstant)
        = (x, (y + 1))
    | (dir == Down) && (a > 0) && (a <= animationConstant)
        = ((x - 1), (y + 1))
    | (dir == Up) && (a > 2 * animationConstant) && (a < 3 * animationConstant)
        = ((x + 1), (y - 1))
    | (dir == Up) && (a > animationConstant) && (a <= 2 * animationConstant)
        = (x, (y - 1))
    | (dir == Up) && (a > 0) && (a <= animationConstant)
        = ((x - 1), (y - 1))
    | otherwise = (0, 0)

-- Retrieves monster spawn locations
monsterSpawnLocations :: Geo -> [Coord]
monsterSpawnLocations geo = [i | (i, e) <- assocs geo, e == EmptySpace]

-- Retrieves the monster's x coordinate
monsterX :: Monster -> Int
monsterX = fst . monsterCoord

-- Retrieves the monster's y coordinate
monsterY :: Monster -> Int
monsterY = snd . monsterCoord

-- Displays the player's info at the bottom of the screen
playerInfoImage :: Player -> V.Image
playerInfoImage p = V.string V.defAttr $ 
    "Health: " ++ show (playerHealth p `div` 10)
    ++ "  Potions: " ++ show (playerPotions p)
    ++ "  Weapon: " ++ weaponName (currentWeapon p)
    ++ "  Power: " ++ show (weaponAttack $ currentWeapon p)
    ++ "  Key: " ++ (if playerHasKey p then "✓" else "X")
    ++ "  Score: " ++ show (score p)
