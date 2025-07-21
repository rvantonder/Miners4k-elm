port module Main exposing (main)

import Array
import Bitwise
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (Html, br, button, canvas, div, h3, input, label, li, p, strong, text, ul)
import Html.Attributes as Attr exposing (height, id, step, style, type_, value, width)
import Html.Events
import Json.Decode as Decode
import Random
import Task
import Time
import Url
import Url.Parser as Parser exposing (Parser)



-- PORTS


port preventContextMenu : () -> Cmd msg


port renderPixels : List Int -> Cmd msg



-- CONSTANTS


gameWidth : Int
gameWidth =
    640


gameHeight : Int
gameHeight =
    480



-- LEVEL CONFIGURATION (Java-accurate)
-- Java: String[] levelNames = {"Level 1: Miners4k", "Level 2: Rocks", ...}


levelNames : Array.Array String
levelNames =
    Array.fromList
        [ "Level 1: Miners4k"
        , "Level 2: Rocks"
        , "Level 3: Descent"
        , "Level 4: Slime"
        , "Level 5: Massive"
        , "Level 6: Riiiight"
        , "You won!"
        ]



-- Java: String[] infoStrings = {"Bring the gold home!", "Rocks are impenetrable.", ...}


levelInfoStrings : Array.Array String
levelInfoStrings =
    Array.fromList
        [ "Bring the gold home!"
        , "Rocks are impenetrable."
        , "Use arrow keys to scroll."
        , "Slime explodes on contact!"
        , "Wide level!"
        , "Timekiller!"
        , "Bonus level!"
        ]



-- Java: int[] slimes = {0, 0, 0, 6, 10, 25, 0}


levelSlimes : Array.Array Int
levelSlimes =
    Array.fromList [ 0, 0, 0, 6, 10, 25, 0 ]



-- Calculate level parameters exactly like Java


type alias LevelConfig =
    { width : Int -- level_width = current_level / 4 * 384 + 640
    , height : Int -- level_height = current_level > 1 ? 1024 : 480
    , rocks : Int -- level_rocks = (current_level - 1) / 2 * 100
    , target : Int -- level_target = current_level * 500
    , diggers : Int -- level_diggers = current_level * current_level * 50
    , goldLumps : Int -- level_goldLumps = current_level * current_level * 50
    , slimes : Int -- slimes[current_level]
    , timeLimit : Int -- level_timeLimit = level_target * 2
    }


getLevelConfig : Int -> LevelConfig
getLevelConfig currentLevel =
    let
        -- Java level calculation formulas
        baseWidth =
            currentLevel // 4 * 384 + 640

        baseHeight =
            if currentLevel > 1 then
                1024

            else
                480

        baseRocks =
            (currentLevel - 1) // 2 * 100

        baseTarget =
            currentLevel * 500

        baseDiggers =
            currentLevel * currentLevel * 50

        baseGoldLumps =
            currentLevel * currentLevel * 50

        slimeCount =
            Array.get currentLevel levelSlimes |> Maybe.withDefault 0

        -- Special level overrides (exact Java matches)
        config =
            if currentLevel == 0 then
                -- Level 1: Special starter level
                { width = baseWidth
                , height = baseHeight
                , rocks = 0
                , target = 100
                , diggers = 50
                , goldLumps = 10
                , slimes = slimeCount
                , timeLimit = 200
                }

            else if currentLevel == 1 then
                -- Level 2: Custom values
                { width = baseWidth
                , height = baseHeight
                , rocks = 10
                , target = 200
                , diggers = baseDiggers
                , goldLumps = 30
                , slimes = slimeCount
                , timeLimit = 400
                }

            else if currentLevel == 2 then
                -- Level 3: rocks = 50
                { width = baseWidth
                , height = baseHeight
                , rocks = 50
                , target = baseTarget
                , diggers = baseDiggers
                , goldLumps = baseGoldLumps
                , slimes = slimeCount
                , timeLimit = baseTarget * 2
                }

            else if currentLevel == 6 then
                -- Level 7: Massive bonus level
                { width = baseWidth
                , height = 2048
                , rocks = baseRocks
                , target = 99999
                , diggers = 800
                , goldLumps = baseGoldLumps
                , slimes = slimeCount
                , timeLimit = 199998
                }

            else
                -- Standard formula levels
                { width = baseWidth
                , height = baseHeight
                , rocks = baseRocks
                , target = baseTarget
                , diggers = baseDiggers
                , goldLumps = baseGoldLumps
                , slimes = slimeCount
                , timeLimit = baseTarget * 2
                }
    in
    config



-- TYPES


type alias Model =
    { key : Nav.Key -- Navigation key for URL changes
    , url : Url.Url -- Current URL
    , level : Array.Array Int -- Flat array like Java: level[x | y << 10]
    , currentLevel : Int
    , levelStarted : Bool
    , levelWidth : Int
    , levelHeight : Int
    , cameraX : Int
    , cameraY : Int
    , randomSeed : Random.Seed
    , miners : List Miner
    , mouseX : Int
    , mouseY : Int
    , mouseButton : Int -- -1 = none, 0 = left, 2 = right
    , prevMouseX : Int
    , prevMouseY : Int
    , shiftPressed : Bool

    -- Game state management (Java-accurate)
    , score : Int -- Gold delivered to platforms
    , levelTarget : Int -- Gold needed to complete level
    , totalMiners : Int -- Current number of living miners
    , gameStartTime : Int -- Milliseconds when level started
    , currentTime : Int -- Current time in milliseconds (for timer calculations)
    , gameState : GameState -- Current game state
    , arrowKeysPressed : ArrowKeys -- For camera scrolling

    -- Portal system
    , portalMode : Bool -- True if portal placement mode is active
    , portals : Maybe ( Portal, Maybe Portal ) -- (P1, P2) - P1 always exists if any, P2 optional
    , portalTimeRemaining : Int -- Seconds remaining until portals expire (0 = no portals)
    , portalPreview : Maybe Portal -- Preview portal shown under mouse cursor
    , dirtPreview : Maybe ( Int, Int ) -- Preview dirt area shown under mouse cursor (x, y)

    -- Pause system
    , paused : Bool -- True if game is paused (space key)

    -- Speed control system
    , gameSpeed : Float -- Speed multiplier: 1.0 = normal (25ms), 0.5 = half speed (50ms), 2.0 = double speed (12.5ms)

    -- Config/Cheat panel
    , configPanelExpanded : Bool -- True if config/cheat panel is expanded
    , configStartingTime : Int -- Starting time in seconds for cheat mode
    , configGoldValue : Int -- Starting gold value for cheat mode
    , configNumMiners : Int -- Starting number of miners for cheat mode
    , configLevel : Int -- Starting level for cheat mode
    }


type alias Miner =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , animFrame : Int
    , state : MinerState
    , carryingGold : Bool
    , targetPlatform : Maybe String -- "left" or "right" when seeking platform
    , facingLeft : Bool -- True if miner is facing left
    , jumpVelocity : Int -- Java miners[i][4]: -1=on ground, 0=falling, 1-16=jumping
    , fallDistance : Int -- Java miners[i][7]: tracks pixels fallen, dies at 100+
    }


type MinerState
    = Walking
    | Falling
    | Dying Int -- Death animation frame
    | Dead


type GameState
    = LevelIntro -- Showing level info (Java: !levelStarted)
    | Playing -- Main gameplay (Java: levelStarted && !levelOver)
    | LevelComplete -- Level passed, showing success message
    | LevelFailed -- Level failed, showing failure message
    | GameComplete -- All levels completed


type alias ArrowKeys =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    }


type alias MinerMovement =
    { x : Float
    , y : Float
    , jumpVelocity : Int
    , fallDistance : Int
    , state : MinerState
    , facingLeft : Bool
    }


type alias GoldInteraction =
    { carryingGold : Bool
    , level : Array.Array Int
    , targetPlatform : Maybe String
    , justCollected : Bool
    , justDropped : Bool
    }



-- 2D Array for level data


type alias Array2D a =
    { width : Int
    , height : Int
    , data : List a
    }



-- Portal system


type alias Portal =
    { x : Int -- X position in level coordinates
    , y : Int -- Y position in level coordinates (top of portal)
    , isP1 : Bool -- True for P1 (blue), False for P2 (red)
    }


type Msg
    = NoOp
    | StartLevel
    | RenderFrame
    | UpdateMiners
    | SeedGenerated Int
    | MouseMove Int Int
    | MouseDown Int
    | MouseUp
    | KeyDown Int
    | KeyUp Int
    | NextLevel -- Advance to next level
    | RestartLevel -- Restart current level
    | GameTick Int -- Timer updates with current time
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ActivatePortalMode -- P key pressed, activate portal placement mode
    | PortalTick -- Portal countdown timer tick
    | SetGameSpeed Float -- Change game speed multiplier
    | ToggleConfigPanel -- Toggle config/cheat panel expansion
    | SetConfigStartingTime Int -- Set starting time for cheat mode
    | SetConfigGoldValue Int -- Set starting gold value for cheat mode
    | SetConfigNumMiners Int -- Set starting number of miners for cheat mode
    | SetConfigLevel Int -- Set starting level for cheat mode
    | StartNewWithConfig -- Start new game with current config values



-- URL PARSING


parseLevelFromUrl : Url.Url -> Maybe Int
parseLevelFromUrl url =
    url.query
        |> Maybe.andThen (String.split "&" >> List.filterMap parseLevelParam >> List.head)


parseGoldFromUrl : Url.Url -> Maybe Int
parseGoldFromUrl url =
    url.query
        |> Maybe.andThen (String.split "&" >> List.filterMap parseGoldParam >> List.head)


parseLevelParam : String -> Maybe Int
parseLevelParam param =
    case String.split "=" param of
        [ "level", value ] ->
            String.toInt value

        _ ->
            Nothing


parseGoldParam : String -> Maybe Int
parseGoldParam param =
    case String.split "=" param of
        [ "gold", value ] ->
            String.toInt value

        _ ->
            Nothing



-- MINER INITIALIZATION (Java algorithm)


createInitialMiners : Int -> Int -> Random.Seed -> List Miner
createInitialMiners levelWidth numMiners seed =
    let
        -- Generate miners using Java algorithm (number varies by level)
        ( miners, _ ) =
            List.range 0 (numMiners - 1)
                |> List.foldl (createSingleMiner levelWidth numMiners) ( [], seed )
    in
    List.reverse miners


createSingleMiner : Int -> Int -> Int -> ( List Miner, Random.Seed ) -> ( List Miner, Random.Seed )
createSingleMiner levelWidth totalMiners minerIndex ( miners, seed ) =
    let
        -- Java: miners[i][0] = random.nextInt(88 - 24 - 16) + 24
        -- Safe spawn area on platforms: x = 24 to 47 (48 positions)
        ( baseX, seed1 ) =
            Random.step (Random.int 24 47) seed

        -- Java: if (i < miners.length / 2) miners[i][0] = level_width - miners[i][0]
        -- First half of miners get moved to right side, second half stay on left
        finalX =
            if minerIndex < totalMiners // 2 then
                toFloat (levelWidth - baseX)
                -- Right platform (mirrored)

            else
                toFloat baseX

        -- Left platform
        -- Java: miners[i][1] = -random.nextInt(400)
        -- Start above screen with random height
        ( negativeY, seed2 ) =
            Random.step (Random.int 0 399) seed1

        finalY =
            toFloat -negativeY

        -- Face inward from spawn position
        -- Right spawners (minerIndex < totalMiners // 2) face LEFT, left spawners face RIGHT
        facingLeft =
            if minerIndex < totalMiners // 2 then
                True

            else
                False

        -- Right side faces left, left side faces right
        seed3 =
            seed2

        -- No random direction needed
        miner =
            { x = finalX
            , y = finalY
            , vx = 0.0
            , vy = 0.0
            , animFrame = 0
            , state = Falling -- All start falling from above
            , carryingGold = False
            , targetPlatform = Nothing
            , facingLeft = facingLeft
            , jumpVelocity = 0 -- Start falling (Java: miners[i][4] = 0)
            , fallDistance = -640 -- Java: miners[i][7] = -640 (start high above)
            }
    in
    ( miner :: miners, seed3 )



-- INIT


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        -- Parse level from URL, default to level 0 if not specified or invalid
        urlLevel =
            parseLevelFromUrl url |> Maybe.withDefault 0

        -- Clamp to valid level range (0-6)
        currentLevel =
            max 0 (min 6 urlLevel)

        -- Parse gold from URL for testing purposes
        urlGold =
            parseGoldFromUrl url |> Maybe.withDefault 0

        -- Create a placeholder model with default seed
        placeholderSeed =
            Random.initialSeed 12345

        levelConfig =
            getLevelConfig currentLevel

        levelResult =
            createLevel currentLevel placeholderSeed

        model =
            { key = key
            , url = url
            , level = levelResult.level
            , currentLevel = currentLevel
            , levelStarted = False
            , levelWidth = levelResult.levelWidth
            , levelHeight = levelResult.levelHeight
            , cameraX = 0
            , cameraY = 0
            , randomSeed = levelResult.seed
            , miners = createInitialMiners levelResult.levelWidth levelConfig.diggers placeholderSeed
            , mouseX = 0
            , mouseY = 0
            , mouseButton = -1
            , prevMouseX = 0
            , prevMouseY = 0
            , shiftPressed = False

            -- Game state management
            , score = urlGold
            , levelTarget = levelConfig.target
            , totalMiners = levelConfig.diggers
            , gameStartTime = 0
            , currentTime = 0
            , gameState = LevelIntro
            , arrowKeysPressed = { left = False, right = False, up = False, down = False }

            -- Portal system
            , portalMode = False
            , portals = Nothing
            , portalTimeRemaining = 0
            , portalPreview = Nothing
            , dirtPreview = Nothing

            -- Pause system
            , paused = False

            -- Speed control system
            , gameSpeed = 1.5 -- Start at 1.5x speed (faster than original Java)

            -- Config/Cheat panel
            , configPanelExpanded = False
            , configStartingTime = 200 -- 200 seconds default (level 1 time limit)
            , configGoldValue = 0 -- Start with 0 gold by default
            , configNumMiners = 50 -- Default 50 miners for level 1
            , configLevel = 1 -- Default to level 1
            }

        -- Generate fresh seed from current time
        generateFreshSeed =
            Task.perform
                (\posix -> SeedGenerated (Time.posixToMillis posix))
                Time.now
    in
    ( model, Cmd.batch [ generateFreshSeed, preventContextMenu () ] )



-- HELPER FUNCTIONS for pixel-accurate terrain access
-- Java: level[x | y << 10] - get pixel color at position (x, y)


getPixelAt : Int -> Int -> Array.Array Int -> Int
getPixelAt x y levelArray =
    let
        index =
            Bitwise.or x (Bitwise.shiftLeftBy 10 y)

        -- x | y << 10
    in
    Array.get index levelArray |> Maybe.withDefault 0



-- Java: level[x | y << 10] = color - set pixel color at position (x, y)


setPixelAt : Int -> Int -> Int -> Array.Array Int -> Array.Array Int
setPixelAt x y color levelArray =
    let
        index =
            Bitwise.or x (Bitwise.shiftLeftBy 10 y)

        -- x | y << 10
    in
    Array.set index color levelArray



-- Check if pixel at (x, y) is solid terrain (non-zero and not air)


isTerrainAt : Int -> Int -> Array.Array Int -> Bool
isTerrainAt x y levelArray =
    getPixelAt x y levelArray /= 0



-- LEVEL GENERATION (exact Java port)


createLevel : Int -> Random.Seed -> { level : Array.Array Int, levelWidth : Int, levelHeight : Int, seed : Random.Seed }
createLevel currentLevel seed =
    let
        -- Get level configuration (Java-accurate)
        levelConfig =
            getLevelConfig currentLevel

        levelWidth =
            levelConfig.width

        levelHeight =
            levelConfig.height

        -- Generate heightmap using Java subdivision algorithm
        ( heightmap, seed1 ) =
            generateHeightmap seed

        -- Create platforms for gold dumping
        heightmapWithPlatforms =
            createPlatforms levelWidth heightmap

        -- Generate full level data as flat array (matching Java's level[x | y << 10])
        baseLevelArray =
            generateLevelPixels levelWidth levelHeight heightmapWithPlatforms currentLevel seed1

        -- Add gold objects using Java's algorithm
        ( levelWithGold, seed2 ) =
            addGoldObjects baseLevelArray levelWidth levelHeight currentLevel seed1
    in
    { level = levelWithGold
    , levelWidth = levelWidth
    , levelHeight = levelHeight
    , seed = seed2
    }



-- Java: level[0] = 200; level[512] = 200; for (int i = 512; i > 1; i /= 2) ...


generateHeightmap : Random.Seed -> ( Array.Array Int, Random.Seed )
generateHeightmap seed =
    let
        -- Initialize heightmap array exactly like Java
        initialHeightmap =
            Array.repeat 1024 200

        heightmapWithEndpoints =
            initialHeightmap
                |> Array.set 0 200
                |> Array.set 512 200

        -- Perform subdivision with step sizes 512, 256, 128, 64, 32, 16, 8, 4, 2, 1
        ( finalHeightmap, finalSeed ) =
            subdivideHeightmap heightmapWithEndpoints 512 seed
    in
    ( finalHeightmap, finalSeed )


subdivideHeightmap : Array.Array Int -> Int -> Random.Seed -> ( Array.Array Int, Random.Seed )
subdivideHeightmap heightmap step seed =
    if step > 1 then
        let
            ( newHeightmap, newSeed ) =
                subdivideAtStep heightmap step seed
        in
        subdivideHeightmap newHeightmap (step // 2) newSeed

    else
        ( heightmap, seed )



-- Java: for (int p = 0; p < 1024; p += i)


subdivideAtStep : Array.Array Int -> Int -> Random.Seed -> ( Array.Array Int, Random.Seed )
subdivideAtStep heightmap step seed =
    let
        positions =
            List.range 0 ((1024 // step) - 1) |> List.map ((*) step)
    in
    List.foldl (subdivideAtPosition step) ( heightmap, seed ) positions



-- Java: level[p + i / 2] = (d0 + d1) / 2 + (random.nextInt(i) - i / 2) / 4;


subdivideAtPosition : Int -> Int -> ( Array.Array Int, Random.Seed ) -> ( Array.Array Int, Random.Seed )
subdivideAtPosition step p ( heightmap, seed ) =
    let
        d0 =
            Array.get p heightmap |> Maybe.withDefault 200

        d1 =
            Array.get (Bitwise.and (p + step) 1023) heightmap |> Maybe.withDefault 200

        -- Java: (p + i) & 1023
        midPoint =
            p + step // 2

        ( randomValue, newSeed ) =
            Random.step (Random.int 0 (step - 1)) seed

        variation =
            (randomValue - step // 2) // 4

        newHeight =
            (d0 + d1) // 2 + variation

        newHeightmap =
            Array.set midPoint newHeight heightmap
    in
    ( newHeightmap, newSeed )



-- Java: for (int x = 0; x < 88; x++) { level[x] = level[88] - 2; ... }


createPlatforms : Int -> Array.Array Int -> Array.Array Int
createPlatforms levelWidth heightmap =
    let
        -- Create platforms exactly like Java
        leftPlatformHeight =
            Array.get 88 heightmap |> Maybe.withDefault 200 |> (\h -> h - 2)

        rightPlatformHeight =
            Array.get (levelWidth - 88 - 1) heightmap |> Maybe.withDefault 200 |> (\h -> h - 2)

        -- Set left platform (x = 0 to 87)
        withLeftPlatform =
            List.range 0 87
                |> List.foldl (\x acc -> Array.set x leftPlatformHeight acc) heightmap

        -- Set right platform (x = levelWidth - 88 to levelWidth - 1)
        withBothPlatforms =
            List.range (levelWidth - 88) (levelWidth - 1)
                |> List.foldl (\x acc -> Array.set x rightPlatformHeight acc) withLeftPlatform
    in
    withBothPlatforms



-- Java: for (int y = 1; y < level_height; y++) for (int x = 0; x < level_width; x++) { int i = x | y << 10; ... }


generateLevelPixels : Int -> Int -> Array.Array Int -> Int -> Random.Seed -> Array.Array Int
generateLevelPixels levelWidth levelHeight heightmap currentLevel seed =
    let
        -- Create flat array for level data (size = 1024 * 2048 max)
        levelArray =
            Array.repeat (1024 * 2048) 0

        -- Generate terrain pixel by pixel exactly like Java
        terrainArray =
            List.range 1 (levelHeight - 1)
                |> List.foldl (generateRowPixels levelWidth levelHeight heightmap seed) levelArray
    in
    terrainArray


generateRowPixels : Int -> Int -> Array.Array Int -> Random.Seed -> Int -> Array.Array Int -> Array.Array Int
generateRowPixels levelWidth levelHeight heightmap seed y levelArray =
    List.range 0 (levelWidth - 1)
        |> List.foldl (generateSinglePixel levelWidth levelHeight heightmap seed y) levelArray



-- Java: int i = x | y << 10;


generateSinglePixel : Int -> Int -> Array.Array Int -> Random.Seed -> Int -> Int -> Array.Array Int -> Array.Array Int
generateSinglePixel levelWidth levelHeight heightmap seed y x levelArray =
    let
        i =
            Bitwise.or x (Bitwise.shiftLeftBy 10 y)

        -- Java: x | y << 10
        heightAtX =
            Array.get (modBy 1024 x) heightmap |> Maybe.withDefault 200

        -- Java brightness calculation
        ( randomValue, _ ) =
            Random.step (Random.float 0 1) seed

        br =
            1.2 - (randomValue - 0.5) * randomValue * randomValue * 0.6

        br2 =
            br * (1 - toFloat i / (1024 * 6048.0))

        pixelColor =
            if x < 8 || x >= levelWidth - 8 || y >= levelHeight - 8 then
                -- Rock on the edges
                let
                    c =
                        round (180.0 * br2)
                in
                Bitwise.or (Bitwise.shiftLeftBy 16 c) (Bitwise.or (Bitwise.shiftLeftBy 8 c) c)

            else if y < heightAtX then
                -- Nothing on top of the level (air)
                0

            else
                -- Dirt and grass
                let
                    r =
                        round (111.0 * br2)

                    g =
                        round (92.0 * br2)

                    b =
                        round (51.0 * br2)

                    -- Grass if it's the top four pixels
                    ( finalR, finalG, finalB ) =
                        if y < heightAtX + 4 then
                            let
                                grassR =
                                    round (44.0 * br2)

                                grassG =
                                    round (148.0 * br2)

                                grassB =
                                    round (49.0 * br2)
                            in
                            -- Gray platforms on left/right
                            if x < 88 || x > levelWidth - 89 then
                                ( grassG, grassG, grassG )

                            else
                                ( grassR, grassG, grassB )

                        else
                            ( r, g, b )
                in
                Bitwise.or (Bitwise.shiftLeftBy 16 finalR) (Bitwise.or (Bitwise.shiftLeftBy 8 finalG) finalB)
    in
    Array.set i pixelColor levelArray


generatePixel : Int -> Int -> List Int -> Random.Seed -> Int -> Int
generatePixel width height heightmap seed index =
    let
        x =
            modBy width index

        y =
            index // width

        heightAtX =
            List.drop x heightmap |> List.head |> Maybe.withDefault 200

        -- Add some brightness variation like in Java
        ( randomBrightness, _ ) =
            Random.step (Random.float 0 1) seed

        brightness =
            1.2 - (randomBrightness - 0.5) * randomBrightness * randomBrightness * 0.6

        depthFactor =
            1.0 - toFloat index / (1024.0 * 6048.0)

        finalBrightness =
            brightness * depthFactor
    in
    if x < 8 || x >= width - 8 || y >= height - 8 then
        -- Rock on the edges
        let
            grayValue =
                round (180.0 * finalBrightness)
        in
        Bitwise.or (Bitwise.shiftLeftBy 16 grayValue) (Bitwise.or (Bitwise.shiftLeftBy 8 grayValue) grayValue)

    else if y < heightAtX then
        0
        -- Empty space above ground

    else if y < heightAtX + 4 then
        -- Grass (green)
        let
            r =
                round (44.0 * finalBrightness)

            g =
                round (148.0 * finalBrightness)

            b =
                round (49.0 * finalBrightness)

            -- Make platforms gray instead of green
            ( finalR, finalG, finalB ) =
                if x < 88 || x > width - 89 then
                    ( g, g, g )
                    -- Gray platforms

                else
                    ( r, g, b )

            -- Normal grass
        in
        Bitwise.or (Bitwise.shiftLeftBy 16 finalR) (Bitwise.or (Bitwise.shiftLeftBy 8 finalG) finalB)

    else
        -- Dirt (brown)
        let
            r =
                round (111.0 * finalBrightness)

            g =
                round (92.0 * finalBrightness)

            b =
                round (51.0 * finalBrightness)
        in
        Bitwise.or (Bitwise.shiftLeftBy 16 r) (Bitwise.or (Bitwise.shiftLeftBy 8 g) b)



-- GOLD OBJECT GENERATION (Java algorithm)


addGoldObjects : Array.Array Int -> Int -> Int -> Int -> Random.Seed -> ( Array.Array Int, Random.Seed )
addGoldObjects levelArray levelWidth levelHeight currentLevel seed =
    let
        -- Use level configuration for accurate counts
        levelConfig =
            getLevelConfig currentLevel

        totalObjects =
            levelConfig.goldLumps + levelConfig.rocks + levelConfig.slimes

        -- Generate all objects: gold lumps, rocks, and slime (Java lines 265-310)
        ( levelWithObjects, finalSeed ) =
            List.range 0 (totalObjects - 1)
                |> List.foldl (addSingleObject levelWidth levelHeight levelConfig) ( levelArray, seed )
    in
    ( levelWithObjects, finalSeed )



-- Java algorithm: determines object type by index and places appropriate object


addSingleObject : Int -> Int -> LevelConfig -> Int -> ( Array.Array Int, Random.Seed ) -> ( Array.Array Int, Random.Seed )
addSingleObject levelWidth levelHeight levelConfig objectIndex ( levelArray, seed ) =
    let
        -- Java: Determine type by index (lines 275-286)
        objectType =
            if objectIndex < levelConfig.goldLumps then
                0
                -- Gold

            else if objectIndex < levelConfig.goldLumps + levelConfig.rocks then
                1
                -- Rock

            else
                2

        -- Slime
        -- Java position selection: favor deeper positions (lines 268-273)
        -- Java: int x = random.nextInt(level_width - 40) + 20
        ( x, seed1 ) =
            Random.step (Random.int 20 (levelWidth - 20)) seed

        -- Java: Three y picks with depth bias - random.nextInt(level_height - 240) + 200
        ( y0Raw, seed2 ) =
            Random.step (Random.int 0 (levelHeight - 240)) seed1

        ( y1Raw, seed3 ) =
            Random.step (Random.int 0 (levelHeight - 240)) seed2

        ( y2Raw, seed4 ) =
            Random.step (Random.int 0 (levelHeight - 240)) seed3

        y0 =
            y0Raw + 200

        y1 =
            y1Raw + 200

        y2 =
            y2Raw + 200

        y =
            max y0 (max y1 y2)

        -- Take deepest position
        -- Java size determination (lines 277-287)
        ( size, seed5 ) =
            if objectType == 0 then
                -- Gold: size 4-11
                Random.step (Random.int 4 12) seed4

            else if objectType == 1 then
                -- Rock: size 8-39
                Random.step (Random.int 8 40) seed4

            else
                -- Slime: fixed size 6
                ( 6, seed4 )

        -- Draw object at this position
        levelWithObject =
            drawObjectBlob levelArray levelWidth levelHeight x y size objectType seed5
    in
    ( levelWithObject, seed5 )


addSingleGoldObject : Int -> Int -> Int -> ( Array.Array Int, Random.Seed ) -> ( Array.Array Int, Random.Seed )
addSingleGoldObject levelWidth levelHeight objectIndex ( levelArray, seed ) =
    let
        -- Java: int x = random.nextInt(level_width - 40) + 20
        ( x, seed1 ) =
            Random.step (Random.int 20 (levelWidth - 20)) seed

        -- Java: Three random y positions, take the maximum (deepest)
        -- Java: random.nextInt(level_height - 240) + 200
        ( y0Raw, seed2 ) =
            Random.step (Random.int 0 (levelHeight - 240)) seed1

        ( y1Raw, seed3 ) =
            Random.step (Random.int 0 (levelHeight - 240)) seed2

        ( y2Raw, seed4 ) =
            Random.step (Random.int 0 (levelHeight - 240)) seed3

        y0 =
            y0Raw + 200

        y1 =
            y1Raw + 200

        y2 =
            y2Raw + 200

        y =
            max y0 (max y1 y2)

        -- Java: int size = random.nextInt(8) + 4 (size 4-11)
        ( size, seed5 ) =
            Random.step (Random.int 4 12) seed4

        -- 4 to 11 inclusive
        -- Draw gold blob at this position
        levelWithGold =
            drawGoldBlob levelArray levelWidth levelHeight x y size
    in
    ( levelWithGold, seed5 )



-- Java blob drawing algorithm: draw circular objects (gold, rocks, slime)


drawObjectBlob : Array.Array Int -> Int -> Int -> Int -> Int -> Int -> Int -> Random.Seed -> Array.Array Int
drawObjectBlob levelArray levelWidth levelHeight centerX centerY size objectType seed =
    let
        -- Create list of positions to check within blob radius
        positions =
            List.range (centerX - size) (centerX + size)
                |> List.concatMap
                    (\x ->
                        List.range (centerY - size) (centerY + size)
                            |> List.map (\y -> ( x, y ))
                    )

        -- Filter to only positions within circular radius and level bounds
        validPositions =
            positions
                |> List.filter
                    (\( x, y ) ->
                        let
                            dx =
                                x - centerX

                            dy =
                                y - centerY

                            distanceSquared =
                                dx * dx + dy * dy
                        in
                        -- Java: xx >= 0 && yy >= 0 && xx < 1024 && yy < 2048 && d < size * size
                        distanceSquared
                            < size
                            * size
                            && x
                            >= 0
                            && x
                            < 1024
                            && y
                            >= 0
                            && y
                            < 2048
                    )
    in
    -- Draw object pixels only on existing terrain (Java: if (level[xx | yy << 10] != 0))
    List.foldl (setObjectPixel levelWidth objectType size centerX centerY seed) levelArray validPositions



-- Java blob drawing algorithm: draw circular gold blobs


drawGoldBlob : Array.Array Int -> Int -> Int -> Int -> Int -> Int -> Array.Array Int
drawGoldBlob levelArray levelWidth levelHeight centerX centerY size =
    let
        -- Create list of positions to check within blob radius
        positions =
            List.range (centerX - size) (centerX + size)
                |> List.concatMap
                    (\x ->
                        List.range (centerY - size) (centerY + size)
                            |> List.map (\y -> ( x, y ))
                    )

        -- Filter to only positions within circular radius and level bounds
        validPositions =
            positions
                |> List.filter
                    (\( x, y ) ->
                        let
                            dx =
                                x - centerX

                            dy =
                                y - centerY

                            distanceSquared =
                                dx * dx + dy * dy
                        in
                        -- Java: xx >= 0 && yy >= 0 && xx < 1024 && yy < 2048 && d < size * size
                        distanceSquared
                            < size
                            * size
                            && x
                            >= 0
                            && x
                            < 1024
                            && y
                            >= 0
                            && y
                            < 2048
                    )
    in
    -- Draw gold pixels only on existing terrain (not empty space)
    List.foldl (setGoldPixel levelWidth) levelArray validPositions



-- Java object pixel setting with type-specific colors (lines 296-307)


setObjectPixel : Int -> Int -> Int -> Int -> Int -> Random.Seed -> ( Int, Int ) -> Array.Array Int -> Array.Array Int
setObjectPixel levelWidth objectType size centerX centerY seed ( x, y ) levelArray =
    let
        -- Check if there's already terrain at this position
        currentPixel =
            getPixelAt x y levelArray
    in
    -- Only place object on existing solid terrain (Java: if (level[xx | yy << 10] != 0))
    if currentPixel /= 0 then
        let
            objectColor =
                if objectType == 1 then
                    -- Rocks: gradient based on distance from light source (Java lines 298-301)
                    let
                        dx =
                            toFloat (x - centerX) + (toFloat size / 3.0)

                        dy =
                            toFloat (y - centerY) + (toFloat size / 3.0)

                        d2 =
                            round ((dx * dx / (toFloat size * toFloat size) + dy * dy / (toFloat size * toFloat size)) * 64.0)

                        ( randomVal, _ ) =
                            Random.step (Random.int 0 15) seed

                        c =
                            200 - d2 - randomVal

                        grayValue =
                            max 0 (min 255 c)
                    in
                    Bitwise.or (Bitwise.shiftLeftBy 16 grayValue) (Bitwise.or (Bitwise.shiftLeftBy 8 grayValue) grayValue)

                else
                -- Gold and slime: monocolored (Java line 306)
                -- Gold (type 0): 0xffff00, Slime (type 2): 0x00ff00
                -- Java: (type * 128 - type / 2) << 16 ^ 0xffff00
                if
                    objectType == 0
                then
                    0x00FFFF00
                    -- Gold

                else
                    0xFF00

            -- Slime
        in
        setPixelAt x y objectColor levelArray

    else
        levelArray


setGoldPixel : Int -> ( Int, Int ) -> Array.Array Int -> Array.Array Int
setGoldPixel levelWidth ( x, y ) levelArray =
    let
        -- Check if there's already terrain at this position
        currentPixel =
            getPixelAt x y levelArray
    in
    -- Only place gold on existing solid terrain (not air/empty)
    if currentPixel /= 0 then
        setPixelAt x y 0x00FFFF00 levelArray
        -- Gold color

    else
        levelArray



-- MINER SPRITES (converted from Java ASCII art)


type alias Sprite =
    List String



-- Sprite data: 7 characters wide, 10 lines tall
-- Characters: ' ' = transparent, '!' = hair, 'o' = skin, '*' = clothes, 'X' = gold, '#' = blood


minerSprites : Array.Array Sprite
minerSprites =
    Array.fromList
        [ -- Frame 0: Standing
          [ " !!!!  "
          , " !oooo "
          , " !oooo "
          , "  **   "
          , "  **XX "
          , "  o*XX "
          , "  **   "
          , "  **   "
          , "  **   "
          , "  ooo  "
          ]
        , -- Frame 1: Walking 1
          [ "  !    "
          , " !!!!  "
          , " !oooo "
          , "  oooo "
          , "  **XXX"
          , "  *oXXX"
          , "  **** "
          , " ** ** "
          , "**   oo"
          , "oo     "
          ]
        , -- Frame 2: Walking 2
          [ "  !!!  "
          , " !oooo "
          , "!!oooo "
          , "  ** XX"
          , "  ***oX"
          , "  ** XX"
          , "  ***  "
          , " ***** "
          , "o** ** "
          , "o    oo"
          ]
        , -- Frame 3: Walking 3
          [ " !!!!  "
          , "!!oooo "
          , " !oooo "
          , "  **   "
          , "  **XXX"
          , "  *oXXX"
          , "  **   "
          , " o***  "
          , "  o**  "
          , "    oo "
          ]
        , -- Frame 4: Death 1
          [ "       "
          , "       "
          , " !!!!  "
          , " !oooo "
          , " !oooo "
          , "  **   "
          , "  **XX "
          , "  o*XX "
          , " #*# # "
          , "  ###  "
          ]
        , -- Frame 5: Death 2
          [ "       "
          , "       "
          , "       "
          , "       "
          , "       "
          , "   #   "
          , "#!!!! #"
          , " !oooo "
          , " #o#o# "
          , " ##### "
          ]
        , -- Frame 6: Death 3
          [ "       "
          , "       "
          , "       "
          , "       "
          , "       "
          , "       "
          , "       "
          , "       "
          , "   #   "
          , "#     #"
          ]
        ]



-- Color mapping for sprite characters


spriteCharToColor : Char -> Bool -> Int
spriteCharToColor char carryingGold =
    case char of
        ' ' ->
            -1

        -- Transparent
        '!' ->
            0xFF00

        -- Hair (green)
        'o' ->
            0x00FFB6C1

        -- Skin (light pink)
        '*' ->
            0xFF

        -- Clothes (blue)
        'X' ->
            if carryingGold then
                0x00FFFF00

            else
                0xFF

        -- Gold (yellow) or blue clothes
        '#' ->
            0x00FF0000

        -- Blood (red)
        _ ->
            0x00FF00FF



-- Debug magenta for unknown chars
-- PERFORMANCE OPTIMIZED RENDERING
-- VIEW WITH SIMPLE CANVAS AND SIDE PANELS


view : Model -> Html Msg
view model =
    div
        [ style "margin" "0"
        , style "padding" "20px"
        , style "background-color" "#1a1a1a"
        , style "color" "#ffffff"
        , style "font-family" "Arial, sans-serif"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "min-height" "100vh"
        ]
        [ div
            [ style "display" "flex"
            , style "gap" "20px"
            , style "align-items" "flex-start"
            , style "max-width" "1200px"
            ]
            [ -- Left tribute panel
              div
                [ style "width" "250px"
                , style "padding" "20px"
                , style "background-color" "#2a2a2a"
                , style "border-radius" "8px"
                , style "line-height" "1.5"
                , style "font-size" "14px"
                , style "color" "#cccccc"
                ]
                [ p []
                    [ text "A demo recreation of "
                    , Html.a [ Attr.href "https://x.com/notch", Attr.target "_blank", style "color" "#4a9eff", style "text-decoration" "none" ] [ text "@notch" ]
                    , text "'s amazing "
                    , strong [] [ text "Miners4K" ]
                    , text " ("
                    , Html.a [ Attr.href "https://jvm-gaming.org/t/miners4k/25427", Attr.target "_blank", style "color" "#4a9eff", style "text-decoration" "none" ] [ text "2005" ]
                    , text ") in "
                    , Html.a [ Attr.href "https://elm-lang.org/", Attr.target "_blank", style "color" "#4a9eff", style "text-decoration" "none" ] [ text "Elm" ]
                    , text "! Such clever game design packed into 4KB. Full credit to the original. Just wanted to preserve this gem in a modern browser format 🎮"
                    ]
                , p [] [ text "I added Portals 🌀 and Time Stop ⏳ mechanics!" ]
                , p []
                    [ text "An agentic coding experiment using "
                    , Html.a [ Attr.href "https://ampcode.com", Attr.target "_blank", style "color" "#4a9eff", style "text-decoration" "none" ] [ text "Amp" ]
                    , text "."
                    ]
                , p []
                    [ text "- "
                    , Html.a [ Attr.href "https://x.com/rvtond", Attr.target "_blank", style "color" "#4a9eff", style "text-decoration" "none" ] [ text "@rvtond" ]
                    ]
                , p []
                    [ Html.a [ Attr.href "https://github.com/rvantonder/Miners4k-elm", Attr.target "_blank", style "color" "#4a9eff", style "text-decoration" "none" ] [ text "🌐 GitHub" ]
                    ]
                , -- Config/Cheats panel
                  div
                    [ style "margin-top" "16px"
                    , style "padding-top" "16px"
                    , style "border-top" "1px solid #444"
                    ]
                    [ div
                        [ style "cursor" "pointer"
                        , style "margin-bottom" "8px"
                        , style "font-weight" "bold"
                        , style "color" "#ffffff"
                        , style "display" "flex"
                        , style "align-items" "center"
                        , style "gap" "8px"
                        , Html.Events.onClick ToggleConfigPanel
                        ]
                        [ text
                            (if model.configPanelExpanded then
                                "▼ "

                             else
                                "▶ "
                            )
                        , text "Config / Cheats"
                        ]
                    , if model.configPanelExpanded then
                        div
                            [ style "margin-top" "12px"
                            , style "padding" "12px"
                            , style "background-color" "#1e1e1e"
                            , style "border-radius" "4px"
                            , style "border" "1px solid #444"
                            ]
                            [ -- Game Speed
                              div [ style "margin-bottom" "16px" ]
                                [ div
                                    [ style "margin-bottom" "4px"
                                    , style "font-size" "12px"
                                    , style "color" "#aaa"
                                    ]
                                    [ text ("Game Speed: " ++ String.fromFloat (round (model.gameSpeed * 10) |> toFloat |> (\x -> x / 10)) ++ "x") ]
                                , input
                                    [ type_ "range"
                                    , Attr.min "0.1"
                                    , Attr.max "5.0"
                                    , step "0.1"
                                    , value (String.fromFloat model.gameSpeed)
                                    , style "width" "100%"
                                    , Html.Events.onInput (\s -> SetGameSpeed (String.toFloat s |> Maybe.withDefault 1.0))
                                    ]
                                    []
                                ]

                            -- Starting Time
                            , div [ style "margin-bottom" "12px" ]
                                [ label
                                    [ style "display" "block"
                                    , style "font-size" "12px"
                                    , style "color" "#aaa"
                                    , style "margin-bottom" "4px"
                                    ]
                                    [ text "Starting Time (seconds)" ]
                                , input
                                    [ type_ "number"
                                    , Attr.min "0"
                                    , Attr.max "3600"
                                    , value (String.fromInt model.configStartingTime)
                                    , style "width" "100%"
                                    , style "padding" "4px"
                                    , style "background-color" "#333"
                                    , style "border" "1px solid #555"
                                    , style "color" "#fff"
                                    , style "border-radius" "3px"
                                    , Html.Events.onInput (\s -> SetConfigStartingTime (String.toInt s |> Maybe.withDefault 180))
                                    ]
                                    []
                                ]

                            -- Gold Value
                            , div [ style "margin-bottom" "12px" ]
                                [ label
                                    [ style "display" "block"
                                    , style "font-size" "12px"
                                    , style "color" "#aaa"
                                    , style "margin-bottom" "4px"
                                    ]
                                    [ text "Starting Gold" ]
                                , input
                                    [ type_ "number"
                                    , Attr.min "0"
                                    , Attr.max "9999"
                                    , value (String.fromInt model.configGoldValue)
                                    , style "width" "100%"
                                    , style "padding" "4px"
                                    , style "background-color" "#333"
                                    , style "border" "1px solid #555"
                                    , style "color" "#fff"
                                    , style "border-radius" "3px"
                                    , Html.Events.onInput (\s -> SetConfigGoldValue (String.toInt s |> Maybe.withDefault 0))
                                    ]
                                    []
                                ]

                            -- Number of Miners
                            , div [ style "margin-bottom" "12px" ]
                                [ label
                                    [ style "display" "block"
                                    , style "font-size" "12px"
                                    , style "color" "#aaa"
                                    , style "margin-bottom" "4px"
                                    ]
                                    [ text "Number of Miners" ]
                                , input
                                    [ type_ "number"
                                    , Attr.min "1"
                                    , Attr.max "50"
                                    , value (String.fromInt model.configNumMiners)
                                    , style "width" "100%"
                                    , style "padding" "4px"
                                    , style "background-color" "#333"
                                    , style "border" "1px solid #555"
                                    , style "color" "#fff"
                                    , style "border-radius" "3px"
                                    , Html.Events.onInput (\s -> SetConfigNumMiners (String.toInt s |> Maybe.withDefault 10))
                                    ]
                                    []
                                ]

                            -- Level
                            , div [ style "margin-bottom" "0px" ]
                                [ label
                                    [ style "display" "block"
                                    , style "font-size" "12px"
                                    , style "color" "#aaa"
                                    , style "margin-bottom" "4px"
                                    ]
                                    [ text "Level" ]
                                , input
                                    [ type_ "number"
                                    , Attr.min "1"
                                    , Attr.max "7"
                                    , value (String.fromInt model.configLevel)
                                    , style "width" "100%"
                                    , style "padding" "4px"
                                    , style "background-color" "#333"
                                    , style "border" "1px solid #555"
                                    , style "color" "#fff"
                                    , style "border-radius" "3px"
                                    , Html.Events.onInput (\s -> SetConfigLevel (String.toInt s |> Maybe.withDefault 1))
                                    ]
                                    []
                                ]

                            -- Start New button
                            , div [ style "margin-top" "16px" ]
                                [ button
                                    [ Html.Events.onClick StartNewWithConfig
                                    , style "width" "100%"
                                    , style "padding" "8px"
                                    , style "background-color" "#4CAF50"
                                    , style "border" "none"
                                    , style "color" "white"
                                    , style "border-radius" "4px"
                                    , style "cursor" "pointer"
                                    , style "font-size" "14px"
                                    , style "font-weight" "bold"
                                    ]
                                    [ text "Start New Game" ]
                                ]
                            ]

                      else
                        text ""
                    ]
                ]
            , -- Game area
              div
                [ style "flex-shrink" "0"
                , style "width" "640px"
                , style "max-width" "640px"
                , style "position" "relative"
                ]
                [ canvas
                    [ id "gameCanvas"
                    , width gameWidth
                    , height gameHeight
                    , style "border" "1px solid black"
                    , style "display" "block"
                    , style "image-rendering" "pixelated"
                    , style "cursor" "none"
                    , style "max-width" "100%"
                    , style "height" "auto"
                    , Html.Events.on "mousemove" mouseDecoder
                    , Html.Events.preventDefaultOn "mousedown" mouseDownDecoderWithPrevent
                    , Html.Events.preventDefaultOn "mouseup" mouseUpDecoderWithPrevent
                    ]
                    []

                -- Java-accurate game UI overlay
                , viewGameUI model
                ]
            , -- Right controls panel
              div
                [ style "width" "250px"
                , style "padding" "20px"
                , style "background-color" "#2a2a2a"
                , style "border-radius" "8px"
                , style "line-height" "1.5"
                , style "font-size" "14px"
                ]
                [ h3
                    [ style "margin-top" "0"
                    , style "color" "#ffffff"
                    , style "border-bottom" "1px solid #444"
                    , style "padding-bottom" "8px"
                    ]
                    [ text "Game Controls" ]
                , ul
                    [ style "margin" "0"
                    , style "padding-left" "20px"
                    ]
                    [ li [ style "margin-bottom" "8px" ]
                        [ strong [] [ text "Left Mouse Button:" ]
                        , text " Drag to build dirt (7×7 brush)"
                        ]
                    , li [ style "margin-bottom" "8px" ]
                        [ strong [] [ text "Right Mouse Button:" ]
                        , text " Drag to dig terrain"
                        ]
                    , li [ style "margin-bottom" "8px" ]
                        [ strong [] [ text "Arrow Keys:" ]
                        , text " Scroll camera around level"
                        ]
                    , li [ style "margin-bottom" "8px" ]
                        [ strong [] [ text "P Key:" ]
                        , text " Portal mode (costs 20 gold)"
                        ]
                    , li [ style "margin-bottom" "8px" ]
                        [ strong [] [ text "Space Key:" ]
                        , text " Time stop (costs 80 gold, digging still works)"
                        ]
                    ]
                , h3
                    [ style "margin-top" "0"
                    , style "color" "#ffffff"
                    , style "border-bottom" "1px solid #444"
                    , style "padding-bottom" "8px"
                    ]
                    [ text "Objective" ]
                , p []
                    [ text "Help miners collect gold and deliver it to the gray platforms. Levels are "
                    , strong [] [ Html.i [] [ text "incomplete" ] ]
                    , text ". Demo works well for first 2 levels."
                    ]
                ]
            ]
        ]



-- Game UI overlay (matches Java rendering)


viewGameUI : Model -> Html Msg
viewGameUI model =
    let
        levelConfig =
            getLevelConfig model.currentLevel

        -- Calculate actual time left (Java lines 703-706)
        timeLeft =
            if model.gameStartTime == 0 then
                levelConfig.timeLimit

            else
                let
                    elapsed =
                        (model.currentTime - model.gameStartTime) // 1000

                    -- Convert to seconds
                in
                max 0 (levelConfig.timeLimit - elapsed)

        -- Use the portal time remaining counter
        portalTimeRemaining =
            model.portalTimeRemaining
    in
    div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "width" "100%"
        , style "height" "100%"
        , style "pointer-events" "none"
        , style "color" "white"
        , style "font-family" "Arial, sans-serif"
        ]
        [ case model.gameState of
            LevelIntro ->
                viewLevelIntro model

            Playing ->
                viewPlayingUI model timeLeft portalTimeRemaining

            LevelComplete ->
                viewLevelComplete model

            LevelFailed ->
                viewLevelFailed model

            GameComplete ->
                viewGameComplete model
        ]



-- Java level intro screen (lines 687-699)


viewLevelIntro : Model -> Html Msg
viewLevelIntro model =
    let
        levelName =
            Array.get model.currentLevel levelNames |> Maybe.withDefault "Unknown Level"

        levelInfo =
            Array.get model.currentLevel levelInfoStrings |> Maybe.withDefault ""
    in
    div
        [ style "position" "absolute"
        , style "top" "100px"
        , style "left" "200px"
        , style "pointer-events" "auto"
        ]
        [ div
            [ style "font-size" "30px"
            , style "font-weight" "bold"
            , style "margin-bottom" "16px"
            ]
            [ text levelName ]
        , div
            [ style "font-size" "12px"
            , style "margin-bottom" "16px"
            ]
            [ text levelInfo ]
        , div
            [ style "font-size" "12px" ]
            [ text "(click to start)" ]
        ]



-- Java playing UI (lines 703-706)


viewPlayingUI : Model -> Int -> Int -> Html Msg
viewPlayingUI model timeLeft portalTimeRemaining =
    div []
        [ -- Regular game UI in top-left corner
          div
            [ style "position" "absolute"
            , style "top" "10px"
            , style "left" "10px"
            , style "background" "rgba(0,0,0,0.7)"
            , style "padding" "5px"
            , style "font-size" "12px"
            ]
            [ div [] [ text ("Miners: " ++ String.fromInt model.totalMiners) ]
            , div [] [ text ("Gold: " ++ String.fromInt model.score ++ " / " ++ String.fromInt model.levelTarget) ]
            , div [] [ text ("Time left: " ++ String.fromInt timeLeft) ]
            ]

        -- Show portal and time stop status in top-right corner
        , div
            [ style "position" "absolute"
            , style "top" "10px"
            , style "right" "10px"
            , style "background" "rgba(0,0,0,0.7)"
            , style "padding" "5px"
            , style "font-size" "12px"
            , style "text-align" "right"
            ]
            [ -- Portal status
              case model.portals of
                Just ( _, Just _ ) ->
                    -- Both portals are active - show countdown timer
                    let
                        timerColor =
                            if model.portalTimeRemaining <= 5 then
                                "#FF0000"

                            else
                                "#FFFF00"
                    in
                    div
                        [ style "color" timerColor ]
                        [ text ("Portals expire in: " ++ String.fromInt model.portalTimeRemaining ++ "s") ]

                _ ->
                    -- No complete portal pair - show normal portal UI
                    if model.score >= 20 then
                        div
                            [ style "color" "#00FF00" ]
                            [ text
                                (if model.portalMode then
                                    "Click to place portal!"

                                 else
                                    "Press P for Portals (20 gold)"
                                )
                            ]

                    else
                        div
                            [ style "color" "#666666" ]
                            [ text ("Portals: " ++ String.fromInt model.score ++ "/20 gold") ]

            -- Time stop status
            , if model.paused then
                div
                    [ style "color" "red" ]
                    [ text "Time stop (paused)" ]

              else if model.score >= 80 then
                div
                    [ style "color" "#00FF00" ]
                    [ text "Press <space> for time stop (80 gold)" ]

              else
                div
                    [ style "color" "#666666" ]
                    [ text ("Time stop: " ++ String.fromInt model.score ++ "/80 gold") ]
            ]
        ]



-- Java level complete screen (lines 710-722)


viewLevelComplete : Model -> Html Msg
viewLevelComplete model =
    div
        [ style "position" "absolute"
        , style "top" "100px"
        , style "left" "200px"
        , style "font-size" "30px"
        , style "font-weight" "bold"
        , style "pointer-events" "auto"
        ]
        [ text "LEVEL PASSED!" ]



-- Java level failed screen (lines 724-733)


viewLevelFailed : Model -> Html Msg
viewLevelFailed model =
    div
        [ style "position" "absolute"
        , style "top" "100px"
        , style "left" "200px"
        , style "font-size" "30px"
        , style "font-weight" "bold"
        , style "pointer-events" "auto"
        ]
        [ text "LEVEL FAILED!" ]



-- Game complete screen


viewGameComplete : Model -> Html Msg
viewGameComplete model =
    div
        [ style "position" "absolute"
        , style "top" "100px"
        , style "left" "200px"
        , style "font-size" "30px"
        , style "font-weight" "bold"
        , style "pointer-events" "auto"
        ]
        [ text "CONGRATULATIONS!"
        , div [ style "font-size" "16px", style "margin-top" "20px" ]
            [ text "You completed all levels!" ]
        ]



-- Mouse event decoders


mouseDecoder : Decode.Decoder Msg
mouseDecoder =
    Decode.map2 MouseMove
        (Decode.oneOf
            [ Decode.field "offsetX" Decode.int
            , Decode.field "clientX" Decode.int -- Fallback
            ]
        )
        (Decode.oneOf
            [ Decode.field "offsetY" Decode.int
            , Decode.field "clientY" Decode.int -- Fallback
            ]
        )


mouseDownDecoder : Decode.Decoder Msg
mouseDownDecoder =
    Decode.map MouseDown
        (Decode.field "button" Decode.int)


mouseDownDecoderWithPrevent : Decode.Decoder ( Msg, Bool )
mouseDownDecoderWithPrevent =
    Decode.map (\button -> ( MouseDown button, True ))
        (Decode.field "button" Decode.int)


mouseUpDecoderWithPrevent : Decode.Decoder ( Msg, Bool )
mouseUpDecoderWithPrevent =
    Decode.succeed ( MouseUp, True )



-- GOLD DETECTION AND COLLECTION


isGoldPixel : Int -> Bool
isGoldPixel color =
    color == 0x00FFFF00



-- Gold is yellow
-- Get character at sprite position (Java: SPRITES[(xx + 3) + (yy + 8) * 7 + frameIndex * 7 * 10])


getSpriteCharAt : Int -> Int -> Int -> Char
getSpriteCharAt spriteX spriteY frameIndex =
    if spriteX >= 0 && spriteX < 7 && spriteY >= 0 && spriteY < 10 then
        case Array.get frameIndex minerSprites of
            Just sprite ->
                case List.drop spriteY sprite |> List.head of
                    Just row ->
                        String.slice spriteX (spriteX + 1) row
                            |> String.uncons
                            |> Maybe.map Tuple.first
                            |> Maybe.withDefault ' '

                    Nothing ->
                        ' '

            Nothing ->
                ' '

    else
        ' '


checkForGold : Array.Array Int -> Int -> Int -> Int -> Int -> Bool
checkForGold level levelW levelH x y =
    let
        -- Java-accurate: only check pixels where sprite has actual content (lines 619-628)
        checkSpritePixel xx yy =
            let
                spriteX =
                    xx + 3

                -- Convert to sprite coordinate (xx ranges -3 to 3)
                spriteY =
                    yy + 8

                -- Convert to sprite coordinate (yy ranges -8 to 1)
                spriteIndex =
                    0

                -- Use walking frame 0 for gold detection
                spriteChar =
                    getSpriteCharAt spriteX spriteY spriteIndex

                worldX =
                    x + xx

                worldY =
                    y + yy
            in
            -- Only check if sprite has content AND world position is valid
            if spriteChar /= ' ' && worldX >= 0 && worldX < levelW && worldY >= 0 && worldY < levelH then
                let
                    pixel =
                        getPixelAt worldX worldY level
                in
                isGoldPixel pixel

            else
                False

        -- Check all sprite pixel positions (Java loops: xx=-3 to 3, yy=-8 to 1)
        spritePositions =
            List.range -3 3
                |> List.concatMap
                    (\xx ->
                        List.range -8 1
                            |> List.map (\yy -> ( xx, yy ))
                    )

        hasGold =
            List.any (\( xx, yy ) -> checkSpritePixel xx yy) spritePositions
    in
    hasGold


removeGoldAtMiner : Array.Array Int -> Int -> Int -> Int -> Int -> Array.Array Int
removeGoldAtMiner level levelW levelH x y =
    let
        -- Java-accurate: remove gold only where sprite has actual content (matching checkForGold)
        removeSpriteGold xx yy levelArray =
            let
                spriteX =
                    xx + 3

                -- Convert to sprite coordinate
                spriteY =
                    yy + 8

                -- Convert to sprite coordinate
                spriteIndex =
                    0

                -- Use walking frame 0
                spriteChar =
                    getSpriteCharAt spriteX spriteY spriteIndex

                worldX =
                    x + xx

                worldY =
                    y + yy
            in
            -- Only remove if sprite has content AND world position is valid
            if spriteChar /= ' ' && worldX >= 0 && worldX < levelW && worldY >= 0 && worldY < levelH then
                let
                    pixel =
                        getPixelAt worldX worldY levelArray
                in
                if isGoldPixel pixel then
                    setPixelAt worldX worldY 0x00 levelArray
                    -- Java: set to black (line 657)

                else
                    levelArray

            else
                levelArray

        -- Remove gold at all sprite pixel positions (Java loops: xx=-3 to 3, yy=-8 to 1)
        spritePositions =
            List.range -3 3
                |> List.concatMap
                    (\xx ->
                        List.range -8 1
                            |> List.map (\yy -> ( xx, yy ))
                    )
    in
    List.foldl (\( xx, yy ) acc -> removeSpriteGold xx yy acc) level spritePositions


isOnPlatform : Int -> Int -> Bool
isOnPlatform levelW x =
    -- Java: (miners[i][0] == 8 + 32 || miners[i][0] == level_width - 8 - 32)
    -- Left platform drop point: x = 40, Right platform drop point: x = levelW - 40
    -- Must be exactly at these positions to drop gold
    x == 40 || x == (levelW - 40)



-- Java: miners[i][1] <= level[miners[i][0]] (check if miner is above ground level)


isAboveGround : Array.Array Int -> Int -> Int -> Int -> Int -> Bool
isAboveGround level levelW levelH x y =
    if x >= 0 && x < levelW && x < 1024 then
        let
            groundHeight =
                getGroundLevel level levelW levelH x
        in
        y <= groundHeight

    else
        False



-- Java gold stacking physics: drop 4 pixels that fall diagonally


dropGoldOnPlatform : Array.Array Int -> Int -> Int -> Int -> Int -> Array.Array Int
dropGoldOnPlatform level levelW levelH minerX minerY =
    let
        -- Drop 4 gold pixels starting from miner position
        dropPositions =
            List.range 0 3

        finalLevel =
            List.foldl (dropSingleGoldPixel levelW levelH minerX minerY) level dropPositions
    in
    finalLevel


dropSingleGoldPixel : Int -> Int -> Int -> Int -> Int -> Array.Array Int -> Array.Array Int
dropSingleGoldPixel levelW levelH minerX minerY pixelIndex level =
    let
        -- Start position: miner position minus 5 in Y (Java: yy = miners[i][1] - 5)
        startX =
            minerX

        startY =
            minerY - 5

        -- Find final position using diagonal falling physics
        finalPosition =
            simulateGoldFall level levelW levelH startX startY

        -- Place gold pixel at final position (Java color: 0xfefe00, not 0xffff00)
        goldColor =
            0x00FEFE00

        -- Delivered gold color (slightly different from mined gold)
    in
    case finalPosition of
        Just ( x, y ) ->
            if y >= 0 && y < levelH then
                setPixelAt x y goldColor level

            else
                level

        Nothing ->
            level



-- Simulate Java gold falling physics


simulateGoldFall : Array.Array Int -> Int -> Int -> Int -> Int -> Maybe ( Int, Int )
simulateGoldFall level levelW levelH startX startY =
    let
        -- Java algorithm: try straight down, diagonal left, diagonal right
        fallStep : Int -> Int -> Maybe ( Int, Int )
        fallStep x y =
            if y >= levelH - 1 then
                Just ( x, y )
                -- Hit bottom

            else if getPixelAt x (y + 1) level /= 0 then
                -- Can't fall straight down, try diagonally
                if x > 0 && getPixelAt (x - 1) (y + 1) level == 0 then
                    fallStep (x - 1) (y + 1)
                    -- Fall diagonally left

                else if x < levelW - 1 && getPixelAt (x + 1) (y + 1) level == 0 then
                    fallStep (x + 1) (y + 1)
                    -- Fall diagonally right

                else
                    Just ( x, y )
                -- Can't fall anymore

            else
                fallStep x (y + 1)

        -- Fall straight down
    in
    if startX >= 0 && startX < levelW && startY >= 0 then
        fallStep startX startY

    else
        Nothing


getPlatformTarget : Int -> Int -> String
getPlatformTarget levelW x =
    -- Determine which platform is closer
    let
        leftDistance =
            x

        rightDistance =
            levelW - x
    in
    if leftDistance <= rightDistance then
        "left"

    else
        "right"



-- MINER PHYSICS AND AI


checkMinerCollision : Float -> Float -> Array.Array Int -> Int -> Int -> Bool
checkMinerCollision x y level levelWidth levelHeight =
    let
        minerX =
            round x

        minerY =
            round y

        -- Check if miner would overlap with solid terrain
        -- Miner is 7 pixels wide, 10 pixels tall
        terrainBlocked =
            List.range 0 6
                |> List.any
                    (\dx ->
                        List.range 0 9
                            |> List.any
                                (\dy ->
                                    let
                                        checkX =
                                            minerX + dx

                                        checkY =
                                            minerY + dy
                                    in
                                    checkX
                                        >= 0
                                        && checkX
                                        < levelWidth
                                        && checkY
                                        >= 0
                                        && checkY
                                        < levelHeight
                                        && isPixelSolid level levelWidth levelHeight checkY checkX
                                )
                    )
    in
    terrainBlocked


checkPortalTeleportation : Miner -> Maybe ( Portal, Maybe Portal ) -> Array.Array Int -> Int -> Int -> Miner
checkPortalTeleportation miner maybePortals level levelWidth levelHeight =
    case maybePortals of
        Nothing ->
            miner

        Just ( p1, maybeP2 ) ->
            let
                -- Check collision with P1 (generous collision detection)
                minerX =
                    round miner.x

                minerY =
                    round miner.y

                -- Expand portal collision area for easier triggering
                -- Portal is 7x12, but we'll check a larger area around it
                portalBuffer =
                    3

                -- Extra pixels around portal for easier collision
                p1Collision =
                    minerX
                        < p1.x
                        + 7
                        + portalBuffer
                        && minerX
                        + 7
                        > p1.x
                        - portalBuffer
                        && minerY
                        < p1.y
                        + 12
                        + portalBuffer
                        && minerY
                        + 10
                        > p1.y
                        - portalBuffer

                -- Check collision with P2 if it exists (same generous detection)
                p2Collision =
                    case maybeP2 of
                        Nothing ->
                            False

                        Just p2 ->
                            minerX
                                < p2.x
                                + 7
                                + portalBuffer
                                && minerX
                                + 7
                                > p2.x
                                - portalBuffer
                                && minerY
                                < p2.y
                                + 12
                                + portalBuffer
                                && minerY
                                + 10
                                > p2.y
                                - portalBuffer
            in
            if p1Collision && maybeP2 /= Nothing then
                -- Teleport from P1 to P2
                case maybeP2 of
                    Just p2 ->
                        teleportMinerToPortal miner p1 p2 level levelWidth levelHeight

                    Nothing ->
                        miner

            else if p2Collision then
                -- Teleport from P2 to P1
                case maybeP2 of
                    Just p2 ->
                        teleportMinerToPortal miner p2 p1 level levelWidth levelHeight

                    Nothing ->
                        miner

            else
                miner


teleportMinerToPortal : Miner -> Portal -> Portal -> Array.Array Int -> Int -> Int -> Miner
teleportMinerToPortal miner fromPortal toPortal level levelWidth levelHeight =
    let
        -- Determine which side the miner entered from by checking their position relative to portal
        minerX =
            round miner.x

        portalLeftEdge =
            fromPortal.x

        portalRightEdge =
            fromPortal.x + 7

        -- More precise entry detection: check if miner is more on left or right side
        enteredFromLeft =
            minerX < (portalLeftEdge + 2)

        -- If mostly on left side of portal
        -- Calculate exit position: miners should exit AWAY from the destination portal
        -- Need to account for the 3-pixel collision buffer around portals
        -- Portal is 7 wide + 3 buffer = 10 total collision width, so place miners at least 12+ pixels away
        ( exitX, exitY ) =
            if enteredFromLeft then
                -- Entered from left → exit moving right → place to RIGHT of exit portal
                ( toPortal.x + 12, toPortal.y )
                -- +12 to clear the portal + buffer zone

            else
                -- Entered from right → exit moving left → place to LEFT of exit portal
                ( toPortal.x - 12, toPortal.y )

        -- -12 to clear the portal + buffer zone
        -- Check if exit position is blocked
        exitBlocked =
            checkMinerCollision (toFloat exitX) (toFloat exitY) level levelWidth levelHeight

        -- Find alternative position if blocked
        finalPosition =
            if exitBlocked then
                if enteredFromLeft then
                    -- Try further to the right
                    findSafeExitPositionRight (toFloat toPortal.x) (toFloat toPortal.y) level levelWidth levelHeight

                else
                    -- Try further to the left
                    findSafeExitPositionLeft (toFloat toPortal.x) (toFloat toPortal.y) level levelWidth levelHeight

            else
                ( toFloat exitX, toFloat exitY )

        -- Set facing direction to continue moving in exit direction
        newFacingLeft =
            not enteredFromLeft

        -- If entered from left, face right (not left)
    in
    { miner | x = Tuple.first finalPosition, y = Tuple.second finalPosition, facingLeft = newFacingLeft }


findSafeExitPosition : Float -> Float -> Array.Array Int -> Int -> Int -> ( Float, Float )
findSafeExitPosition portalX portalY level levelWidth levelHeight =
    let
        -- Try positions prioritizing sides first, then other positions
        candidatePositions =
            [ ( portalX + 7, portalY ) -- Right (preferred for left→right movement)
            , ( portalX - 7, portalY ) -- Left (preferred for right→left movement)
            , ( portalX + 10, portalY ) -- Further right
            , ( portalX - 10, portalY ) -- Further left
            , ( portalX, portalY - 10 ) -- Above
            , ( portalX, portalY + 12 ) -- Below
            , ( portalX + 7, portalY - 10 ) -- Top-right
            , ( portalX - 7, portalY - 10 ) -- Top-left
            , ( portalX + 7, portalY + 12 ) -- Bottom-right
            , ( portalX - 7, portalY + 12 ) -- Bottom-left
            ]

        -- Find first safe position
        safePosition =
            candidatePositions
                |> List.filter (\( x, y ) -> not (checkMinerCollision x y level levelWidth levelHeight))
                |> List.head
                |> Maybe.withDefault ( portalX + 7, portalY )

        -- Fallback to right side of portal
    in
    safePosition


findSafeExitPositionRight : Float -> Float -> Array.Array Int -> Int -> Int -> ( Float, Float )
findSafeExitPositionRight portalX portalY level levelWidth levelHeight =
    let
        -- Try positions to the right of the portal, progressively further away
        -- Account for 3-pixel buffer zone around portals
        candidatePositions =
            [ ( portalX + 12, portalY ) -- Right side (clear of buffer)
            , ( portalX + 15, portalY ) -- Further right
            , ( portalX + 18, portalY ) -- Even further right
            , ( portalX + 12, portalY - 10 ) -- Right side, above
            , ( portalX + 12, portalY + 12 ) -- Right side, below
            , ( portalX + 20, portalY ) -- Much further right
            ]

        safePosition =
            candidatePositions
                |> List.filter (\( x, y ) -> not (checkMinerCollision x y level levelWidth levelHeight))
                |> List.head
                |> Maybe.withDefault ( portalX + 12, portalY )
    in
    safePosition


findSafeExitPositionLeft : Float -> Float -> Array.Array Int -> Int -> Int -> ( Float, Float )
findSafeExitPositionLeft portalX portalY level levelWidth levelHeight =
    let
        -- Try positions to the left of the portal, progressively further away
        -- Account for 3-pixel buffer zone around portals
        candidatePositions =
            [ ( portalX - 12, portalY ) -- Left side (clear of buffer)
            , ( portalX - 15, portalY ) -- Further left
            , ( portalX - 18, portalY ) -- Even further left
            , ( portalX - 12, portalY - 10 ) -- Left side, above
            , ( portalX - 12, portalY + 12 ) -- Left side, below
            , ( portalX - 20, portalY ) -- Much further left
            ]

        safePosition =
            candidatePositions
                |> List.filter (\( x, y ) -> not (checkMinerCollision x y level levelWidth levelHeight))
                |> List.head
                |> Maybe.withDefault ( portalX - 12, portalY )
    in
    safePosition


updateMiner : Array.Array Int -> Int -> Int -> Miner -> Random.Seed -> Float -> ( Miner, Array.Array Int, Random.Seed )
updateMiner level levelW levelH miner seed gameSpeed =
    case miner.state of
        Dead ->
            ( miner, level, seed )

        Dying frame ->
            if frame < 2 then
                ( { miner | state = Dying (frame + 1) }, level, seed )

            else
                ( { miner | state = Dead }, level, seed )

        _ ->
            let
                currentX =
                    round miner.x

                currentY =
                    round miner.y

                -- Java logic: handle jumping (lines 482-497)
                ( movement, newSeed ) =
                    if miner.jumpVelocity > 1 && currentY > 1 then
                        -- Miner is jumping, check if can move to target position
                        let
                            targetX =
                                currentX
                                    + (if miner.facingLeft then
                                        -1

                                       else
                                        1
                                      )

                            targetY =
                                currentY - (miner.jumpVelocity // 8)
                        in
                        if
                            targetX
                                >= 0
                                && targetX
                                < levelW
                                && targetY
                                >= 0
                                && not (isPixelSolid level levelW levelH targetY targetX)
                        then
                            -- Successful jump move
                            ( { x = toFloat targetX
                              , y = toFloat targetY
                              , jumpVelocity = miner.jumpVelocity - 1
                              , fallDistance = miner.fallDistance
                              , state =
                                    if miner.jumpVelocity <= 2 then
                                        Falling

                                    else
                                        Walking
                              , facingLeft = miner.facingLeft
                              }
                            , seed
                            )

                        else
                            -- Hit something while jumping, stop jumping
                            ( { x = miner.x, y = miner.y, jumpVelocity = 0, fallDistance = miner.fallDistance, state = Falling, facingLeft = miner.facingLeft }, seed )

                    else
                        -- Not jumping, handle ground/falling logic
                        let
                            -- Java: if miner is inside gold, move up one pixel (line 502-503)
                            adjustedY =
                                if
                                    currentY
                                        > 0
                                        && isPixelSolid level levelW levelH currentY currentX
                                        && getPixelAt currentX currentY level
                                        == 0x00FFFF00
                                then
                                    currentY - 1

                                else
                                    currentY

                            -- Java: check if should fall (line 506)
                            shouldFall =
                                adjustedY
                                    < 4
                                    || (adjustedY
                                            >= 0
                                            && adjustedY
                                            < levelH
                                            - 1
                                            && not (isPixelSolid level levelW levelH (adjustedY + 1) currentX)
                                       )
                        in
                        if shouldFall then
                            -- Java: 66% chance to jump if on ground (lines 508-511)
                            if miner.jumpVelocity == -1 then
                                let
                                    ( randomVal, newSeed1 ) =
                                        Random.step (Random.int 0 2) seed
                                in
                                if randomVal /= 0 then
                                    -- Jump! (66% chance)
                                    ( { x = miner.x, y = toFloat adjustedY, jumpVelocity = 16, fallDistance = miner.fallDistance, state = Walking, facingLeft = miner.facingLeft }, newSeed1 )

                                else
                                    -- Fall (33% chance) - lines 516-522
                                    fallTwoPixels level levelW levelH miner.x adjustedY miner.fallDistance miner.facingLeft newSeed1

                            else
                                -- Continue falling
                                fallTwoPixels level levelW levelH miner.x adjustedY miner.fallDistance miner.facingLeft seed

                        else
                            -- On solid ground - handle movement and wall collision
                            handleGroundMovement level levelW levelH miner adjustedY seed

                finalVy =
                    0.0

                -- Java doesn't use continuous velocity
                -- Check for gold collection (Java: happens for all living miners, lines 619-665)
                goldInteraction =
                    if not miner.carryingGold then
                        let
                            hasGold =
                                checkForGold level levelW levelH (round movement.x) (round movement.y)
                        in
                        if hasGold then
                            let
                                levelWithoutGold =
                                    removeGoldAtMiner level levelW levelH (round movement.x) (round movement.y)

                                platform =
                                    getPlatformTarget levelW (round movement.x)
                            in
                            { carryingGold = True, level = levelWithoutGold, targetPlatform = Just platform, justCollected = True, justDropped = False }

                        else
                            { carryingGold = miner.carryingGold, level = level, targetPlatform = miner.targetPlatform, justCollected = False, justDropped = False }

                    else if miner.carryingGold && isOnPlatform levelW (round movement.x) && isAboveGround level levelW levelH (round movement.x) (round movement.y) then
                        -- Drop off gold on platform with proper stacking physics
                        -- Java: miners[i][1] <= level[miners[i][0]] (miner must be above ground at this x position)
                        let
                            levelWithDroppedGold =
                                dropGoldOnPlatform level levelW levelH (round movement.x) (round movement.y)
                        in
                        { carryingGold = False, level = levelWithDroppedGold, targetPlatform = Nothing, justCollected = False, justDropped = True }

                    else
                        { carryingGold = miner.carryingGold, level = level, targetPlatform = miner.targetPlatform, justCollected = False, justDropped = False }

                -- Update animation frame for walking
                newAnimFrame =
                    if movement.state == Walking then
                        modBy 16 (miner.animFrame + 1)
                        -- Java uses 0-15 animation frames

                    else
                        miner.animFrame

                -- Update facing direction based on movement or gold dropping
                newFacingLeft =
                    if goldInteraction.justDropped then
                        not miner.facingLeft
                        -- REQUIREMENT 2: Turn around after dropping gold on platform

                    else
                        movement.facingLeft

                -- Use the facing direction from movement logic (includes probabilistic turning)
                -- Check for slime collision (Java lines 630-648: slime explosion)
                slimeCollision =
                    checkForSlime goldInteraction.level levelW levelH (round movement.x) (round movement.y)

                updatedMiner =
                    if slimeCollision then
                        -- Miner hit slime - start dying animation
                        { miner
                            | x = movement.x
                            , y = movement.y
                            , vx = 0.0
                            , vy = finalVy
                            , state = Dying 0 -- Start death animation
                            , animFrame = newAnimFrame
                            , carryingGold = goldInteraction.carryingGold
                            , targetPlatform = goldInteraction.targetPlatform
                            , jumpVelocity = movement.jumpVelocity
                            , fallDistance = movement.fallDistance
                            , facingLeft = newFacingLeft
                        }

                    else
                        -- Normal miner update
                        { miner
                            | x = movement.x
                            , y = movement.y
                            , vx = 0.0 -- Java doesn't use continuous vx
                            , vy = finalVy
                            , state = movement.state
                            , animFrame = newAnimFrame
                            , carryingGold = goldInteraction.carryingGold
                            , targetPlatform = goldInteraction.targetPlatform
                            , jumpVelocity = movement.jumpVelocity
                            , fallDistance = movement.fallDistance
                            , facingLeft = newFacingLeft
                        }
            in
            ( updatedMiner, goldInteraction.level, newSeed )



-- Check if miner collides with slime (Java lines 630-648)


checkForSlime : Array.Array Int -> Int -> Int -> Int -> Int -> Bool
checkForSlime level levelW levelH x y =
    let
        -- Check all pixels covered by the miner sprite (7x10 pixels)
        spritePositions =
            List.range x (x + 6)
                |> List.concatMap
                    (\spriteX ->
                        List.range (y - 9) y
                            -- Miner sprite is 10 pixels tall, y is bottom
                            |> List.map (\spriteY -> ( spriteX, spriteY ))
                    )
    in
    -- Check if any sprite pixel overlaps with slime (green color 0x00FF00)
    List.any
        (\( checkX, checkY ) ->
            if checkX >= 0 && checkX < levelW && checkY >= 0 && checkY < levelH then
                getPixelAt checkX checkY level == 0xFF00

            else
                False
        )
        spritePositions



-- Slime spreading mechanics (Java lines 456-474)


spreadSlime : Array.Array Int -> Int -> Int -> Random.Seed -> Array.Array Int
spreadSlime level levelW levelH seed =
    let
        -- Only spread slime occasionally to prevent too rapid expansion
        ( shouldSpread, newSeed ) =
            Random.step (Random.int 0 15) seed
    in
    if shouldSpread == 0 then
        -- 1/16 chance each frame
        -- Find all slime pixels and try to spread them
        let
            slimePositions =
                findAllSlimePositions level levelW levelH
        in
        List.foldl (trySpreadSlimeAt levelW levelH) level slimePositions

    else
        level



-- Find all slime pixels in the level


findAllSlimePositions : Array.Array Int -> Int -> Int -> List ( Int, Int )
findAllSlimePositions level levelW levelH =
    List.range 0 (levelW - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (levelH - 1)
                    |> List.filterMap
                        (\y ->
                            if getPixelAt x y level == 0xFF00 then
                                Just ( x, y )

                            else
                                Nothing
                        )
            )



-- Try to spread slime from a position to adjacent pixels (Java spreading logic)


trySpreadSlimeAt : Int -> Int -> ( Int, Int ) -> Array.Array Int -> Array.Array Int
trySpreadSlimeAt levelW levelH ( x, y ) level =
    let
        -- Check all 4 adjacent directions
        adjacentPositions =
            [ ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]

        -- Spread to adjacent solid terrain (not air, gold, or slime itself)
        validTargets =
            adjacentPositions
                |> List.filter
                    (\( adjX, adjY ) ->
                        adjX
                            >= 0
                            && adjX
                            < levelW
                            && adjY
                            >= 0
                            && adjY
                            < levelH
                            && (let
                                    pixel =
                                        getPixelAt adjX adjY level
                                in
                                -- Spread to solid terrain (non-zero) but not gold or existing slime
                                pixel /= 0 && pixel /= 0x00FFFF00 && pixel /= 0xFF00
                               )
                    )
    in
    -- Convert all valid targets to slime
    List.foldl (\( adjX, adjY ) acc -> setPixelAt adjX adjY 0xFF00 acc) level validTargets



-- Check if miner is on solid ground (Java: y position is bottom of sprite)


isOnGround : Array.Array Int -> Int -> Int -> Int -> Int -> Bool
isOnGround level levelW levelH x y =
    let
        -- In Java, y is the bottom of the miner sprite, so check one pixel below
        checkPositions =
            List.range x (x + 6)

        bottomY =
            y + 1

        -- Java checks (miners[i][1] + 1)
        hasGroundBelow =
            List.any (isPixelSolid level levelW levelH bottomY) checkPositions
    in
    hasGroundBelow



-- Check if a pixel is solid (grass, dirt, or rock - not sky) - Java accurate


isPixelSolid : Array.Array Int -> Int -> Int -> Int -> Int -> Bool
isPixelSolid level levelW levelH y x =
    if x >= 0 && x < levelW && y >= 0 && y < levelH then
        let
            pixel =
                getPixelAt x y level

            -- Use Java indexing: x | y << 10
        in
        pixel /= 0
        -- Solid means non-zero (Java stores 0 for air/empty)

    else
        True



-- Treat out-of-bounds as solid
-- Java falling logic: fall up to 2 pixels (lines 516-522)


fallTwoPixels : Array.Array Int -> Int -> Int -> Float -> Int -> Int -> Bool -> Random.Seed -> ( MinerMovement, Random.Seed )
fallTwoPixels level levelW levelH x startY fallDist facingLeft seed =
    let
        -- First fall step
        fallY1 =
            if
                startY
                    < 4
                    || (startY
                            >= 0
                            && startY
                            < levelH
                            - 1
                            && not (isPixelSolid level levelW levelH (startY + 1) (round x))
                       )
            then
                startY + 1

            else
                startY

        -- Second fall step
        fallY2 =
            if
                fallY1
                    < 4
                    || (fallY1
                            >= 0
                            && fallY1
                            < levelH
                            - 1
                            && not (isPixelSolid level levelW levelH (fallY1 + 1) (round x))
                       )
            then
                fallY1 + 1

            else
                fallY1

        newFallDist =
            fallDist + (fallY2 - startY)
    in
    ( { x = x, y = toFloat fallY2, jumpVelocity = 0, fallDistance = newFallDist, state = Falling, facingLeft = facingLeft }, seed )



-- Java ground movement and wall collision (lines 527-571)


handleGroundMovement : Array.Array Int -> Int -> Int -> Miner -> Int -> Random.Seed -> ( MinerMovement, Random.Seed )
handleGroundMovement level levelW levelH miner adjustedY seed =
    let
        currentX =
            round miner.x

        -- Check if fallen too far (Java line 528-534)
        shouldDie =
            miner.fallDistance > 100
    in
    if shouldDie then
        -- Miner dies from falling too far - transition to dying state
        ( { x = miner.x, y = toFloat adjustedY, jumpVelocity = -1, fallDistance = 0, state = Dying 1, facingLeft = miner.facingLeft }, seed )

    else
        -- Reset fall distance and handle horizontal movement (lines 536-571)
        let
            newFallDist =
                0

            newJumpVel =
                -1

            -- On ground
            -- Java line 542: 1/20 chance to stand still
            ( standStillRoll, seed1 ) =
                Random.step (Random.int 0 19) seed

            shouldStandStill =
                standStillRoll == 0
        in
        if shouldStandStill then
            -- Stand still this frame
            ( { x = miner.x, y = toFloat adjustedY, jumpVelocity = newJumpVel, fallDistance = newFallDist, state = Walking, facingLeft = miner.facingLeft }, seed1 )

        else
            -- Try to move (Java lines 543-571)
            let
                -- Java: Use same direction logic for all miners (no special gold pathfinding)
                targetDirection =
                    if miner.facingLeft then
                        -1

                    else
                        1

                newFacingForGold =
                    miner.facingLeft

                targetX =
                    currentX + targetDirection

                validMove =
                    findValidMove level levelW levelH targetX adjustedY
            in
            case validMove of
                Just ( validX, validY ) ->
                    -- Successful move (hit = false)
                    let
                        -- Java line 562: 1/4000 chance to randomly turn around even when not hitting wall
                        ( randomTurnRoll, seed2 ) =
                            Random.step (Random.int 0 3999) seed1

                        shouldRandomTurn =
                            randomTurnRoll == 0

                        newFacingLeft =
                            if shouldRandomTurn then
                                not newFacingForGold

                            else
                                newFacingForGold
                    in
                    ( { x = toFloat validX, y = toFloat validY, jumpVelocity = newJumpVel, fallDistance = newFallDist, state = Walking, facingLeft = newFacingLeft }, seed2 )

                Nothing ->
                    -- Hit a wall (hit = true) - random turning logic (lines 560-570)
                    let
                        -- Java line 562: 1/10 chance to turn around when hitting wall
                        ( turnRoll, seed2 ) =
                            Random.step (Random.int 0 9) seed1

                        shouldTurn =
                            turnRoll == 0

                        newFacingLeft =
                            if shouldTurn then
                                not newFacingForGold

                            else
                                newFacingForGold

                        -- Java lines 567-568: 66% chance to jump when turning after hitting wall
                        ( jumpChance, seed3 ) =
                            Random.step (Random.int 0 2) seed2

                        shouldJump =
                            shouldTurn && jumpChance /= 0
                    in
                    if shouldJump then
                        ( { x = miner.x, y = toFloat adjustedY, jumpVelocity = 16, fallDistance = newFallDist, state = Walking, facingLeft = newFacingLeft }, seed3 )
                        -- Jump!

                    else
                        ( { x = miner.x, y = toFloat adjustedY, jumpVelocity = newJumpVel, fallDistance = newFallDist, state = Walking, facingLeft = newFacingLeft }, seed3 )



-- Note: Java miners don't actively seek platforms - they find them through random movement
-- Java-accurate slope walking: check -4 to +2 pixels for valid movement (lines 548-558 in Java)


findValidMove : Array.Array Int -> Int -> Int -> Int -> Int -> Maybe ( Int, Int )
findValidMove level levelW levelH targetX currentY =
    let
        -- Java logic: for (int y = 2; y >= -4; y--)
        checkYOffsets =
            [ 2, 1, 0, -1, -2, -3, -4 ]

        checkOffset yOffset =
            let
                testY =
                    currentY + yOffset
            in
            if not (isPixelSolid level levelW levelH testY targetX) then
                Just ( targetX, testY )

            else
                Nothing
    in
    -- Return the first valid move found (like Java's break statement)
    List.filterMap checkOffset checkYOffsets
        |> List.head



-- Get the ground level at a given x position (Java: return y where miner's bottom should be)


getGroundLevel : Array.Array Int -> Int -> Int -> Int -> Int
getGroundLevel level levelW levelH x =
    let
        findGround y =
            if y >= levelH then
                levelH - 1

            else if isPixelSolid level levelW levelH y x then
                y - 1
                -- Return position where miner's bottom (y position) should be

            else
                findGround (y + 1)
    in
    findGround 0



-- PIXEL BUFFER GENERATION (like Java's System.arraycopy)


generatePixelBuffer : Model -> List Int
generatePixelBuffer model =
    -- Start with terrain, then overlay miners
    let
        -- Get base terrain pixels
        basePixels =
            List.range 0 (gameHeight - 1)
                |> List.concatMap (copyScreenRow model)

        -- Convert to array for easy miner rendering
        pixelArray =
            Array.fromList basePixels

        -- Render all miners on top of terrain
        minersPixels =
            List.foldl (renderMinerToBuffer model) pixelArray model.miners

        -- Render portals on top of miners
        portalsPixels =
            case model.portals of
                Nothing ->
                    minersPixels

                Just ( p1, maybeP2 ) ->
                    let
                        withP1 =
                            renderPortalToBuffer model p1 minersPixels
                    in
                    case maybeP2 of
                        Nothing ->
                            withP1

                        Just p2 ->
                            renderPortalToBuffer model p2 withP1

        -- Render preview portal on top of everything (with transparency effect)
        withPreviewPortal =
            case model.portalPreview of
                Nothing ->
                    portalsPixels

                Just previewPortal ->
                    renderPreviewPortalToBuffer model previewPortal portalsPixels

        -- Render dirt preview on top of everything else
        finalPixels =
            case model.dirtPreview of
                Nothing ->
                    withPreviewPortal

                Just ( previewX, previewY ) ->
                    renderPreviewDirtToBuffer model previewX previewY withPreviewPortal
    in
    Array.toList finalPixels


renderMinerToBuffer : Model -> Miner -> Array.Array Int -> Array.Array Int
renderMinerToBuffer model miner pixelArray =
    let
        -- Convert world coordinates to screen coordinates
        screenX =
            round miner.x - model.cameraX

        screenY =
            round miner.y - model.cameraY
    in
    -- Only render if miner is visible on screen
    if screenX >= -7 && screenX < gameWidth && screenY >= -10 && screenY < gameHeight then
        renderMinerSprite pixelArray screenX screenY miner

    else
        pixelArray


renderMinerSprite : Array.Array Int -> Int -> Int -> Miner -> Array.Array Int
renderMinerSprite pixelArray screenX screenY miner =
    let
        spriteIndex =
            case miner.state of
                Walking ->
                    miner.animFrame // 4

                -- Java: (miners[i][3] / 4) - changes every 4 steps
                Falling ->
                    0

                -- Use standing frame when falling
                Dying frame ->
                    min (frame + 4) 6

                -- Death frames 4-6
                Dead ->
                    6

        -- Final death frame
        sprite =
            Array.get spriteIndex minerSprites |> Maybe.withDefault []
    in
    sprite
        |> List.indexedMap
            (\row line ->
                let
                    -- Flip sprite horizontally if facing left
                    flippedLine =
                        if miner.facingLeft then
                            String.reverse line

                        else
                            line
                in
                String.toList flippedLine
                    |> List.indexedMap
                        (\col char ->
                            let
                                pixelX =
                                    screenX + col

                                -- Java: yy goes from -8 to +1, so sprite extends upward from miner position
                                pixelY =
                                    screenY + row - 8

                                -- Offset by 8 to match Java positioning
                                color =
                                    spriteCharToColor char miner.carryingGold

                                bufferIndex =
                                    pixelY * gameWidth + pixelX
                            in
                            if color /= -1 && pixelX >= 0 && pixelX < gameWidth && pixelY >= 0 && pixelY < gameHeight then
                                ( bufferIndex, color )

                            else
                                ( -1, 0 )
                        )
                    |> List.filter (\( idx, _ ) -> idx /= -1)
            )
        |> List.concat
        |> List.foldl (\( idx, color ) acc -> Array.set idx color acc) pixelArray


renderPortalToBuffer : Model -> Portal -> Array.Array Int -> Array.Array Int
renderPortalToBuffer model portal pixelArray =
    let
        -- Convert world coordinates to screen coordinates
        screenX =
            portal.x - model.cameraX

        screenY =
            portal.y - model.cameraY

        -- Portal dimensions: 7 pixels wide, 12 pixels tall (miner width + 20%)
        portalWidth =
            7

        portalHeight =
            12

        -- Portal colors: blue for P1, bright orange for P2
        portalColor =
            if portal.isP1 then
                0x66FF

            else
                0x00FF6600
    in
    -- Only render if portal is visible on screen
    if screenX >= -portalWidth && screenX < gameWidth && screenY >= -portalHeight && screenY < gameHeight then
        List.range 0 (portalHeight - 1)
            |> List.concatMap
                (\row ->
                    List.range 0 (portalWidth - 1)
                        |> List.map
                            (\col ->
                                let
                                    pixelX =
                                        screenX + col

                                    pixelY =
                                        screenY + row

                                    bufferIndex =
                                        pixelY * gameWidth + pixelX
                                in
                                if pixelX >= 0 && pixelX < gameWidth && pixelY >= 0 && pixelY < gameHeight then
                                    ( bufferIndex, portalColor )

                                else
                                    ( -1, 0 )
                            )
                        |> List.filter (\( idx, _ ) -> idx /= -1)
                )
            |> List.foldl (\( idx, color ) acc -> Array.set idx color acc) pixelArray

    else
        pixelArray


renderPreviewDirtToBuffer : Model -> Int -> Int -> Array.Array Int -> Array.Array Int
renderPreviewDirtToBuffer model centerX centerY pixelArray =
    let
        -- Convert world coordinates to screen coordinates
        screenX =
            centerX - model.cameraX

        screenY =
            centerY - model.cameraY

        -- Use same 7x7 brush pattern as buildTerrain/digTerrain (minus corners)
        brushSize =
            3

        -- -3 to 3 range
        previewColor =
            0x00FFFFFF

        -- White color for preview
    in
    -- Only render if area is visible on screen
    if screenX >= -7 && screenX < gameWidth + 7 && screenY >= -7 && screenY < gameHeight + 7 then
        List.range -brushSize brushSize
            |> List.concatMap
                (\xx ->
                    List.range -brushSize brushSize
                        |> List.filterMap
                            (\yy ->
                                let
                                    -- Skip corners to match dirt brush pattern
                                    isCorner =
                                        (xx == -3 || xx == 3) && (yy == -3 || yy == 3)

                                    pixelX =
                                        screenX + xx

                                    pixelY =
                                        screenY + yy

                                    bufferIndex =
                                        pixelY * gameWidth + pixelX

                                    -- Create dashed effect for preview - only render some pixels
                                    shouldRender =
                                        not isCorner && modBy 2 (xx + yy) == 0
                                in
                                if shouldRender && pixelX >= 0 && pixelX < gameWidth && pixelY >= 0 && pixelY < gameHeight then
                                    Just ( bufferIndex, previewColor )

                                else
                                    Nothing
                            )
                )
            |> List.foldl (\( idx, color ) acc -> Array.set idx color acc) pixelArray

    else
        pixelArray


renderPreviewPortalToBuffer : Model -> Portal -> Array.Array Int -> Array.Array Int
renderPreviewPortalToBuffer model portal pixelArray =
    let
        -- Convert world coordinates to screen coordinates
        screenX =
            portal.x - model.cameraX

        screenY =
            portal.y - model.cameraY

        -- Portal dimensions: 7 pixels wide, 12 pixels tall (miner width + 20%)
        portalWidth =
            7

        portalHeight =
            12

        -- Preview portal colors: lighter/semi-transparent versions
        baseColor =
            if portal.isP1 then
                0x66FF

            else
                0x00FF3300

        -- Mix with white for lighter effect (simple brightness increase)
        previewColor =
            baseColor + 0x00404040

        -- Add some brightness to make it look "ghosted"
    in
    -- Only render if portal is visible on screen
    if screenX >= -portalWidth && screenX < gameWidth && screenY >= -portalHeight && screenY < gameHeight then
        List.range 0 (portalHeight - 1)
            |> List.concatMap
                (\row ->
                    List.range 0 (portalWidth - 1)
                        |> List.filterMap
                            (\col ->
                                let
                                    pixelX =
                                        screenX + col

                                    pixelY =
                                        screenY + row

                                    bufferIndex =
                                        pixelY * gameWidth + pixelX

                                    -- Create dashed effect for preview - only render some pixels
                                    shouldRender =
                                        modBy 2 (row + col) == 0
                                in
                                if shouldRender && pixelX >= 0 && pixelX < gameWidth && pixelY >= 0 && pixelY < gameHeight then
                                    Just ( bufferIndex, previewColor )

                                else
                                    Nothing
                            )
                )
            |> List.foldl (\( idx, color ) acc -> Array.set idx color acc) pixelArray

    else
        pixelArray


copyScreenRow : Model -> Int -> List Int
copyScreenRow model screenY =
    let
        sourceY =
            screenY + model.cameraY

        -- Ensure we're within bounds
        row =
            if sourceY >= 0 && sourceY < model.levelHeight then
                List.range 0 (gameWidth - 1)
                    |> List.map
                        (\screenX ->
                            let
                                sourceX =
                                    screenX + model.cameraX
                            in
                            if sourceX >= 0 && sourceX < model.levelWidth then
                                getPixelAt sourceX sourceY model.level
                                -- Use Java indexing

                            else
                                0x00FF0000
                         -- Red for out of bounds
                        )

            else
                -- Out of bounds vertically - return debug color
                List.repeat gameWidth 0xFF

        -- Blue for debug
    in
    row



-- TERRAIN MODIFICATION (Java mouse interaction implementation)
-- Java algorithm: right-click/shift removes terrain, left-click adds dirt
-- Uses 7x7 brush minus corners, with line interpolation for smooth drawing


handleTerrainInteraction : Model -> Array.Array Int
handleTerrainInteraction model =
    -- Java: interpolate between old and new mouse positions
    let
        worldMouseX =
            model.mouseX + model.cameraX

        worldMouseY =
            model.mouseY + model.cameraY

        prevWorldMouseX =
            model.prevMouseX + model.cameraX

        prevWorldMouseY =
            model.prevMouseY + model.cameraY

        -- Java: calculate distance for interpolation
        dx =
            worldMouseX - prevWorldMouseX

        dy =
            worldMouseY - prevWorldMouseY

        distance =
            max 1 (round (sqrt (toFloat (dx * dx + dy * dy))))
    in
    List.range 0 distance
        |> List.foldl
            (\i levelArray ->
                let
                    -- Interpolate position
                    xm =
                        prevWorldMouseX + (dx * i) // distance

                    ym =
                        prevWorldMouseY + (dy * i) // distance
                in
                if model.mouseButton == 0 then
                    -- Left mouse (browser button 0): build dirt
                    buildTerrain xm ym levelArray model

                else if model.mouseButton == 2 then
                    -- Right mouse (button 2): remove terrain (digging)
                    digTerrain xm ym levelArray model

                else
                    -- Any other button: no action
                    levelArray
            )
            model.level



-- Java algorithm: 7x7 brush with missing corners for digging


digTerrain : Int -> Int -> Array.Array Int -> Model -> Array.Array Int
digTerrain centerX centerY levelArray model =
    List.range -3 3
        |> List.concatMap
            (\xx ->
                List.range -3 3
                    |> List.map (\yy -> ( xx, yy ))
            )
        |> List.filter
            (\( xx, yy ) ->
                -- Java: remove corners from 7x7 brush
                not ((xx == -3 || xx == 3) && (yy == -3 || yy == 3))
            )
        |> List.foldl
            (\( xx, yy ) acc ->
                let
                    x =
                        centerX + xx

                    y =
                        centerY + yy
                in
                if x >= 0 && y >= 0 && x < 1024 && y < 2048 then
                    let
                        currentColor =
                            getPixelAt x y acc

                        r =
                            Bitwise.shiftRightBy 16 currentColor |> Bitwise.and 0xFF

                        g =
                            Bitwise.shiftRightBy 8 currentColor |> Bitwise.and 0xFF

                        b =
                            currentColor |> Bitwise.and 0xFF
                    in
                    -- Java: only clear if it's not gold, rock, or slime
                    -- Gold: r == g, Rock: r == g == b, Slime: r == b
                    if r /= g && r /= b then
                        setPixelAt x y 0 acc
                        -- Clear terrain

                    else
                        acc

                else
                    acc
            )
            levelArray



-- Java algorithm: add dirt with brightness variation


buildTerrain : Int -> Int -> Array.Array Int -> Model -> Array.Array Int
buildTerrain centerX centerY levelArray model =
    List.range -3 3
        |> List.concatMap
            (\xx ->
                List.range -3 3
                    |> List.map (\yy -> ( xx, yy ))
            )
        |> List.filter
            (\( xx, yy ) ->
                -- Java: remove corners from 7x7 brush
                not ((xx == -3 || xx == 3) && (yy == -3 || yy == 3))
            )
        |> List.foldl
            (\( xx, yy ) acc ->
                let
                    x =
                        centerX + xx

                    y =
                        centerY + yy
                in
                if x >= 0 && y >= 0 && x < 1024 && y < 2048 then
                    let
                        currentColor =
                            getPixelAt x y acc
                    in
                    -- Java: only add dirt if there's no terrain
                    if currentColor == 0 then
                        let
                            -- Java brightness calculation (simplified)
                            -- double br = 1.6 - (random.nextDouble() - 0.5) * random.nextDouble() * random.nextDouble() * 0.6;
                            -- br *= (1 - (x | y << 10) / (1024 * 6048.0));
                            baseBrightness =
                                1.4

                            -- Simplified, Java uses random
                            depthFactor =
                                1.0 - toFloat (Bitwise.or x (Bitwise.shiftLeftBy 10 y)) / (1024.0 * 6048.0)

                            brightness =
                                baseBrightness * depthFactor

                            -- Java dirt colors: r=111, g=92, b=51 with brightness
                            r =
                                min 255 (max 0 (round (111.0 * brightness)))

                            g =
                                min 255 (max 0 (round (92.0 * brightness)))

                            b =
                                min 255 (max 0 (round (51.0 * brightness)))

                            dirtColor =
                                Bitwise.shiftLeftBy 16 r
                                    |> Bitwise.or (Bitwise.shiftLeftBy 8 g)
                                    |> Bitwise.or b
                        in
                        setPixelAt x y dirtColor acc

                    else
                        acc

                else
                    acc
            )
            levelArray



-- ARROW KEY HANDLING AND CAMERA SCROLLING (Java-accurate)


updateArrowKeys : Int -> Bool -> ArrowKeys -> ArrowKeys
updateArrowKeys keyCode pressed arrows =
    case keyCode of
        37 ->
            { arrows | left = pressed }

        -- Left arrow
        38 ->
            { arrows | up = pressed }

        -- Up arrow
        39 ->
            { arrows | right = pressed }

        -- Right arrow
        40 ->
            { arrows | down = pressed }

        -- Down arrow
        _ ->
            arrows



-- Java camera movement logic (lines 675-679)


updateCamera : ArrowKeys -> Int -> Int -> Int -> Int -> ( Int, Int )
updateCamera arrows levelWidth levelHeight cameraX cameraY =
    let
        -- Java: if (keys[37] && xo > 8) xo -= 8;
        newX =
            if arrows.left && cameraX > 8 then
                cameraX - 8

            else if arrows.right && cameraX < levelWidth - gameWidth then
                cameraX + 8
                -- Java: if (keys[39] && xo < level_width - 640) xo += 8;

            else
                cameraX

        -- Java: if (keys[38] && yo > 8) yo -= 8;
        newY =
            if arrows.up && cameraY > 8 then
                cameraY - 8

            else if arrows.down && cameraY < levelHeight - gameHeight then
                cameraY + 8
                -- Java: if (keys[40] && yo < level_height - 480) yo += 8;

            else
                cameraY
    in
    ( newX, newY )



-- Helper function to detect when a miner delivers gold (for score tracking)


isGoldDeliveredThisFrame : Miner -> Miner -> Bool
isGoldDeliveredThisFrame oldMiner newMiner =
    -- Gold was delivered if: was carrying gold before, not carrying now, and on platform
    oldMiner.carryingGold
        && not newMiner.carryingGold
        && (oldMiner.targetPlatform /= Nothing)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartLevel ->
            ( { model | gameState = Playing, gameStartTime = 0, currentTime = 0 }
            , Task.perform (GameTick << Time.posixToMillis) Time.now
            )

        RenderFrame ->
            let
                pixelBuffer =
                    generatePixelBuffer model
            in
            ( model, renderPixels pixelBuffer )

        UpdateMiners ->
            if model.gameState == Playing then
                let
                    -- Handle camera scrolling (Java-accurate)
                    ( newCameraX, newCameraY ) =
                        updateCamera model.arrowKeysPressed model.levelWidth model.levelHeight model.cameraX model.cameraY

                    -- Handle terrain interaction
                    levelAfterInteraction =
                        if model.mouseButton >= 0 then
                            handleTerrainInteraction { model | cameraX = newCameraX, cameraY = newCameraY }

                        else
                            model.level

                    -- Update miners only if not paused, but always allow terrain interaction
                    -- Apply speed multiplier by running multiple update iterations for speeds > 1.0
                    updateIterations =
                        max 1 (round model.gameSpeed)

                    minerUpdateResult =
                        if model.paused then
                            -- Keep miners as they are when paused, just update level from terrain interaction
                            { miners = model.miners, level = levelAfterInteraction, seed = model.randomSeed, score = model.score, livingMiners = model.totalMiners }

                        else
                            -- Run multiple iterations for higher speed (Java-style)
                            List.range 1 updateIterations
                                |> List.foldl
                                    (\_ iterResult ->
                                        List.foldl
                                            (\miner acc ->
                                                let
                                                    ( newMiner, newLevel, newSeed ) =
                                                        updateMiner acc.level model.levelWidth model.levelHeight miner acc.seed 1.0

                                                    -- Check for portal teleportation
                                                    minerAfterPortal =
                                                        checkPortalTeleportation newMiner model.portals newLevel model.levelWidth model.levelHeight

                                                    -- Check if gold was delivered (score increment happens in updateMiner via dropGoldOnPlatform)
                                                    scoreDelta =
                                                        if isGoldDeliveredThisFrame miner minerAfterPortal then
                                                            1

                                                        else
                                                            0

                                                    aliveCount =
                                                        if minerAfterPortal.state /= Dead then
                                                            1

                                                        else
                                                            0
                                                in
                                                { miners = minerAfterPortal :: acc.miners
                                                , level = newLevel
                                                , seed = newSeed
                                                , score = acc.score + scoreDelta
                                                , livingMiners = acc.livingMiners + aliveCount
                                                }
                                            )
                                            { miners = [], level = iterResult.level, seed = iterResult.seed, score = iterResult.score, livingMiners = 0 }
                                            iterResult.miners
                                    )
                                    { miners = model.miners, level = levelAfterInteraction, seed = model.randomSeed, score = model.score, livingMiners = model.totalMiners }

                    finalMiners =
                        List.reverse minerUpdateResult.miners

                    -- Apply slime spreading only if not paused (Java lines 456-474)
                    -- Run slime spreading multiple times for higher speeds
                    levelWithSlime =
                        if model.paused then
                            minerUpdateResult.level

                        else
                            List.range 1 updateIterations
                                |> List.foldl
                                    (\_ currentLevel ->
                                        spreadSlime currentLevel model.levelWidth model.levelHeight minerUpdateResult.seed
                                    )
                                    minerUpdateResult.level

                    -- Update model with new values
                    newModel =
                        { model | miners = finalMiners, level = levelWithSlime, randomSeed = minerUpdateResult.seed, prevMouseX = model.mouseX, prevMouseY = model.mouseY, cameraX = newCameraX, cameraY = newCameraY, score = minerUpdateResult.score, totalMiners = minerUpdateResult.livingMiners }

                    pixelBuffer =
                        generatePixelBuffer newModel

                    renderCmd =
                        renderPixels pixelBuffer
                in
                ( newModel, renderCmd )

            else
                -- Not playing, just render current state
                let
                    pixelBuffer =
                        generatePixelBuffer model
                in
                ( model, renderPixels pixelBuffer )

        SeedGenerated seedValue ->
            let
                -- Generate fresh level with new seed, using config level
                freshSeed =
                    Random.initialSeed seedValue

                actualLevel =
                    if model.configLevel > 0 then
                        model.configLevel - 1

                    else
                        model.currentLevel

                levelConfig =
                    getLevelConfig actualLevel

                levelResult =
                    createLevel actualLevel freshSeed

                -- Create fresh miners with config count (or level default if not set)
                minerCount =
                    if model.configNumMiners > 0 then
                        model.configNumMiners

                    else
                        levelConfig.diggers

                freshMiners =
                    createInitialMiners levelResult.levelWidth minerCount freshSeed

                -- Apply config starting time as negative gameStartTime for head start
                startTimeOffset =
                    if model.configStartingTime > 0 then
                        -(model.configStartingTime * 1000)

                    else
                        0

                newModel =
                    { model
                        | level = levelResult.level
                        , randomSeed = levelResult.seed
                        , levelWidth = levelResult.levelWidth
                        , levelHeight = levelResult.levelHeight
                        , miners = freshMiners
                        , mouseX = 0
                        , mouseY = 0
                        , mouseButton = -1
                        , prevMouseX = 0
                        , prevMouseY = 0
                        , shiftPressed = False

                        -- Reset game state values using config values
                        , score = model.configGoldValue
                        , levelTarget = levelConfig.target
                        , totalMiners = minerCount
                        , gameStartTime = startTimeOffset
                        , currentTime = 0
                        , gameState = LevelIntro
                        , cameraX = 0
                        , cameraY = 0
                        , arrowKeysPressed = { left = False, right = False, up = False, down = False }
                        , currentLevel = actualLevel

                        -- Reset portal system when level updates
                        , portalMode = False
                        , portals = Nothing
                        , portalTimeRemaining = 0
                        , portalPreview = Nothing
                        , dirtPreview = Nothing
                    }

                -- Render with fresh level
                pixelBuffer =
                    generatePixelBuffer newModel
            in
            ( newModel, renderPixels pixelBuffer )

        MouseMove x y ->
            let
                updatedModel =
                    { model | mouseX = x, mouseY = y }

                -- Update preview portal position if in portal mode, or dirt preview if not
                finalModel =
                    if model.portalMode then
                        let
                            levelX =
                                x + model.cameraX

                            levelY =
                                y + model.cameraY

                            isP1 =
                                model.portals == Nothing

                            updatedPreview =
                                { x = levelX, y = levelY, isP1 = isP1 }
                        in
                        { updatedModel | portalPreview = Just updatedPreview, dirtPreview = Nothing }

                    else
                        let
                            levelX =
                                x + model.cameraX

                            levelY =
                                y + model.cameraY
                        in
                        { updatedModel | dirtPreview = Just ( levelX, levelY ), portalPreview = Nothing }
            in
            ( finalModel, Cmd.none )

        MouseDown button ->
            case model.gameState of
                LevelIntro ->
                    -- Start the level (Java: if (mouseButton > 0) levelStarted = true)
                    ( { model | gameState = Playing, gameStartTime = 0, currentTime = 0 }
                    , Task.perform (GameTick << Time.posixToMillis) Time.now
                    )

                Playing ->
                    -- Handle portal placement or terrain interaction during gameplay
                    if model.portalMode && button == 2 then
                        -- Right-click cancels portal mode
                        ( { model | portalMode = False, portalPreview = Nothing }, Cmd.none )

                    else if model.portalMode && button == 0 then
                        -- Portal placement mode: place portal at click position
                        let
                            -- Convert screen coordinates to level coordinates
                            levelX =
                                model.mouseX + model.cameraX

                            levelY =
                                model.mouseY + model.cameraY

                            -- Create new portal
                            newPortal =
                                { x = levelX, y = levelY, isP1 = model.portals == Nothing }

                            -- Update portals and calculate cost
                            ( newPortals, goldCost, shouldExitPortalMode ) =
                                case model.portals of
                                    Nothing ->
                                        -- Place first portal (P1) - costs 20 gold, stay in portal mode for P2
                                        ( Just ( newPortal, Nothing ), 20, False )

                                    Just ( p1, Nothing ) ->
                                        -- Place second portal (P2) - free since pair already paid for, exit portal mode
                                        ( Just ( p1, Just { newPortal | isP1 = False } ), 0, True )

                                    Just ( _, _ ) ->
                                        -- Replace existing portals with new P1 - costs 20 gold for new pair, stay in portal mode for P2
                                        ( Just ( newPortal, Nothing ), 20, False )

                            -- Set portal duration (20 seconds)
                            portalDuration =
                                20

                            -- Deduct gold cost
                            newScore =
                                max 0 (model.score - goldCost)

                            -- Update preview portal for next placement (if staying in portal mode)
                            newPortalPreview =
                                if shouldExitPortalMode then
                                    Nothing

                                else
                                    Just { x = model.mouseX + model.cameraX, y = model.mouseY + model.cameraY, isP1 = False }
                        in
                        ( { model
                            | portals = newPortals
                            , portalTimeRemaining = portalDuration
                            , score = newScore
                            , portalMode = not shouldExitPortalMode
                            , portalPreview = newPortalPreview
                          }
                        , Cmd.none
                        )

                    else
                        -- Normal terrain interaction
                        ( { model | mouseButton = button }, Cmd.none )

                LevelComplete ->
                    -- Advance to next level
                    ( model, Task.perform (always NextLevel) (Task.succeed ()) )

                LevelFailed ->
                    -- Restart current level
                    ( model, Task.perform (always RestartLevel) (Task.succeed ()) )

                GameComplete ->
                    -- Game finished, no action
                    ( model, Cmd.none )

        MouseUp ->
            ( { model | mouseButton = -1 }, Cmd.none )

        KeyDown keyCode ->
            let
                newShiftPressed =
                    if keyCode == 16 then
                        True

                    else
                        model.shiftPressed

                -- Shift key
                newArrowKeys =
                    updateArrowKeys keyCode True model.arrowKeysPressed

                -- Space key (32) activates time stop (costs 80 gold) or deactivates if already active
                -- P key (80) activates portal mode if player has enough gold
                -- Escape key (27) cancels portal mode
                ( newModel, portalCmd ) =
                    if keyCode == 32 && model.gameState == Playing then
                        if model.paused then
                            -- Deactivate time stop (no cost)
                            ( { model | paused = False }, Cmd.none )

                        else if model.score >= 80 then
                            -- Activate time stop (costs 80 gold)
                            ( { model | paused = True, score = model.score - 80 }, Cmd.none )

                        else
                            -- Not enough gold for time stop
                            ( model, Cmd.none )

                    else if
                        keyCode
                            == 80
                            && model.gameState
                            == Playing
                            && model.score
                            >= 20
                            && (case model.portals of
                                    Just ( _, Just _ ) ->
                                        False

                                    -- Don't allow portal mode when both portals are active
                                    _ ->
                                        True
                                -- Allow portal mode in all other cases
                               )
                    then
                        let
                            -- Create preview portal under mouse cursor
                            levelX =
                                model.mouseX + model.cameraX

                            levelY =
                                model.mouseY + model.cameraY

                            isP1 =
                                model.portals == Nothing

                            previewPortal =
                                { x = levelX, y = levelY, isP1 = isP1 }
                        in
                        ( { model | portalMode = True, portalPreview = Just previewPortal }, Cmd.none )

                    else if keyCode == 27 && model.portalMode then
                        -- Escape cancels portal mode
                        ( { model | portalMode = False, portalPreview = Nothing }, Cmd.none )

                    else
                        ( model, Cmd.none )
            in
            ( { newModel | shiftPressed = newShiftPressed, arrowKeysPressed = newArrowKeys }, portalCmd )

        KeyUp keyCode ->
            let
                newShiftPressed =
                    if keyCode == 16 then
                        False

                    else
                        model.shiftPressed

                -- Shift key
                newArrowKeys =
                    updateArrowKeys keyCode False model.arrowKeysPressed
            in
            ( { model | shiftPressed = newShiftPressed, arrowKeysPressed = newArrowKeys }, Cmd.none )

        NextLevel ->
            if model.currentLevel < 6 then
                -- Advance to next level (Java: current_level++)
                let
                    nextLevel =
                        model.currentLevel + 1
                in
                ( { model | currentLevel = nextLevel, gameState = LevelIntro }
                , Task.perform (\posix -> SeedGenerated (Time.posixToMillis posix)) Time.now
                )

            else
                -- Game complete!
                ( { model | gameState = GameComplete }, Cmd.none )

        RestartLevel ->
            -- Restart current level with fresh seed
            ( { model | gameState = LevelIntro }
            , Task.perform (\posix -> SeedGenerated (Time.posixToMillis posix)) Time.now
            )

        GameTick currentTime ->
            -- Update game timer and check for time-based game over (only if not paused)
            if model.paused then
                -- Time is stopped - don't update currentTime
                ( model, Cmd.none )

            else
                let
                    newModel =
                        { model
                            | gameStartTime =
                                if model.gameStartTime == 0 then
                                    currentTime

                                else
                                    model.gameStartTime
                            , currentTime = currentTime
                        }

                    elapsed =
                        (currentTime - newModel.gameStartTime) // 1000

                    -- Convert to seconds
                    levelConfig =
                        getLevelConfig model.currentLevel

                    timeLeft =
                        levelConfig.timeLimit - elapsed

                    -- Check game over conditions (Java lines 724-733)
                    gameOverCondition =
                        if model.gameState == Playing then
                            if model.score >= model.levelTarget then
                                Just LevelComplete

                            else if model.totalMiners <= 0 || timeLeft <= 0 then
                                Just LevelFailed

                            else
                                Nothing

                        else
                            Nothing
                in
                case gameOverCondition of
                    Just newState ->
                        ( { newModel | gameState = newState }, Cmd.none )

                    Nothing ->
                        ( newModel, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            -- Handle URL changes, restart game with new level if level parameter changed
            let
                urlLevel =
                    parseLevelFromUrl url |> Maybe.withDefault 0

                currentLevel =
                    max 0 (min 6 urlLevel)

                urlGold =
                    parseGoldFromUrl url |> Maybe.withDefault 0
            in
            if currentLevel /= model.currentLevel then
                -- Level changed, regenerate the level
                let
                    levelConfig =
                        getLevelConfig currentLevel

                    levelResult =
                        createLevel currentLevel model.randomSeed
                in
                ( { model
                    | url = url
                    , currentLevel = currentLevel
                    , level = levelResult.level
                    , levelWidth = levelResult.levelWidth
                    , levelHeight = levelResult.levelHeight
                    , miners = createInitialMiners levelResult.levelWidth levelConfig.diggers levelResult.seed
                    , score = urlGold
                    , levelTarget = levelConfig.target
                    , totalMiners = levelConfig.diggers
                    , gameStartTime = 0
                    , currentTime = 0
                    , gameState = LevelIntro
                    , cameraX = 0
                    , cameraY = 0
                  }
                , Cmd.none
                )

            else
                ( { model | url = url }, Cmd.none )

        ActivatePortalMode ->
            -- This is handled in KeyDown, but we need the case for completeness
            ( model, Cmd.none )

        PortalTick ->
            -- Decrement portal timer and clear portals when expired (only if not paused)
            if model.paused then
                -- Time is stopped - don't update portal timer
                ( model, Cmd.none )

            else if model.portalTimeRemaining > 0 then
                let
                    newTimeRemaining =
                        model.portalTimeRemaining - 1

                    newPortals =
                        if newTimeRemaining <= 0 then
                            Nothing

                        else
                            model.portals
                in
                ( { model | portalTimeRemaining = newTimeRemaining, portals = newPortals }, Cmd.none )

            else
                ( model, Cmd.none )

        SetGameSpeed newSpeed ->
            -- Update game speed multiplier, clamped to reasonable range (0.1x to 5.0x)
            let
                clampedSpeed =
                    max 0.1 (min 5.0 newSpeed)
            in
            ( { model | gameSpeed = clampedSpeed }, Cmd.none )

        ToggleConfigPanel ->
            ( { model | configPanelExpanded = not model.configPanelExpanded }, Cmd.none )

        SetConfigStartingTime newTime ->
            ( { model | configStartingTime = max 0 newTime }, Cmd.none )

        SetConfigGoldValue newGold ->
            ( { model | configGoldValue = max 0 newGold }, Cmd.none )

        SetConfigNumMiners newMiners ->
            ( { model | configNumMiners = max 1 newMiners }, Cmd.none )

        SetConfigLevel newLevel ->
            ( { model | configLevel = max 1 (min 7 newLevel) }, Cmd.none )

        StartNewWithConfig ->
            let
                -- Apply config settings and start new game
                updatedModel =
                    { model
                        | currentLevel = model.configLevel - 1 -- Convert to 0-based
                        , score = model.configGoldValue
                    }
            in
            ( updatedModel, Task.perform (\posix -> SeedGenerated (Time.posixToMillis posix)) Time.now )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        portalTimerSub =
            if model.portalTimeRemaining > 0 then
                Time.every 1000 (\_ -> PortalTick)

            else
                Sub.none

        -- Calculate timing interval: cap at 60fps (16.67ms) for performance, but allow slower speeds
        gameTimerInterval =
            max 16 (round (25.0 / model.gameSpeed))
    in
    Sub.batch
        [ Time.every (toFloat gameTimerInterval) (always UpdateMiners) -- Speed-adjustable game logic (default 40fps = 25ms intervals)
        , Time.every 1000 (GameTick << Time.posixToMillis) -- Timer updates every second
        , Browser.Events.onKeyDown (Decode.map KeyDown (Decode.field "keyCode" Decode.int))
        , Browser.Events.onKeyUp (Decode.map KeyUp (Decode.field "keyCode" Decode.int))
        , portalTimerSub -- Portal countdown timer
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = \model -> { title = "Miners4k Elm - Level " ++ String.fromInt (model.currentLevel + 1), body = [ view model ] }
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
