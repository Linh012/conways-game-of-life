module Main where

import Graphics.Gloss -- graphics
import Graphics.Gloss.Interface.Pure.Game -- controls

{- This is the main entry point to your program. -}

-- Grid/window space
width :: Float
width = 800

height :: Float
height = 800

-- Graphical window
window  :: Display
window = InWindow "Conway's Game of Life" (round width, round height + 100) (200, 0)

-- x and y axis of the grid
newtype Coordinate = Coordinate (Int, Int) deriving (Eq, Ord, Show)

-- Square grid dimensions (number of cells per edge/side of square grid)
gridLength :: Int
gridLength = 40

-- gridLength as a Float data type
gridLengthF :: Float
gridLengthF = fromIntegral gridLength

-- Grid space to number of cells ratio
ratio :: Float
ratio =  width / gridLengthF

-- Grid of game (map coordinate to each cell in grid)
grid :: [Coordinate]
grid = [ Coordinate (x, y) | x <- [0..(gridLength - 1)], y <- [0..(gridLength - 1)]]


-- A data structure to hold the state of the game
data GameState = GameState {
                running :: Bool,
                livingCells :: [Coordinate],
                speed :: Int,
                time :: Float
                }


-- Initialize the game with this grid
initialState :: GameState
initialState = GameState {
                running = False,
                livingCells = [],
                speed = 15,
                time = 0.0
                }



-- Get neighbors of a cell
adjacent :: Coordinate -> [Coordinate]
adjacent (Coordinate (x, y)) = [ Coordinate (checkEdge a, checkEdge b) | a <- [(x-1)..(x+1)], b <- [(y-1)..(y+1)], (a, b) /= (x, y)]


-- Link edges so moving past an edge makes it reappear on the otherside of grid
checkEdge :: Int -> Int
checkEdge x | x < 0 = gridLength - 1
            | x >= gridLength = 0
            | otherwise = x


-- Given cell (coordinate), check how many neighbors are alive
willSurviveHelper :: (Coordinate -> Bool) -> Coordinate -> Int
willSurviveHelper f cell = length aliveNeighbors
                        where
                            neighbors = adjacent cell
                            aliveNeighbors = filter f neighbors


-- Given cell (coordinate), check if cell will live
-- Living cells need 2-3 neighbors to survive
-- Dead cells need 3 neighbors to become alive
-- Otherwise, living cells die and dead cells stay dead
willSurvive :: (Coordinate -> Bool) -> Coordinate -> Bool
willSurvive f cell = case aliveCount of
                        3 -> True
                        2 -> f cell
                        otherwise -> False
                    where
                        aliveCount = willSurviveHelper f cell


-- Check if given cell (coordinate) is currently alive or dead
isLiving :: GameState -> Coordinate -> Bool
isLiving gameState cell = cell `elem` (livingCells gameState)


-- Set background color of window to white
bgcolor :: Color
bgcolor = white

-- Game title
headingText :: Picture
headingText = Translate (-375) (425) $ Scale 0.14 0.14 $ Text "Conway's Game of Life: Click any squares then press 'Enter' key to start/pause!"

-- Game control guide
controlText :: Picture
controlText = Translate (-375) (-425) $ Scale 0.13 0.13 $ Text "Arrow up/down to adjust speed, 'Spacebar' to move one step/tick, 'Delete' to clear."

-- Grid square (cell grid) 
square :: Picture
square = rectangleSolid ratio ratio

-- Grid lines
gridPicture :: [Picture]
gridPicture =
            let
                horizontalL = \x -> line [ (-(width / 2),  ratio * x - (height / 2)),
                                        ((width - (width / 2),  ratio * x - (height / 2))) ]
                verticalL = \x -> line [ ( ratio * x - (width / 2), -(height / 2)),
                                    ( ratio * x - (width / 2), height - (height / 2))]
                xGridLines = foldr (\a -> \b -> horizontalL a:b) [] [0..gridLengthF]
                yGridLines = foldr (\a -> \b -> verticalL a:b) [] [0..gridLengthF]
            in
                xGridLines ++ yGridLines

-- Draw grid lines into grid area
renderGridLines :: (Float, Float) -> Picture
renderGridLines (x, y) =  translate (x *  ratio - width / 2 +   ratio / 2)
                                    (-y *  ratio + height / 2 -  ratio / 2) square


-- Draw the game state (convert state to picture)
render :: GameState -> Picture
render gameState = pictures $ [headingText] ++ gridPicture ++ aliveCellsPictures ++ [controlText]
    where aliveCellsPictures = [ renderGridLines (fromIntegral a, fromIntegral b) | Coordinate(a,b) <- livingCells gameState]


-- Get the next state of the grid (relative to speed setting)
next :: Float -> GameState -> GameState
next t gameState = gameState {
                    time = (time gameState) + t - (1/fromIntegral (speed gameState)),
                    livingCells = filter (willSurvive (isLiving gameState)) grid
                    }


-- Controls (key handlers)
-- Get next game state by moving forward in time by one tick/step
handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gameState = next 0 gameState

-- Simulate game (pause/unpause time)
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) gameState = gameState { running = if (running gameState) then False else True}

-- Clear grid (deletes all living cells)
handleKeys (EventKey (SpecialKey KeyDelete) Down _ _) gameState = gameState { running = False, livingCells = []}

-- Click on grid squares (cells) to make them live/die
handleKeys (EventKey (MouseButton LeftButton) Down _ (x, y)) gameState =
                            let
                                adjustedX = round $ fromIntegral $ div (mod (floor $ x - (width/2))  (floor width)) (floor (width/gridLengthF))
                                adjustedY = round $ (gridLengthF-1) - (fromIntegral $ div (mod (floor $ y + (height/2))  (floor height)) (floor (height/gridLengthF)))
                            in
                                gameState { livingCells =
                                            if (Coordinate(adjustedX, adjustedY)) `elem` (livingCells gameState)
                                            then filter (\z -> z /= Coordinate(adjustedX, adjustedY))
                                                        (livingCells gameState)
                                            else (livingCells gameState) ++ [Coordinate(adjustedX, adjustedY)] }

-- Change simulation speed (speed wraps around maximum which means speed going below 0 wraps to maximum :D)
-- Increase speed
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gameState = gameState { speed = (speed gameState) + 3 }

-- Decrease speed
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) gameState = gameState { speed = (speed gameState) - 3 }

-- Exhaustive pattern matching - do nothing for other keys
handleKeys _ state = state


-- Update game state automatically if running else maintain game state
updateGrid :: Float -> GameState -> GameState
updateGrid t gameState = if (running gameState)
                         then getNextState
                         else gameState
                        where
                            gameTime = time gameState
                            gameSpeed = 1 / (fromIntegral $ speed gameState)
                            getNextState = if (gameTime >= gameSpeed)
                                           then next t gameState
                                           else gameState { time = t + gameTime}



main :: IO()
main = play window bgcolor 100 initialState render handleKeys updateGrid