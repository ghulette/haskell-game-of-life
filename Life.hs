-- This version is fast(er).

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import System.Random
import qualified World as W

-- Generic utility functions
  
count :: Eq a => a -> [a] -> Int
count x xs = length (filter ((==) x) xs)

iterateM :: Monad m => (a -> m a) -> a -> m ()
iterateM f x = f x >>= iterateM f

-- Game of Life

type World = W.World
type Loc = W.Loc

data CellState = Alive | Dead
  deriving (Eq,Show)

width = 40
height = 79

randomCell :: IO CellState
randomCell = do
  x <- randomIO
  return (if x then Alive else Dead)
  
randomWorld :: IO (World CellState)
randomWorld = do
  states <- replicateM (width * height) randomCell
  return $ W.fromList (width,height) states

neighborsAlive :: World CellState -> Loc -> Int
neighborsAlive w x = count Alive allNeighbors
  where allNeighbors = map (W.cellAt w) (W.neighbors w d x)
        d = [(-1,-1),(0,-1),(1,-1),
             (-1, 0),       (1, 0),
             (-1, 1),(0, 1),(1, 1)]

transition :: CellState -> Int -> CellState
transition Dead  n | n == 3           = Alive
transition Alive n | n == 2 || n == 3 = Alive
transition _ _                        = Dead

evolveCell :: World CellState -> Loc -> CellState -> CellState
evolveCell w x s = transition s (neighborsAlive w x)

renderCell :: CellState -> Char
renderCell Alive = '@'
renderCell Dead  = ' '

step :: World CellState -> IO (World CellState)
step w = do
  putStr $ W.render renderCell w
  threadDelay 3000
  return $ W.evolve evolveCell w

main :: IO ()
main = do 
  world <- randomWorld
  iterateM step world
