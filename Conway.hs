module Conway where

-- This version is fast(er).

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

randomCell :: IO CellState
randomCell = do
  x <- randomIO
  return (if x then Alive else Dead)
  
randomWorld :: Int -> Int -> IO (World CellState)
randomWorld width height = do
  states <- replicateM (width * height) randomCell
  let bounds = ((0,0),(width-1,height-1))
  return $ W.fromList bounds W.Plane states

neighborsAlive :: World CellState -> Loc -> Int
neighborsAlive w x = count Alive neighborStates
  where neighborStates = map (W.cellAt w) (neighbors w x)
        neighbors = W.neighbors W.mooreNeighbors

transition :: CellState -> Int -> CellState
transition Dead  n | n == 3           = Alive
transition Alive n | n == 2 || n == 3 = Alive
transition _ _                        = Dead

evolveCell :: World CellState -> Loc -> CellState -> CellState
evolveCell w x s = transition s (neighborsAlive w x)

renderCell :: CellState -> Char
renderCell Alive = '@'
renderCell Dead  = ' '
