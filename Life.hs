-- This version is fast(er).

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM, liftM)
import Data.Array
import System.Random

-- Generic utility functions

chunk :: Int -> [a] -> [[a]]
chunk _ [] = [[]]
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt n xs
  
count :: Eq a => a -> [a] -> Int
count x xs = length (filter ((==) x) xs)

iterateM :: Monad m => (a -> m a) -> a -> m ()
iterateM f x = f x >>= iterateM f

-- Game of Life

data CellState = Alive | Dead
  deriving (Eq,Show)
  
type Loc = (Int,Int)
type World = Array Loc CellState

worldSize = 40
worldRange = ((0,0),(worldSize-1,worldSize-1))
worldGrid = [(x,y) | x <- take worldSize [0..], y <- take worldSize [0..]]

randomCell :: IO CellState
randomCell = do
  x <- randomIO
  return (if x then Alive else Dead)
  
randomWorld :: IO World
randomWorld = do
  states <- replicateM (length worldGrid) randomCell
  return $ array worldRange (zip worldGrid states)

neighbors :: Loc -> [Loc]
neighbors (x,y) = filter (inRange worldRange) locs
  where locs = map (\(dx,dy) -> (x+dx,y+dy)) deltas
        deltas = [(-1,-1),(0,-1),(1,-1),
                  (-1, 0),       (1, 0),
                  (-1, 1),(0, 1),(1, 1)]

neighborsAlive :: World -> Loc -> Int
neighborsAlive w x = count Alive allNeighbors
  where allNeighbors = map (w!) (neighbors x)

transition :: CellState -> Int -> CellState
transition Dead  n | n == 3           = Alive
transition Alive n | n == 2 || n == 3 = Alive
transition _ _                        = Dead

evolveCell :: World -> Loc -> CellState -> CellState
evolveCell w x s = transition s (neighborsAlive w x)

evolve :: World -> World
evolve w = array worldRange [(x,evolveCell w x (w!x)) | x <- worldGrid]

renderWorld :: World -> String
renderWorld w = unlines $ chunk worldSize $ elems $ fmap cellChar w
  where cellChar Alive = '@'
        cellChar Dead  = ' '

step :: World -> IO World
step w = do
  putStr $ renderWorld w
  threadDelay 2000
  return $ evolve w

main :: IO ()
main = do 
  world <- randomWorld
  iterateM step world
