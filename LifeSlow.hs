-- Conway's Game of Life.  This implementation is interesting mainly because
-- of its poor performance.  In particular, this illustrates why lazy
-- evaluation != memoization.

import Control.Monad (replicateM)
import System.Random

type Time = Int
type Loc = (Int,Int)

data CellState = Alive | Dead
  deriving (Eq,Show)

size = 10

randomCell :: IO CellState
randomCell = do
  x <- randomIO
  return (if x then Alive else Dead)

renderCells :: [CellState] -> String
renderCells [] = []
renderCells xs = (map cellChar (take size xs)) ++ 
                 '\n' : (renderCells (drop size xs))
  where cellChar Alive = '@'
        cellChar Dead  = ' '

neighbors :: Loc -> [Loc]
neighbors (x,y) = map (\(dx,dy) -> (x+dx,y+dy))
  [(-1,-1),(0,-1),(1,-1),
   (-1, 0),       (1, 0),
   (-1, 1),(0, 1),(1, 1)]

transition :: CellState -> Int -> CellState
transition Dead  n | n == 3           = Alive
transition Alive n | n == 2 || n == 3 = Alive
transition _ _                        = Dead

alive :: (Loc -> CellState) -> Time -> Loc -> CellState
alive _ _ (x,y) | x < 0 || x >= size || y < 0 || y >= size = Dead
alive q 0 u = q u
alive q t u = transition (ancestorAt u) neighborCount
  where ancestorAt = alive q (t-1)
        neighborStates = map ancestorAt (neighbors u)
        neighborCount = length (filter ((==) Alive) neighborStates)

frame :: (Loc -> CellState) -> Time -> [CellState]
frame f t = map (alive f t) grid
  where grid = [(x,y) | x <- take size [0..], y <- take size [0..]]

main :: IO ()
main = do
  initialStates <- replicateM (size * size) randomCell
  let initf = \(x,y) -> initialStates !! (y * size + x)
  let frames = map (frame initf) [0..]
  let frameStrs = map renderCells frames
  mapM_ putStr frameStrs
