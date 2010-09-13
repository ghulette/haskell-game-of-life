module World where

import Data.Array

-- Utility functions

chunk :: Int -> [a] -> [[a]]
chunk _ [] = [[]]
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt n xs
  
-- World stuff

type Loc = (Int,Int)
type Offset = (Int,Int)
type Rect = (Loc,Loc)

data World a = World {
  worldBounds :: Rect,
  worldCells  :: Array Loc a
} deriving Eq

instance Functor World where
  fmap f (World sz cells) = World sz (fmap f cells)

instance Show a => Show (World a) where
  show = render (head . show)

cells :: World a -> [a]
cells w = map ((!) (worldCells w)) (locationsIn (worldBounds w))

cellAt :: World a -> Loc -> a
cellAt (World _ cels) x = cels ! x

inside :: World a -> Loc -> Bool
inside w = inRange (worldBounds w)

locationsIn :: Rect -> [Loc]
locationsIn ((x1,y1),(x2,y2)) = [(x,y) | y <- [y1..y2], x <- [x1..x2]]

world :: Rect -> (Loc -> a) -> World a
world bounds f = World { worldBounds = bounds, worldCells = cels}
  where locs = locationsIn bounds
        states = map f locs
        cels = array bounds (zip locs states)

fromList :: Rect -> [a] -> World a
fromList bounds xs = World { worldBounds = bounds, worldCells = cels}
  where locs = locationsIn bounds
        cels = array bounds (zip locs xs)

neighbors :: World a -> [Offset] -> Loc -> [Loc]
neighbors w deltas (x,y) = filter (inside w) locs
  where locs = map (\(dx,dy) -> (x+dx,y+dy)) deltas

render :: (a -> Char) -> World a -> String
render f w = unlines $ chunk n $ fmap f $ cells w
  where ((x1,y1),(x2,y2)) = worldBounds w
        n = (x2 - x1) + 1

evolve :: (World a -> Loc -> a -> a) -> World a -> World a
evolve f w = World bounds cels'
  where bounds = worldBounds w
        cels = worldCells w
        grid = locationsIn bounds
        cels' = array bounds [(x,f w x (cels!x)) | x <- grid]
