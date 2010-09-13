module World where

import Data.Array

-- Generic utility functions

chunk :: Int -> [a] -> [[a]]
chunk _ [] = [[]]
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt n xs
  
-- World stuff

type Loc = (Int,Int)
type Size = Loc

data World a = World {
  worldSize  :: Size,
  worldCells :: Array Loc a
}

instance Functor World where
  fmap f (World size cells) = World size (fmap f cells)

boundaries :: Size -> (Loc,Loc)
boundaries (x,y) = ((0,0),(x-1,y-1))

cells :: World a -> [a]
cells w = elems $ (worldCells w)

cellAt :: World a -> Loc -> a
cellAt (World _ cels) x = cels ! x

inside :: World a -> Loc -> Bool
inside w = inRange (boundaries (worldSize w))

locations :: (Loc,Loc) -> [Loc]
locations ((x1,y1),(x2,y2)) = [(x,y) | x <- [x1..x2], y <- [y1..y2]]

world :: Size -> (Loc -> a) -> World a
world size genf = 
  World { worldSize = size, worldCells = array r (zip locs (map genf locs))}
  where r = boundaries size
        locs = locations r

fromList :: Size -> [a] -> World a
fromList size xs = 
  World { worldSize = size, worldCells = array r (zip locs xs)}
  where r = boundaries size
        locs = locations r

neighbors :: World a -> [Loc] -> Loc -> [Loc]
neighbors w deltas (x,y) = filter (inside w) locs
  where locs = map (\(dx,dy) -> (x+dx,y+dy)) deltas

render :: (a -> Char) -> World a -> String
render f w = unlines $ chunk (snd . worldSize $ w) $ fmap f $ cells w

evolve :: (World a -> Loc -> a -> a) -> World a -> World a
evolve f w = World sz cels'
  where sz = worldSize w
        cels = worldCells w
        r = boundaries sz
        grid = locations r
        cels' = array r [(x,f w x (cels!x)) | x <- grid]
