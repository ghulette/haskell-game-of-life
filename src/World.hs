module World where

import Data.Array

-- Utility functions

chunk :: Int -> [a] -> [[a]]
chunk _ [] = [[]]
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt n xs
  
-- World stuff

type Loc = (Int,Int)
type Offset = Loc
type Rect = (Loc,Loc)

data Shape = Plane | Torus
  deriving (Eq,Show)

data World a = World {
  worldBounds :: Rect,
  worldShape  :: Shape,
  worldCells  :: Array Loc a
} deriving Eq

instance Show a => Show (World a) where
  show = render (head . show)

instance Functor World where
  fmap f (World r shp cells) = World r shp (fmap f cells)

world :: Rect -> Shape -> (Loc -> a) -> World a
world bounds shape f = World bounds shape cels
  where locs = locationsIn bounds
        states = map f locs
        cels = array bounds (zip locs states)

fromList :: Rect -> Shape -> [a] -> World a
fromList bounds shape xs = World bounds shape cels
  where locs = locationsIn bounds
        cels = array bounds (zip locs xs)

cells :: World a -> [a]
cells w = map ((worldCells w)!) (locationsIn (worldBounds w))

cellAt :: World a -> Loc -> a
cellAt (World _ _ cels) x = cels ! x

inside :: World a -> Loc -> Bool
inside w = inRange (worldBounds w)

locationsIn :: Rect -> [Loc]
locationsIn ((x1,y1),(x2,y2)) = [(x,y) | y <- [y1..y2], x <- [x1..x2]]

wrap :: World a -> Loc -> Loc
wrap world (x,y) = 
  case worldShape world of
    Plane -> (x,y)
    Torus -> 
      let 
        ((x1,y1),(x2,y2)) = worldBounds world
        w = x2 - x1
        h = y2 - y1
      in
        ((x `mod` w) + x1,(y `mod` h) + y1)

neighbors :: [Offset] -> World a -> Loc -> [Loc]
neighbors ds w (x,y) = filter (inside w) neighborLocs
  where neighborLocs = map (\(dx,dy) -> wrap' (x+dx,y+dy)) ds
        wrap' = wrap w

cardinalNeighbors :: [Offset]
cardinalNeighbors = [(0,-1),(-1, 0),(1, 0),(0, 1)]

mooreNeighbors :: [Offset]
mooreNeighbors = [(-1,-1),(0,-1),(1,-1),
                  (-1, 0),       (1, 0),
                  (-1, 1),(0, 1),(1, 1)]

render :: (a -> Char) -> World a -> String
render f w = unlines $ chunk n $ fmap f $ cells w
  where ((x1,y1),(x2,y2)) = worldBounds w
        n = (x2 - x1) + 1

evolve :: (World a -> Loc -> a -> a) -> World a -> World a
evolve f w = World bounds shape cels'
  where bounds = worldBounds w
        shape = worldShape w
        cels = worldCells w
        grid = locationsIn bounds
        cels' = array bounds [(x,f w x (cels!x)) | x <- grid]
