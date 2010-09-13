import Control.Concurrent (threadDelay)
import World
import qualified Conway

width = 79
height = 39

iterateM :: Monad m => (a -> m a) -> a -> m ()
iterateM f x = f x >>= iterateM f

step :: (World Conway.CellState) -> IO (World Conway.CellState)
step w = do
  putStr $ render Conway.renderCell w
  threadDelay 3000
  return $ evolve Conway.evolveCell w

main :: IO ()
main = do
  world <- Conway.randomWorld width height
  iterateM step world
