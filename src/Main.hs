import World
import Graphics
import qualified Conway

colorCell :: Conway.CellState -> PatchColor
colorCell Conway.Alive = white
colorCell Conway.Dead  = black

main :: IO ()
main = do
  world <- Conway.randomWorld 200 200
  doGraphics colorCell (evolve Conway.evolveCell) world
