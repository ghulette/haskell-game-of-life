import World
import Graphics
import qualified Conway

colorCell :: Conway.CellState -> PatchColor
colorCell Conway.Alive = blue
colorCell Conway.Dead = black

main :: IO ()
main = do
  world <- Conway.randomWorld 100 100
  doGraphics colorCell (evolve Conway.evolveCell) world
