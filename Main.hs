import PolyhedraIntersection
import Text.Show.Pretty

main :: IO ()
main = do
  mesh <- test
  pPrint mesh
