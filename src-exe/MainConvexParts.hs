import           ConvexParts
import qualified Data.IntMap.Strict    as IM
import           Text.Show.Pretty
import           Types

main :: IO ()
main = do
  meshes <- testConvexParts
  pPrint meshes
