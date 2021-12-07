import Test.Hspec
import qualified TimeSpec

main :: IO ()
main =
  hspec spec

spec :: Spec
spec = do
  TimeSpec.spec
