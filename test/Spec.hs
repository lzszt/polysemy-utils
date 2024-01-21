import CacheSpec qualified
import Test.Hspec
import TimeSpec qualified

main :: IO ()
main =
  hspec spec

spec :: Spec
spec = do
  TimeSpec.spec

-- CacheSpec.spec
