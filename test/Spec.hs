import FlakePin.FlakeDirPathSpec qualified
import FlakePin.FlakeInputNameSpec qualified
import Test.Hspec.Core.Runner (hspec)
import Test.Hspec.Core.Spec (describe)

main :: IO ()
main = hspec $ do
    describe "FlakePin.FlakeInputNameSpec" FlakePin.FlakeInputNameSpec.spec
    describe "FlakePin.FlakeDirPathSpec" FlakePin.FlakeDirPathSpec.spec
