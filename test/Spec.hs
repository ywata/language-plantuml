
import Test.Hspec

import Language.PlantUML.ParserSpec
main :: IO()
main = hspec spec1

spec1 :: Spec
spec1 = do
  describe "Language.PlantUML.Parser" Language.PlantUML.ParserSpec.spec

