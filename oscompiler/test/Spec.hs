import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Lib" $ do
    it "does not have a test suite" $
      True `shouldBe` True
