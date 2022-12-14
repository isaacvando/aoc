import Test.Hspec

import Day13 hiding (main)

main :: IO ()
main = hspec $ do
        describe "tests" $ do
            it "should peel" $ do
                peel "[foo]" `shouldBe` ""
