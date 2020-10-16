import Test.Hspec
import Lib (getCommand, parseLine)

main :: IO ()
main = hspec $ do 
  describe "getCommand" $ do
    it "returns the command from an input" $ do 
      getCommand "KIKOO Paul ASV" `shouldBe` "KIKOO"

  describe "parseLine" $ do
    describe "returns a text response depending on input command" $ do
     it "KIKOO" $ do
      parseLine "KIKOO PAUL ASV" `shouldBe` "OKLM \"SuckMyLambdaCalculus\" ( / 127.0.0.1 3000?)*"

     it "unrecognized command" $ do
      parseLine "BLEBLE \"Lemme chat with u \"" `shouldBe` "ERR \"Talk my language u foreigner\""
