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
      parseLine [] "KIKOO PAUL 127.0.0.3:3000 ASV" `shouldBe` (["127.0.0.3:3000"],  "OKLM \"SuckMyLambdaCalculus\" / 127.0.0.1:3000")

     it "KIKOO with existing participants" $ do
      parseLine ["127.0.0.2:3000"] "KIKOO PAUL 127.0.0.3:3000 ASV" `shouldBe` (["127.0.0.2:3000", "127.0.0.3:3000"], "OKLM \"SuckMyLambdaCalculus\" / 127.0.0.1:3000 / 127.0.0.2:3000")

     it "TAVU" $ do 
      parseLine [] "TAVU \"As-tu vu les belles quenouilles ?\"" `shouldBe` ([], "ACK")

     it "unrecognized command" $ do
      parseLine [] "BLEBLE \"Lemme chat with u \"" `shouldBe` ([], "WTF \"Talk my language u foreigner\"")
