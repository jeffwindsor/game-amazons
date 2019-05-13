module AmazonsSpec where
--
import Test.Hspec
--
spec :: Spec
spec = do
    describe "New Game" $ do
        it "starts with an 8 x 8 board with 3 pieces per player" $ do
            let
                --when
                board = newGame
                --then
            length board `shouldBe` 64


--------------------------------------------------------
data Tile = Empty
type Board = [Tile]

newGame :: Board
newGame = const Empty <$> [1..64]