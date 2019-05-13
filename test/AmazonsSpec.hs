module AmazonsSpec where
--
import Test.Hspec
import qualified Data.Set as S (fromList, member)
--
spec :: Spec
spec = do
    describe "New Game" $ do
        it "starts with an 10 x 10 board" $ do
            let board = newGame
            length board `shouldBe` 100

        it "starts with 4 pieces per player" $ do
            let
                board = newGame
            (length $ filter isWhite board) `shouldBe` 4
            (length $ filter isBlack board) `shouldBe` 4

playerCount :: Board -> (Integer, Integer) -> (Integer, Integer)
playerCount (White:ts) (white,black) = playerCount ts (white+1, black)
playerCount (Black:ts) (white,black) = playerCount ts (white, black+1)
playerCount (Empty:ts) count = playerCount ts count
playerCount [] count = count

--------------------------------------------------------
data Tile = Empty | White | Black
    deriving (Eq,Show)

type Board = [Tile]

isWhite :: Tile -> Bool
isWhite White = True
isWhite _     = False

isBlack :: Tile -> Bool
isBlack Black = True
isBlack _     = False

newGame :: Board
newGame = newBoard [3,6,30,39] [60,69,93,96] 100

newBoard ws bs size = setTile <$> [1..size]
    where
        wset = S.fromList ws
        bset = S.fromList bs
        setTile i
            | S.member i wset = White
            | S.member i bset = Black
            | otherwise       = Empty