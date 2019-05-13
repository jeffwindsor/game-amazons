module AmazonsSpec where
--
import Test.Hspec
import qualified Data.Set as S (fromList, member)
--
spec :: Spec
spec = do
    describe "Traditional Board" $ do
        it "starts with an 10 x 10 board" $ do
            let Board s ts = traditionalBoard
            length ts `shouldBe` 100
            s `shouldBe` 10

        it "starts with 4 pieces per player" $ do
            let
                Board _ ts = traditionalBoard
            (length $ filter isWhite ts) `shouldBe` 4
            (length $ filter isBlack ts) `shouldBe` 4

    -- describe "Player" $ do
    --     it "can move peice horizontally right" $ do
    --         let
    --             board = newBoard [0] [] [] 16



--------------------------------------------------------
data Board = Board Size [Tile]
type Size = Integer

traditionalBoard :: Board
traditionalBoard = newBoard [3,6,30,39] [60,69,93,96] [] 10

newBoard :: [Integer] -> [Integer] -> [Integer] -> Integer -> Board
newBoard whites blacks fires size = Board size tiles
    where
        tiles = setTile <$> [1..(size*size)]
        wset = S.fromList whites
        bset = S.fromList blacks
        fset = S.fromList fires
        setTile i
            | S.member i wset = White
            | S.member i bset = Black
            | S.member i fset = Fire
            | otherwise       = Empty

data Tile = Empty | White | Black | Fire
    deriving (Eq,Show)

isWhite :: Tile -> Bool
isWhite White = True
isWhite _     = False

isBlack :: Tile -> Bool
isBlack Black = True
isBlack _     = False