module AmazonsSpec where
--
import Test.Hspec
import qualified Data.Map.Strict as M (Map (..), fromList, lookup)
--
spec :: Spec
spec = do
    describe "Traditional Board" $ do
        it "is a 10 x 10 board" $ do
            let Board s ts = traditionalBoard

            length ts `shouldBe` 100
            s `shouldBe` 10

        it "has 4 pieces per player" $ do
            let Board _ ts = traditionalBoard

            (length $ filter isWhite ts) `shouldBe` 4
            (length $ filter isBlack ts) `shouldBe` 4

    -- describe "Amazon" $ do
    --     it "can move horizontally" $ do
    --         let
    --             from = 0
    --             to = 3
    --             color = White
    --             board = newBoard [from] [] [] 4

    --             turn = Turn from to
    --             actual = validateMove board color from to
    --         actual `shouldBe` True





--------------------------------------------------------
data Board  = Board Size [Tile] deriving (Eq,Show)
data Color  = Black | White deriving (Eq,Show)
data Player = Player Color deriving (Eq,Show)
type Size   = Int
data Tile   = Empty | Amazon Color | Fire deriving (Eq,Show)

type MoveFrom = Int
type MoveTo   = Int
data Turn     = Turn MoveFrom MoveTo

validateMove :: Board -> Color -> MoveFrom -> MoveTo -> Bool
validateMove b c f t = False

traditionalBoard :: Board
traditionalBoard = newBoard starts 10
    where
        set c i = (i, c)
        whites = set (Amazon White) <$> [3,6,30,39]
        blacks = set (Amazon Black) <$> [60,69,93,96]
        starts = (M.fromList (whites ++ blacks))

newBoard :: M.Map Int Tile -> Int -> Board
newBoard starts size = Board size tiles
    where
        tiles = setTile <$> [1..(size*size)]
        setTile i = case M.lookup i starts of
                        (Just t) -> t
                        Nothing  -> Empty

isWhite :: Tile -> Bool
isWhite (Amazon White) = True
isWhite _     = False

isBlack :: Tile -> Bool
isBlack (Amazon Black) = True
isBlack _     = False

isEmpty :: Tile -> Bool
isEmpty Empty = True
isEmpty _     = False