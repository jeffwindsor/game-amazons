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

    describe "Amazon" $ do
        it "can move own color horizontally" $ do
            testValidateMove White White 0 9 `shouldBe` True


testValidateMove playerColor amazonColor from to =
    let board = occupyBoard (M.fromList [(from, Amazon amazonColor)]) 10
    in validateMove board playerColor from to

--------------------------------------------------------
data Board  = Board Size [Tile] deriving (Eq,Show)
data Color  = Black | White deriving (Eq,Show)
data Player = Player Color deriving (Eq,Show)
type Size   = Int
data Tile   = Empty | Amazon Color | Fire deriving (Eq,Show)
type TileMap = M.Map Int Tile

type MoveFrom = Int
type MoveTo   = Int
data Turn     = Turn MoveFrom MoveTo

validateMove :: Board -> Color -> MoveFrom -> MoveTo -> Bool
validateMove b c f t = True

traditionalBoard :: Board
traditionalBoard = occupyBoard occupiedTiles 10
    where occupiedTiles = tileMap [3,6,30,39] [60,69,93,96] mempty

tileMap :: [Int] -> [Int] -> [Int] -> TileMap
tileMap whites blacks fires = (M.fromList (ws ++ bs ++ fs))
    where set c i = (i, c)
          ws = set (Amazon White) <$> whites
          bs = set (Amazon Black) <$> blacks
          fs = set Fire <$> fires

occupyBoard :: TileMap -> Int -> Board
occupyBoard occupiedTiles size = Board size tiles
    where tiles = setTile <$> [1..(size*size)]
          setTile i = case M.lookup i occupiedTiles of
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