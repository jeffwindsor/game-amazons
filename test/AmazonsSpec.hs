module AmazonsSpec where
--
import Test.Hspec

import Data.Text (Text)
import qualified Data.Map.Strict as M (Map (..), (!?), delete, fromList, toList, lookup, alter)
--
spec :: Spec
spec = do
    describe "Traditional Board" $ do
        it "is a 10 x 10 board" $ do
            let Board s ts = traditionalBoard
            s `shouldBe` 10

        it "has 4 pieces per player" $ do
            let
                Board _ tiles = traditionalBoard
                count' f = length $ filter f $ map snd $ M.toList tiles
            count' isWhite `shouldBe` 4
            count' isBlack `shouldBe` 4

    describe "Player" $ do
        it "can move own piece horizontally" $ do
            let color = White
                row = 0
                from = (0,row)
                to = (9,row)
                board = Board 10 $ M.fromList [(from, Amazon color)]
                actual = move color from to board
                expected = Right $ Board 10 $ M.fromList [(to, Amazon color)]
            actual `shouldBe` expected

        it "cannot move other player's piece" $ do
            let from = (0,0)
                to = (9,9)
                board = Board 10 $ M.fromList [(from, Amazon Black)]
                actual = move White from to board
                expected = Left CannotMoveFromTile
            actual `shouldBe` expected

        it "cannot move fire" $ do
            let from = (0,0)
                to = (9,9)
                board = Board 10 $ M.fromList [(from, Fire)]
                actual = move Black from to board
                expected = Left CannotMoveFromTile
            actual `shouldBe` expected

        it "cannot move empty space" $ do
            let from = (0,0)
                to = (9,9)
                board = Board 10 mempty
                actual = move Black from to board
                expected = Left InvalidFromTile
            actual `shouldBe` expected

        it "cannot move out of bounds" $ do
            let from = (10,10)
                to = (9,9)
                board = Board 10 mempty
                actual = move Black from to board
                expected = Left InvalidFromTile
            actual `shouldBe` expected
        -- it "cannot move over fire" $ do
        --     testValidateMove White White 0 9 [(4,Fire)] `shouldBe` False

--------------------------------------------------------
type Size   = Int
type Tiles  = M.Map Coordinate Tile
type Column = Int
type Row    = Int
type Coordinate = (Column, Row)

data Board  = Board Size Tiles deriving (Eq,Show)
data Color  = Black | White deriving (Eq,Show)
data Tile   = Amazon Color | Fire deriving (Eq,Show)
data MoveError =  CannotMoveFromTile
                | InvalidFromTile
                -- | InvalidTo
                    deriving (Eq,Show)

-- BOARD
move :: Color -> Coordinate -> Coordinate -> Board -> Either MoveError Board
move playerColor from to (Board size tiles) =
    case M.lookup from tiles of
        (Just (Amazon c)) | c == playerColor ->
            let mFromTile = tiles M.!? from
            in Right $ Board size $ M.delete from $ M.alter (const mFromTile) to tiles
        (Just _ ) -> Left CannotMoveFromTile
        Nothing   -> Left InvalidFromTile


traditionalBoard :: Board
traditionalBoard = Board 10 $ buildTilesByType [(3,0),(6,0),(0,2), (9,2)] [(0,6),(9,6),(3,9),(6,9)] mempty

buildTilesByType :: [Coordinate] -> [Coordinate] -> [Coordinate] -> Tiles
buildTilesByType whites blacks fires = (M.fromList (ws ++ bs ++ fs))
    where set c i = (i, c)
          ws = set (Amazon White) <$> whites
          bs = set (Amazon Black) <$> blacks
          fs = set Fire <$> fires

-- TILES
isPathEmpty :: Coordinate -> Coordinate -> Board -> Bool
isPathEmpty = undefined

-- TILE
isWhite :: Tile -> Bool
isWhite (Amazon White) = True
isWhite _     = False

isBlack :: Tile -> Bool
isBlack (Amazon Black) = True
isBlack _     = False
