import Test.HUnit
import Test.QuickCheck
import qualified Data.Set as Set
import Minesweeper

testInitGame :: Test
testInitGame = TestCase $ do
    case initGame 5 5 3 (0, 0) of
      Left errMsg -> assertFailure $ "initGame failed: " ++ errMsg
      Right action -> do
        game <- action
        assertEqual "Game should have correct dimensions" (height game, width game) (5, 5)
        assertBool "Game should have the correct number of mines" (Set.size (mines game) == 3)
        assertBool "No cells should be revealed initially" (Set.null (revealed game))
        assertBool "No cells should be flagged initially" (Set.null (flagged game))

testRevealSafeCell :: Test
testRevealSafeCell = TestCase $ do
    let game = Minesweeper 3 3 (Set.fromList [(1, 1)]) Set.empty Set.empty
    let updatedGame = makeMove game (0, 0)
    assertBool "Safe cell should be revealed" (Set.member (0, 0) (revealed updatedGame))
    let n = nearbyMines updatedGame (0, 0)
    assertEqual "Nearby mines count should be correct" n 1

testRevealAdjacentCells :: Test
testRevealAdjacentCells = TestCase $ do
    let game = Minesweeper 3 3 (Set.fromList [(2, 2)]) Set.empty Set.empty
    let updatedGame = makeMove game (0, 0)
    let expectedRevealed = Set.fromList [(0, 0), (0, 1), (1, 0), (1, 1), (0, 2), (1, 2), (2, 0), (2, 1)]
    assertBool "All adjacent cells should be revealed" (Set.isSubsetOf expectedRevealed (revealed updatedGame))

testHitMine :: Test
testHitMine = TestCase $ do
    let game = Minesweeper 3 3 (Set.fromList [(1, 1)]) Set.empty Set.empty
    assertBool "Cell (1,1) should be a mine" (isMine game (1, 1))

testWinCondition :: Test
testWinCondition = TestCase $ do
    let game = Minesweeper 2 2 (Set.fromList [(0, 0)]) (Set.fromList [(1, 0), (0, 1), (1, 1)]) Set.empty
    assertBool "Player should win when all safe cells are revealed" (won game)

testLargeWinCondition :: Test
testLargeWinCondition = TestCase $ do
    let game = Minesweeper 5 5 (Set.fromList [(0, 0), (1, 1)]) (Set.fromList [(i, j) | i <- [0..4], j <- [0..4], (i, j) /= (0, 0), (i, j) /= (1, 1)]) Set.empty
    assertBool "Player should win on a larger grid when all safe cells are revealed" (won game)

testInvalidMoves :: Test
testInvalidMoves = TestCase $ do
    let game = Minesweeper 3 3 (Set.fromList [(1, 1)]) Set.empty Set.empty
    let updatedGame = makeMove game (0, 0)
    assertBool "Revealing an already revealed cell should do nothing" (makeMove updatedGame (0, 0) == updatedGame)
    
testFlagging :: Test
testFlagging = TestCase $ do
    let game = Minesweeper 3 3 Set.empty Set.empty Set.empty
    let flaggedGame = game {flagged = Set.insert (0, 0) (flagged game)}
    assertBool "Cell (0,0) should be flagged" (Set.member (0, 0) (flagged flaggedGame))
    let unflaggedGame = flaggedGame {flagged = Set.delete (0, 0) (flagged flaggedGame)}
    assertBool "Cell (0,0) should be unflagged" (not (Set.member (0, 0) (flagged unflaggedGame)))

testFlagEdgeCases :: Test
testFlagEdgeCases = TestCase $ do
    let game = Minesweeper 3 3 Set.empty (Set.fromList [(0, 0)]) Set.empty
    let flaggedGame = flagCell game (0, 0)
    assertBool "Revealed cell cannot be flagged" (not (Set.member (0, 0) (flagged flaggedGame)))

testGameLoss :: Test
testGameLoss = TestCase $ do
    let game = Minesweeper 3 3 (Set.fromList [(1, 1)]) Set.empty Set.empty
    let updatedGame = makeMove game (1, 1)
    assertBool "Game should end with a loss when a mine is revealed" (isMine game (1, 1) && Set.member (1, 1) (revealed updatedGame))

propMinesWithinBounds :: Int -> Int -> Int -> Property
propMinesWithinBounds h w m =
    h > 0 && w > 0 && m > 0 && h * w > 1 && m <= max 1 (h * w - 1) ==> ioProperty $ do
        case initGame h w m (0, 0) of
            Left _ -> return False
            Right action -> do
                game <- action
                let minesWithinBounds = all (\(x, y) -> x >= 0 && x < h && y >= 0 && y < w) (Set.toList (mines game))
                return minesWithinBounds

propDynamicGridSizes :: Int -> Int -> Property
propDynamicGridSizes h w =
    h > 0 && w > 0 && h * w > 1 ==> ioProperty $ do 
        let m = max 1 ((h * w) `div` 6) 
        case initGame h w m (0, 0) of
            Left _ -> return False 
            Right action -> do
                game <- action
                return (height game == h && width game == w)

tests :: Test
tests =
    TestList
        [ TestLabel "Game Initialization" testInitGame,
          TestLabel "Reveal Safe Cell" testRevealSafeCell,
          TestLabel "Hit Mine" testHitMine,
          TestLabel "Win Condition" testWinCondition,
          TestLabel "Invalid Moves" testInvalidMoves,
          TestLabel "Flagging and Unflagging" testFlagging,
          TestLabel "Reveal Adjacent Cells" testRevealAdjacentCells,
          TestLabel "Game Loss" testGameLoss,
          TestLabel "Flag Edge Cases" testFlagEdgeCases,
          TestLabel "Large Win Condition" testLargeWinCondition
        ]

main :: IO ()
main = do
    _ <- runTestTT tests
    quickCheck propMinesWithinBounds
    quickCheck propDynamicGridSizes
