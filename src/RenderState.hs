
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}


{-|
This module defines the board. A board is an array of CellType elements indexed by a tuple of ints: the height and width.

for example, The following array represents a 3 by 4 board (left top corner is (1,1); right bottom corner is (3,4)) with a snake at 
(2, 2) and (2, 3) and an apple at (3,4)

< ((1,1) : Empty), ((1,2) : Empty), ((1,3) : Empty),     ((1,2) : Empty)
, ((2,1) : Empty), ((2,2) : Snake)  ((2,3) : SnakeHead)  ((2,4) : Empty)
, ((3,1) : Empty), ((3,2) : Empty), ((3,3) : Empty),     ((3,4) : Apple) >

Which would look like this:

- - - -
- 0 $ -
- - - X


-}
module RenderState where

-- This are all imports you need. Feel free to import more things.
import Data.Array ( (//), listArray, Array, elems)
import Data.Foldable ( foldl' )
import Data.List (intercalate, intersperse)
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder
import Data.Monoid(mempty)

-- A point is just a tuple of integers.
type Point = (Int, Int)

-- | Cell types. We distinguish between Snake and SnakeHead
data CellType = Empty | Snake | SnakeHead | Apple deriving (Show, Eq)

-- | The board info is just a description of height and width.
data BoardInfo = BoardInfo {height :: Int, width :: Int} deriving (Show, Eq)
type Board = Array Point CellType     -- ^The board is an Array indexed by points with elements of type CellType

-- | A delta is a small change in the board at some points. For example [((2,2), SnakeHead), ((2,1), Empty)]
--   would represent the change "cell (2,2) should change to become the SnakeHead and cell (2,1) should change by an empty cell"
type DeltaBoard = [(Point, CellType)]

-- | The render message represent all message the GameState can send to the RenderState
--   Right now Possible messages are a RenderBoard with a payload indicating which cells change
--   or a GameOver message.
data RenderMessage = RenderBoard DeltaBoard | UpdateScore Int | GameOver deriving Show

-- | The RenderState contains the board and if the game is over or not.
data RenderState   = RenderState {board :: Board, gameOver :: Bool, score :: Int} deriving Show

-- | Given The board info, this function should return a board with all Empty cells
emptyGrid :: BoardInfo -> Board
emptyGrid (BoardInfo h w) = listArray ((1,1), (h, w)) (repeat Empty)


{- 
This is a test for emptyGrid. It should return 
array ((1,1),(2,2)) [((1,1),Empty),((1,2),Empty),((2,1),Empty),((2,2),Empty)]
-}
-- >>> emptyGrid (BoardInfo 2 2)


-- | Given BoardInfo, initial point of snake and initial point of apple, builds a board
buildInitialBoard 
  :: BoardInfo -- ^ Board size
  -> Point     -- ^ initial point of the snake
  -> Point     -- ^ initial Point of the apple
  -> RenderState
buildInitialBoard bInfo s a = RenderState {board = b'', gameOver = False, score = 0}
  where
    b   = emptyGrid bInfo
    b'  = b//[(s, SnakeHead)]
    b'' = b'//[(a, Apple)]

{- 
This is a test for buildInitialBoard. It should return 
RenderState {board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = False}
-}
-- >>> buildInitialBoard (BoardInfo 2 2) (1,1) (2,2)

updateMessages :: RenderState -> [RenderMessage] -> RenderState
updateMessages rs []     = rs
updateMessages rs (x:xs) = updateMessages (updateRenderState rs x) xs 
 
-- | Given the current render state, and a message -> update the render state
updateRenderState :: RenderState -> RenderMessage -> RenderState
updateRenderState (RenderState{board = b, gameOver = o, score = s}) rm = case rm of 
  RenderBoard rb -> (RenderState{board = b//rb, gameOver = o, score = s})
  UpdateScore us -> (RenderState{board = b, gameOver = o, score   = s+us})
  GameOver       -> (RenderState{board = b, gameOver = True, score = s})

{-
This is a test for updateRenderState

message1 should return:
RenderState {board = array ((1,1),(2,2)) [((1,1),Empty),((1,2),SnakeHead),((2,1),Apple),((2,2),Apple)], gameOver = False}

message2 should return:
RenderState {board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = True}
-}
-- >>> initial_board =  buildInitialBoard (BoardInfo 2 2) (1,1) (2,2)
-- >>> message1 = RenderBoard [((1,2), SnakeHead), ((2,1), Apple), ((1,1), Empty)]
-- >>> message2 = GameOver
-- >>> updateRenderState initial_board message1
-- >>> updateRenderState initial_board message2


-- | Provisional Pretty printer
--   For each cell type choose a string to representing. 
--   a good option is
--     Empty -> "- "
--     Snake -> "0 "
--     SnakeHead -> "$ "
--     Apple -> "X "
--   In other to avoid shrinking, I'd recommend to use some character followed by an space.
ppCell :: CellType -> Builder
ppCell Empty     = "- "
ppCell Snake     = "0 "
ppCell SnakeHead = "$ "
ppCell Apple     = "X "

ppScore :: Int -> Builder
ppScore n = ast <> s <> ast 
  where
    ast = "+++++++++++\n"
    s = "Score: " <> (B.intDec n) <> "\n"
  
-- | convert the RenderState in a String ready to be flushed into the console.
--   It should return the Board with a pretty look. If game over, return the empty board.
render :: BoardInfo -> RenderState -> Builder
render bi@(BoardInfo maxX maxY) rs@(RenderState b g s) = if g
  then printBoard (emptyGrid bi) maxX maxY s
  else printBoard b maxX maxY s


printBoard :: Board -> Int -> Int -> Int -> Builder
printBoard b h w s = b' <> "\n\n" <> score 
  where
    b'    = fst $ foldl' fprint (mempty, 0) b -- (foldl' (\ a b -> a <> ppCell b ) "" b) 
    score = ppScore s
    fprint (!s, !i) c = if ((i + 1) `mod` w) == 0
        then (s <> ppCell c <> B.charUtf8 '\n', i + 1 )
        else (s <> ppCell c , i + 1)




{-
This is a test for render. It should return:
"- - - - \n- 0 $ - \n- - - X \n"

- - - -
- 0 $ -
- - - X

Notice, that this depends on what you've chosen for ppCell
-}
-- >>> board = listArray ((1,1), (3,4)) [Empty, Empty, Empty, Empty, Empty, Snake, SnakeHead, Empty, Empty, Empty, Empty, Apple]
-- >>> board_info = BoardInfo 3 4
-- >>> render_state = RenderState board  False
-- >>> render board_info render_state
-- "- - - - \n- 0 $ - \n- - - X \n"
