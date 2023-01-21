{-|
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where 

-- These are all the import. Feel free to use more if needed.
import RenderState (BoardInfo (..), Point, DeltaBoard)
import qualified RenderState as Board
import Data.Sequence ( Seq(..), empty)
import qualified Data.Sequence as S
import System.Random ( uniformR, RandomGen(split), StdGen, Random (randomR))
import Data.Maybe (isJust)

-- The movement is one of this.
data Movement = North | South | East | West deriving (Show, Eq)

-- | The snakeSeq is a non-empty sequence. It is important to use precise types in Haskell
--   In first sight we'd define the snake as a sequence, but If you think carefully, an empty 
--   sequence can't represent a valid Snake, therefore we must use a non empty one.
--   You should investigate about Seq type in haskell and we it is a good option for our porpouse.
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point} deriving (Show, Eq)

-- | The GameState represents all important bits in the game. The Snake, The apple, the current direction of movement and 
--   a random seed to calculate the next random apple.
data GameState = GameState
  { snakeSeq :: SnakeSeq
  , applePosition :: Point
  , movement :: Movement
  , randomGen :: StdGen
  }
  deriving (Show, Eq)

-- | This function should calculate the opposite movement.
opositeMovement :: Movement -> Movement
opositeMovement North = South
opositeMovement South = North
opositeMovement East  = West
opositeMovement West  = East

-- >>> opositeMovement North == South
-- >>> opositeMovement South == North
-- >>> opositeMovement East == West
-- >>> opositeMovement West == East


-- | Purely creates a random point within the board limits
--   You should take a look to System.Random documentation. 
--   Also, in the import list you have all relevant functions.
makeRandomPoint :: BoardInfo -> GameState -> (Point, GameState)
makeRandomPoint (BoardInfo maxX maxY) r = (p, r)
  where
    (r1, r2)  = split r
    p  = (fst (uniformR (1, maxX) r1), fst (uniformR (1, maxY) r2))

{-
We can't test makeRandomPoint, because different implementation may lead to different valid result.
-}


-- | Check if a point is in the snake
inSnake :: Point -> SnakeSeq  -> Bool
inSnake p SnakeSeq{snakeHead = head, snakeBody = body} = p == head || not (S.null $ S.filter (==p) body)

{-
This is a test for inSnake. It should return 
True
True
False
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> inSnake (1,1) snake_seq
-- >>> inSnake (1,2) snake_seq
-- >>> inSnake (1,4) snake_seq

-- | Calculates de new head of the snake. Considering it is moving in the current direction
--   Take into acount the edges of the board
nextHead :: BoardInfo -> GameState -> Point
nextHead (BoardInfo maxH maxW) (GameState SnakeSeq{snakeHead = (x, y)} _ m _) = case m of 
  North -> (hN, y)
  South -> (hS, y)
  East  -> (x, wE)
  West  -> (x, wW)
  where
    wW = if y == 1    then maxW else y-1
    wE = if y == maxW then 1    else y+1
    hN = if x == 1    then maxW else x-1
    hS = if x == maxH then 1    else x+1

{-
--        $ 0 0 -          0 0 - $
--        - - - -    =>    - - - -
--        - - - -    =>    - - - -
--        - - - X          - - - X
This is a test for nextHead. It should return
True
True
True
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,2) 
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> nextHead board_info game_state1 == (1,4)
-- >>> nextHead board_info game_state2 == (2,1)
-- >>> nextHead board_info game_state3 == (4,1)


-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: BoardInfo -> GameState -> (Point, StdGen)
newApple b (GameState s a m r)
  | p == a || inSnake p s = makeRandomPoint b (fst $ split r)
  | otherwise = (p, r')
  where
    (p, r') = makeRandomPoint b r



{- We can't test this function because it depends on makeRandomPoint -}


-- | Moves the snake based on the current direction. It sends the adequate RenderMessage
-- Notice that a delta board must include all modified cells in the movement.
-- For example, if we move between this two steps
--        - - - -          - - - -
--        - 0 $ -    =>    - - 0 $
--        - - - -    =>    - - - -
--        - - - X          - - - X
-- We need to send the following delta: [((2,2), Empty), ((2,3), Snake), ((2,4), SnakeHead)]
--
-- Another example, if we move between this two steps
--        - - - -          - - - -
--        - - - -    =>    - X - -
--        - - - -    =>    - - - -
--        - 0 $ X          - 0 0 $
-- We need to send the following delta: [((2,2), Apple), ((4,3), Snake), ((4,4), SnakeHead)]
-- 
{-
--        $ 0 0 -          0 0 - $
--        - - - -    =>    - - - -
--        - - - -    =>    - - - -
--        - - - X          - - - X
-}


-- Returns (new snake, tail point)
nextBody :: SnakeSeq -> Point -> Bool -> (SnakeSeq, Point)
nextBody (SnakeSeq head body) newHead eats
  | eats      = if S.length body >= 1 then (SnakeSeq newHead snake', S.index snake' last) else (SnakeSeq newHead (S.singleton head), head)
  | otherwise = if S.null body then (SnakeSeq newHead S.empty, head) else (SnakeSeq newHead (S.deleteAt last $ snake'), S.index snake' last)
  where
    snake' = head S.<| body
    last   = S.length snake' - 1
    len    = S.length body


eatsApple :: BoardInfo -> GameState -> Bool 
eatsApple bi g@(GameState (SnakeSeq _ b) a _ _) = inSnake a $ SnakeSeq (nextHead bi g) b

lost :: BoardInfo -> GameState -> Bool
lost bi g@(GameState s _ _ _) = inSnake (nextHead bi g) s


move :: BoardInfo -> GameState -> ([Board.RenderMessage] , GameState)
move bi g@(GameState s@SnakeSeq{snakeHead = head, snakeBody = body} a m r)
  | eats       = ([Board.RenderBoard snakeList', Board.UpdateScore 1], GameState newSnake (fst a') m (snd a'))
  | gOver      = ([Board.GameOver], g)
  | otherwise  = ([Board.RenderBoard snakeList], GameState newSnake a m r)
  where
    eats             = eatsApple bi g
    gOver            = lost bi g
    (head', a')      = (nextHead bi g, newApple bi g)
    (newSnake, tail) = nextBody s head' eats
    snakeList        = [(head', Board.SnakeHead), (head, Board.Snake), (tail, Board.Empty)]
    snakeList'       = [(a, Board.SnakeHead), (head, Board.Snake), (fst a', Board.Apple)] 


{- This is a test for move. It should return

RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
RenderBoard [((2,1),SnakeHead),((1,1),Snake),((3,1),Apple)] ** your Apple might be different from mine
RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]

-}

-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,1) 
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> fst $ move board_info game_state1
-- >>> fst $ move board_info game_state2
-- >>> fst $ move board_info game_state3
