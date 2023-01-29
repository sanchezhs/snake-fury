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
import Control.Monad.Trans.State.Strict (State, get, put, modify, gets, runState)

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

type GameStep a = State GameState a

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
makeRandomPoint (BoardInfo maxX maxY) (GameState s a m r) = (p, GameState s a m r)
  where
    (r1, r2)  = split r
    p  = (fst (uniformR (1, maxX) r1), fst (uniformR (1, maxY) r2))

makeRandomPoint' :: BoardInfo -> GameStep Point 
makeRandomPoint' (BoardInfo h w) = do 
  r <- gets randomGen                        -- get state
  let (r1, r2) = split r
      rnd      =  (uniformR (1, h) r1)
  modify $ \ x -> x{randomGen = snd rnd}     -- modify state
  pure (fst rnd, fst (uniformR (1, w) r2))   -- put point inside monad
  

newApple' :: BoardInfo -> GameState Point
newApple' b@(BoardInfo h w) = do
  a <- gets applePosition
  let a' = makeRandomPoint' b 
        


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
newApple :: BoardInfo -> GameState -> (Point, GameState)
newApple b g@(GameState s a m r)
  | p == a || inSnake p s = makeRandomPoint b (GameState s p m (fst $ split r)) 
  | otherwise = (p, g')
  where
    (p, g') = makeRandomPoint b g



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

extendSnake :: Point -> BoardInfo -> GameState -> (RenderState.DeltaBoard, GameState)
extendSnake newHead bi (GameState (SnakeSeq head body) a m r) = (delta, gState)
  where
    delta  = [(newHead, Board.SnakeHead), (head, Board.Snake)]
    gState = GameState (SnakeSeq newHead (head S.<| body)) a m r


displaceSnake :: Point -> BoardInfo -> GameState -> (RenderState.DeltaBoard, GameState)
displaceSnake newHead bi (GameState (SnakeSeq head body) a m r)
  | S.null body        = ([(newHead, Board.SnakeHead), (head, Board.Empty)], GameState (SnakeSeq newHead body) a m r)
  | S.length body == 1 = ([(newHead, Board.SnakeHead), (head, Board.Snake), (S.index body 0, Board.Empty)], GameState (SnakeSeq newHead (S.singleton head)) a m r)
  | otherwise          = ([(newHead, Board.SnakeHead), (head, Board.Snake), (tail, Board.Empty)], GameState (SnakeSeq newHead body') a m r)
  where
    body' = S.deleteAt (S.length body) $ head S.<| body
    tail  = S.index body $ S.length body - 1
  

move :: BoardInfo -> GameState -> ([Board.RenderMessage] , GameState)
move bi g@(GameState s@SnakeSeq{snakeHead = head, snakeBody = body} a m r)
  | newHead == a = ([Board.RenderBoard (delta' ++ [(apple, Board.Apple)]), Board.UpdateScore 1], g' {snakeSeq=s', applePosition=apple})
  | otherwise    = ([Board.RenderBoard delta''], g'')
  where
    newHead = nextHead bi g
    (apple, gA) = newApple bi g
    (delta', g'@(GameState s' a' m' r')) = extendSnake newHead bi g
    (delta'', g''@(GameState s'' a'' m'' r'')) = displaceSnake newHead bi g






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
