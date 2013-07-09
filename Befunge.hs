{-# LANGUAGE RankNTypes #-}
import System.Random
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Applicative
import Control.Monad.RWS
import Control.Monad.ST
import Control.Arrow hiding (left, right)
import Data.Array.ST
import Data.Char
import Input

data Dir = U | D | L | R
    deriving (Show, Eq)

dirFn d = case d of
    U -> up
    D -> down
    L -> left
    R -> right

right = first (+ 1)
left = first (subtract 1)
up = second (+ 1)
down = second (subtract 1)

type Stack = []

type Gen = StdGen

data BData = Int  Int
           | Char Char
    deriving (Show, Eq)

data BState = BState { stack      :: Stack Int
                     , dir        :: Dir
                     , pos        :: (Int, Int)
                     , stringMode :: Bool
                     }
    deriving (Show)

onStack :: (Stack Int -> Stack Int) -> BState -> BState
onStack f st = st {stack = f (stack st)}

onPos :: ((Int, Int) -> (Int, Int)) -> BState -> BState
onPos f st = st {pos = f (pos st)}

type Board s = STArray s (Int, Int) Char
--type IBoard = Array

--type Program = ErrorT String (RandT Gen (InputT BData (StateT BState (Reader Board))))
--type Program = ErrorT String (RandT Gen (InputT BData (RWS Board String BState)))
type Program a = forall s. ErrorT String (RandT Gen (RWST (Board s) String BState (InputT BData (ST s)))) a

push :: Int -> Program ()
push = modify . onStack . (:)

errOnEmpty f = do
    s <- fmap stack get
    case s of
        [] -> throwError "Empty stack"
        xs -> f xs

pop :: Program Int
pop = errOnEmpty (\(x:xs) -> setStack xs >> return x)

randomDir :: Program Dir
randomDir = fmap ([U, D, L, R] !!) (getRandomR (1 :: Int, 4))

setRandomDir :: Program ()
setRandomDir = randomDir >>= setDir

binOp :: (Int -> Int -> Int) -> Program ()
binOp f = (f <$> pop <*> pop) >>= push

modifyStack = modify . onStack
setStack = modifyStack . const

modifyPos = modify . onPos

setDir d = modify (\st -> st {dir = d})

add :: Program ()
add = binOp (+)

sub = binOp (flip (-))
mul = binOp (*)
idiv = binOp (flip div)
dup = errOnEmpty $ setStack . (\xs -> head xs : xs)
imod = binOp (flip mod)

inot = do
    x <- pop
    if x == 0 then push 1 else push 0

gt = (\a b -> if b > a then 1 else 0) <$> pop <*> pop

horizBranch = do
    x <- pop
    if x == 0 then setDir R else setDir L

vertBranch = do
    x <- pop
    if x == 0 then setDir D else setDir U

beginQuote = modify (\st -> st {stringMode = True})
endQuote   = modify (\st -> st {stringMode = False})

befungeInput = lift . lift $ lift input

swap = do
    x <- pop
    y <- pop
    push x
    push y

outputChar = pop >>= tell . (:"") . chr
outputInt  = pop >>= tell . show

skipOneCell = modifyPos . dirFn . dir =<< get

g = mkStdGen 1

defState = BState { stack = [1], dir = R, pos = (0,0), stringMode = False}

writeToBoard :: (Int, Int) -> Char -> Program ()
writeToBoard pos c = do
    board <- ask
    lift . lift . lift . lift $ writeArray board pos c

runProgram :: String -> Program a -> (((Either String a, StdGen), BState, String), [BData])
runProgram bStr p = runST (newListArray ((0,0),(1,1)) bStr >>= p')
    where p' e = (flip runInputT []) . (\x -> runRWST x e defState) . (flip runRandT g) . runErrorT $ p

