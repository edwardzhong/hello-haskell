
import           Control.Applicative -- Const
import           Control.Monad -- MonadPlus , guard
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Monad.Identity
import           Data.Char

isPassValid::String -> Bool
isPassValid s = length s >= 8 && check s
        where check s = and [f s | f <- any <$> [isUpper,isLower,isNumber]]

-- setPassword::MaybeT (WriterT String IO)()
-- setPassword = do
--         liftIO $ putStrLn "Please set a password"
--         pass <- liftIO $ getLine
--         -- when (isPassValid pass) (tell pass)
--         guard $ isPassValid pass
--         tell pass
-- runWriterT $ runMaybeT setPassword

setPassword::MaybeT IO String
setPassword = do
        liftIO $ putStrLn "Please set a password"
        pass <- liftIO $ getLine
        guard $ isPassValid pass
        return pass
--runMaybeT setPassword

push::Int -> MaybeT (State [Int]) Int
-- push x = MaybeT $ state $ \xs -> (Just x,x:xs)
push x = do
        xs <- lift get
        lift $ put $ x:xs
        return x

pop::MaybeT (State [Int]) Int
-- pop = MaybeT $ state $ \xs -> case xs of
--                                 [] ->  (Nothing,xs)
--                                 (y:ys) -> (Just y,ys)
pop = do
        xs <- lift get
        case xs of
                []     -> do 
                        lift $ put []
                        fail "err"
                (y:ys) -> lift $ put ys >> return y

stack::MaybeT (State [Int]) Int
stack = do
        push 1
        -- push 2
        a <- pop
        b <- pop
        return b
        
-- (runState $ runMaybeT stack) []

guardTest :: Int -> IO ()
guardTest a = guard (a > 5) >> putStrLn "larger than 5"

whenTest :: Int -> IO ()
whenTest a = when (a > 5) $ putStrLn "larger than 5"