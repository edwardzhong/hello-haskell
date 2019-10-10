{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Char
import Language.Haskell.TH
-- import Data.Typeable
import GHC.Generics

-- import           Text.Parsec
data Book =
    Book
        { bid :: Int
        , bname :: String
        }
    deriving (Generic, Show, Read, Eq)

main :: IO ()
main = putStrLn "this is test"

isPassValid :: String -> Bool
isPassValid s = length s >= 8 && check s
  where
    check s = and [f s | f <- any <$> [isUpper, isLower, isNumber]]

-- setPassword::MaybeT (WriterT String IO)()
-- setPassword = do
--         liftIO $ putStrLn "Please set a password"
--         pass <- liftIO $ getLine
--         -- when (isPassValid pass) (tell pass)
--         guard $ isPassValid pass
--         tell pass
-- runWriterT $ runMaybeT setPassword
setPassword :: MaybeT IO String
setPassword = do
    liftIO $ putStrLn "Please set a password"
    pass <- liftIO $ getLine
    guard $ isPassValid pass
    return pass

--runMaybeT setPassword
push :: Int -> MaybeT (State [Int]) Int
-- push x = MaybeT $ state $ \xs -> (Just x,x:xs)
push x = do
    xs <- lift get
    lift $ put $ x : xs
    return x

-- (runState $ runMaybeT $ push 1) [1,2,3]
pop :: MaybeT (State [Int]) Int
-- pop = MaybeT $ state $ \xs -> case xs of
--                                 [] ->  (Nothing,xs)
--                                 (y:ys) -> (Just y,ys)
pop = do
    xs <- lift get
    case xs of
        [] -> do
            lift $ put []
            fail "err"
        (y:ys) -> lift $ put ys >> return y

-- (runState $ runMaybeT $ pop ) [1,2,3]
stack :: MaybeT (State [Int]) Int
stack = do
    push 1
    a <- pop
    b <- pop
    return b

--   push 2
-- (runState $ runMaybeT stack) []
guardTest :: Int -> IO ()
guardTest a = guard (a > 5) >> putStrLn "larger than 5"

whenTest :: Int -> IO ()
whenTest a = when (a > 5) $ putStrLn "larger than 5"

-- ref :: DecsQ
ref :: Q [String]
ref = do
    TyConI (DataD _ _ _ _ cons _) <- reify ''Book
    [RecC conName fields] <- return cons
    return $ (\(fileName, _, fileType) -> nameBase fileName) <$> fields
-- TyConI (DataD [] Ghci1.Book [] Nothing [RecC Ghci1.Book [(Ghci1.bid,Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Types.Int),(Ghci1.bname,Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Base.String)]] [])


