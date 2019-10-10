module Model
    ( User(..)
    , Book(..)
    , initFile
    , initial
    , searchUser
    , addUser
    , deleteUser
    , searchBook
    , addBook
    , deleteBook
    , getUser
    , getBook
    , borrowBook
    , returnBook
    ) where

import Control.Exception
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Data.Char
import Data.List
import System.Directory
import System.IO
import System.IO.Error

data Book = Book {
   bID::Int,
   bName::String,
   cate::String,
   num::Int
} deriving(Show,Read,Eq)

data User = User {
   uID::Int,
   uName::String,
   role::Roles,
   password::String,
   borrow::[Book]   
} deriving(Show,Read,Eq)

data Roles = Admin|Guest deriving(Show,Read,Eq,Ord,Bounded,Enum)

-- type Users = [User]
-- type Books = [Book]
-- data Datas = Users | Books
data Datas = Users [User] | Books [Book]

numRegex = "^[0-9]+$"
-- num  =~ numRegex::Bool

-- 保存数据的文件路径
userPath = "user.txt"
bookPath = "book.txt"

-- 初始数据
initUsers = [User {uID = 0, uName = "admin", role = Admin, password = "8888", borrow = []}]

initBooks =
    [ Book {bID = 0, bName = "Learn You a Haskell for Great Good!", cate = "Haskell", num = 5}
    , Book {bID = 1, bName = "Real World Haskell", cate = "Haskell", num = 5}
    , Book {bID = 2, bName = "JavaScript框架设计", cate = "JavaScript", num = 5}
    , Book {bID = 3, bName = "JavaScript高级程序设计", cate = "JavaScript", num = 5}
    , Book {bID = 4, bName = "CLR via C#", cate = "C#", num = 5}
    , Book {bID = 5, bName = "ASP.NET设计模式", cate = "C#", num = 5}
    ]


getUser::IO [User]
getUser = read <$> readFile userPath -- <$> 就是中缀版的 fmap


getBook::IO [Book]
getBook = fmap read $ readFile bookPath -- read `liftM` readFile bookPath
        -- content <- readFile bookPath
        -- return $ read content

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist !"
    | isFullError e = putStrLn "The space is full !"
    | isIllegalOperation e = putStrLn "Illegal operation !"
    | otherwise = ioError e


saveData::Datas -> IO ()
saveData (Books a) = save bookPath a
saveData (Users a) = save userPath a

saveUser::Show a => a -> IO ()
saveUser = save userPath

saveBook::Show a => a -> IO ()
saveBook = save bookPath


-- 保存到临时文件.temp再改名,避免同时操作文件锁定的问题
save :: Show a => FilePath -> a -> IO ()
save path x = do
    (name, handle) <- openTempFile "." "temp"
    hPutStr handle $ show x
    hClose handle
    removeFile path
    renameFile name path


clearFile :: IO [Char]
clearFile = do
    doesFileExist userPath >>= \exist ->
        if exist then removeFile userPath >> return "Remove user file"
        else return "User file not exist !"
    bookExist <- doesFileExist bookPath
    case bookExist of
        True -> removeFile bookPath >> return "Remove book file"
        otherwise -> return "Book file not exist !"


initFile :: IO ()
initFile = do 
    handle <- openFile userPath ReadWriteMode -- openFile 方式
    hPutStr handle $ show initUsers
    hClose handle
    withFile bookPath ReadWriteMode (\handle -> do 
        hPutStr handle $ show initBooks 
        hClose handle) -- withFile 方式
    -- appendFile fileName ""

initial = clearFile >> initFile

searchUser::String -> IO [User]
searchUser n = getUser >>= filterM (\User {uID=id,uName=name} -> return $ show id == n || isPrefixOf n name)

addUser::(String,String) -> IO [User]
addUser (name,pass) = do
    users <- getUser
    let nid = succ $ maximum [ id | User{uID=id} <- users ]
        user = User{uID=nid,uName=name,role=Guest,password=pass,borrow=[]}
    saveData $ Users $ user:users
    return [user]

deleteUser::String -> IO (Either String String)
deleteUser id = do
    users <- getUser
    books <- getBook
    case find (\User{uID=x} -> show x == id) users of
        Nothing -> return $ Left "User not exist !"
        Just user -> do 
            saveData $ Books $ (\book@Book{ bID = bid, num = n } -> if findBorrowBook bid user then book{ num = n + 1 } else book) <$> books
            saveData $ Users [ u | u@User{uID=x} <- users, show x /= id ]
            return . Right $ uName user ++ " had been Successful deleted !"

findBorrowBook id user = do
    case find (\Book{bID = x } -> x == id) (borrow user) of
        Nothing -> False
        Just book -> True

searchBook::String -> IO [Book]
searchBook n = do
    books <- getBook
    -- return $ filter (\Book {bID=id,bName=name,cate=cat} -> show id == n || isPrefixOf n cat) books
    return [ book| book@Book {bID=id,bName=name,cate=cat} <- books, show id == n || isPrefixOf n (map toLower cat) || isInfixOf n (map toLower name) ]

addBook::(String,String,Int)->IO [Book]
addBook (name,cat,n) = do
    books <- getBook
    let nid = head $ [1..100] \\ (foldr (\Book{bID=id} x -> id:x) [] books)
        book = Book{bID=nid,bName=name,cate=cat,num= n}
    saveData $ Books $ book:books
    return [book]

-- newtype EitherT m a = EitherT { runEitherT :: m (Either a)}
-- bindMT :: (Monad m) => EitherT m a -> (a -> EitherT m b) -> EitherT m b
-- x `bindMT` f = EitherT $ do
--     unwrapped <- runEitherT x
--     case unwrapped of
--       Left x -> return $ Left x
--       Right y -> runEitherT (f y)

-- returnMT :: (Monad m) => a -> EitherT m a
-- returnMT a = EitherT $ return (Right a)

-- failMT :: (Monad m) => a -> EitherT m a
-- failMT x = EitherT $ return Left x

-- instance (Monad m) => Monad (EitherT m) where
--     return = returnMT
--     (>>=) = bindMT
--     fail = failMT

-- instance MonadTrans EitherT where
--     lift m = EitherT (Right `liftM` m)

-- runWriterT $ delBook "2" -- 使用 monad变换器 实现，对比使用 monad组合 实现的 deleteBook
-- delBook::String -> WriterT [(Bool,String)] IO ()
-- delBook id = do
--         books <- liftIO getBook
--         case find (\Book{bID=x} -> show x == id) books of
--             Nothing -> tell [(False,"Book not exist !")]
--             Just book -> do 
--                 liftIO $ saveBook $ filter (\Book{bID=x} -> show x /= id) books 
--                 tell [(True, "《" ++ bName book ++ "》had been Successful deleted !")]

-- runWriterT . runMaybeT $ delBook "2"
delBook::String -> MaybeT (WriterT String IO) ()
delBook id = do
    books <- liftIO getBook
    case find (\Book{bID=x} -> show x == id) books of
        Nothing -> tell "book not exist !" >> fail "err"
        Just book -> do
            -- liftIO $ saveBook $ filter (\Book{bID=x} -> show x /= id) books 
            liftIO $ saveData $ Books [ b | b@Book{ bID = x } <- books, show x /= id ]
            tell $ "《" ++ bName book ++ "》had been Successful deleted !"

deleteBook::String -> IO (Either String String)
deleteBook id = do
    books <- getBook
    case find (\Book{bID=x} -> show x == id) books of
        -- Nothing -> putStrLn "Book not exist !" >> return False
        Nothing -> return $ Left "Book not exist !"
        Just book -> do 
            saveBook $ filter (\Book{bID=x} -> show x /= id) books 
            -- putStrLn $ "《" ++ bName book ++ "》had been Successful deleted !"
            return . Right $ "《" ++ bName book ++ "》had been Successful deleted !"

borrowBook::(Int,Int) -> IO (Either String String)
borrowBook (uid,bid) = do 
    users <- getUser
    books <- getBook
    case find (\User {uID=id} -> id == uid) users of 
        Nothing -> return $ Left "User not exist !"
        Just user -> case findIndex (\Book {bID=id} -> id == bid) (borrow user) of
            Just index -> return $ Left "User already borrowed the book !"
            Nothing -> case find (\Book {bID=id} -> id == bid) books of
                Nothing -> return $ Left "Book not exist !"
                Just book -> do 
                    if num book <= 0 then return $ Left "The book has been borrowed !"
                    else do 
                        saveData $ Books [if id == bid then book {num = n-1} else book | book@Book {bID=id,num=n} <- books] -- 和map或<$>功能一样
                        saveData $ Users $ (\user@User{uID=id,borrow=bs} -> if id == uid then user{borrow = book:bs} else user) <$> users
                        return . Right $ uName user ++ " borrow《"++ (bName book) ++ "》success!"
   
returnBook::(Int,Int) -> IO (Either String String) 
returnBook (uid,bid) = do 
    users <- getUser
    books <- getBook
    case find (\User{uID=id} -> id == uid) users of 
        Nothing -> return $ Left "User not exist !"
        Just user -> case findIndex (\Book {bID=id} -> id == bid) (borrow user) of
            Nothing -> return $ Left "User not borrow the Book !"
            Just index -> case find (\Book {bID=id} -> id == bid) books of
                Nothing -> return $ Left "Book not exist !"
                Just book -> do 
                    saveData $ Books [if id == bid then book {num = n+1} else book | book@Book{bID=id,num=n} <- books]
                    saveData $ Users [if id == uid then user {borrow = filter (\Book{bID=id} -> id /= bid) bs} else user | user@User{uID=id,borrow=bs} <- users]
                    return . Right $ uName user ++ " return《"++ (bName book) ++ "》success!"