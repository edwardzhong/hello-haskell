module Model
    ( User(..)
     ,Book(..)
     ,initFile
     ,initial
     ,searchUser
     ,addUser
     ,deleteUser
     ,searchBook
     ,addBook
     ,deleteBook
     ,getUser
     ,getBook
     ,borrowBook
     ,returnBook
    ) where
import           Control.Exception
import           Data.Char
import           Data.List
import           System.IO
import           System.IO.Error
import           System.Directory
import           Control.Monad
import           Text.Regex.Posix

data Book = Book {
   bID::Int,
   bName::String,
   cate::String,
   num::Int
} deriving(Show,Read,Eq)

data User = User {
   uID::Int,
   uName::String,
   role::Role,
   pass::String,
   borrow::[Book]   
} deriving(Show,Read,Eq)

data Role = Admin|Guest deriving(Show,Read,Eq,Ord,Bounded,Enum)


numRegex = "^[0-9]+$"
-- num  =~ numRegex::Bool

-- 保存数据的文件路径
userPath = "user.txt"
bookPath = "book.txt"

-- 初始数据
initUsers = [User{uID=0,uName="admin",role=Admin,pass="8888",borrow=[]}]
initBooks = [Book{bID=0,bName="Learn You a Haskell for Great Good!",cate="Haskell",num=5}
        ,Book{bID=1,bName="Real World Haskell",cate="Haskell",num=5}
        ,Book{bID=2,bName="JavaScript框架设计",cate="javaScript",num=5}
        ,Book{bID=3,bName="JavaScript高级程序设计",cate="javaScript",num=5}
        ,Book{bID=4,bName="CLR via C#",cate="C#",num=5}
        ,Book{bID=5,bName="ASP.NET设计模式",cate="C#",num=5}]

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


saveUser::Show a => a -> IO ()
saveUser = save userPath

saveBook::Show a => a -> IO ()
saveBook = save bookPath

-- 保存到临时文件.temp再改名,避免同时操作文件锁定的问题
save::Show a => FilePath -> a -> IO ()
save path x = do
        (name, handle) <- openTempFile "." "temp"
        hPutStr handle $ show x
        hClose handle
        removeFile path
        renameFile name path

clearFile :: IO [Char]
clearFile = do 
        doesFileExist userPath >>= \exist -> if exist then removeFile userPath >> return "Remove user file" else return "User file not exist !"
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
        let nid = head $ [1..100] \\ [id|User{uID=id} <- users]
            user = User{uID=nid,uName=name,role=Guest,pass=pass,borrow=[]}
        saveUser $ user:users
        return [user]

deleteUser::String -> IO (Either String String)
deleteUser id = do
        users <- getUser
        case find (\User{uID=x} -> show x == id) users of
            Nothing -> return $ Left "User not exist !"
            Just user -> do 
                saveUser $ [ x | User{uID=x} <- users, show x /= id ]
                return . Right $ uName user ++ " had been Successful deleted !"        

searchBook::String -> IO [Book]
searchBook n = do
        books <- getBook
        -- return $ filter (\Book {bID=id,bName=name,cate=cat} -> show id == n || isPrefixOf n cat) books
        return [ book| book@Book {bID=id,bName=name,cate=cat} <- books, show id == n || isPrefixOf n cat || (n =~ name :: Bool)]

addBook::(String,String,Int)->IO [Book]
addBook (name,cat,n) = do
        books <- getBook
        let nid = head $ [1..100] \\ (foldr (\Book{bID=id} x -> id:x) [] books)
            book = Book{bID=nid,bName=name,cate=cat,num= n}
        saveBook $ book:books
        return [book]

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
                            saveBook [if id == bid then book {num = n-1} else book | book@Book {bID=id,num=n} <- books] -- 和map或<$>功能一样
                            saveUser $ (\user@User{uID=id,borrow=bs} -> if id == uid then user{borrow = book:bs} else user) <$> users
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
                        saveBook [if id == bid then book {num = n+1} else book | book@Book{bID=id,num=n} <- books]
                        saveUser [if id == uid then user {borrow = filter (\Book{bID=id} -> id /= bid) bs} else user | user@User{uID=id,borrow=bs} <- users]
                        return . Right $ uName user ++ " return《"++ (bName book) ++ "》success!"

