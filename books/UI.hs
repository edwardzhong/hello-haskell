module UI
    ( mainUI
    ) where
import           Control.Monad
import           Data.Char
import           Data.List
import           Model
import qualified Data.Map             as Map

title = "Book Management System (Haskell)"
mainDispatch = [('i',"Initialization",initialize)
   ,('u',"User Management",userUI)
   ,('b',"Book Management",bookUI)
   ,('r',"Borrow or Return",borrowUI)
   ,('e',"Exit",exit)]

userDispatch = [('a',"All Users",allUserUI)
   ,('s',"Search User",searchUserUI)
   ,('c',"Add User",addUserUI)
   ,('d',"Delete User",delUserUI)
   ,('u',"Up",mainUI)
   ,('e',"Exit",exit)]

bookDispatch = [('a',"All Books",allBookUI)
   ,('s',"search Book",searchBookUI)
   ,('c',"Add book",addBookUI)
   ,('d',"Delete book",delBookUI)
   ,('u',"Up",mainUI)
   ,('e',"Exit",exit)]

borrowDispatch = [('b',"Borrow",borrowBookUI)
   ,('r',"Return",returnBookUI)
   ,('u',"Up",mainUI)
   ,('e',"Exit",exit)]

-- 参数前置减少括号,同时也更加直观
x |> f = f x

renderUI list = do
    putStrLn $ replicate 40 '='
    -- putStr $ replicate ((40 - (length title)) `div` 2) ' '
    putStr $ replicate ((`div` 2) . (40-) $ length title) ' ' 
    putStrLn $ title ++ "\n"
    putStrLn $ unlines [(' ' <$ [1..8]) ++ x:" - " ++ y|(x,y,_)<-list]
    -- mapM_ (\(x,y,_) -> putStrLn $ replicate 8 ' ' ++ x:" - " ++ y) list
    putStrLn $ replicate 6 ' ' ++ "Please Input key " ++ (intersperse ',' [ x |(x,_,_) <- list])
    putStrLn $ replicate 40 '='
    str <- getLine
    if null str then mainUI
    else case str |> head |> toLower |> dispatch of
          Just (_,_,action) -> action
          Nothing           -> mainUI
    where dispatch c = find (\(x,_,_) -> x == c) list

mainUI::IO()
mainUI = renderUI mainDispatch

exit = return ()

initialize = initial >> putStrLn "Initialize Success !" >> mainUI

userUI = renderUI userDispatch

allUserUI = getUser >>= showUser >> userUI
searchUserUI = do
    putStr "Please Input Keyword:"
    users <- searchUser =<< getLine
    if null users then putStrLn "No match result" 
    else showUser users
    userUI

addUserUI = do
    [name,pass] <- forM ["Please Input User Name:","Please Input User Password:"] (\x -> putStrLn x >> getLine)
    addUser (name,pass) >>= showUser
    userUI

delUserUI = do 
    putStrLn "Please Input User ID or Exit (e) :"
    id <- getLine
    if head id == 'e' then userUI
    else case and . map isDigit $ id of
         False -> putStrLn "User ID must be Number(Int)!" >> delUserUI
         True -> deleteUser id >>= \x -> case x of
             Left err -> putStrLn err >> delUserUI
             Right msg -> putStrLn msg >> userUI

bookUI = renderUI bookDispatch

addBookUI = do
    [name,cate,num] <- mapM (\x -> putStrLn x >> getLine) ["Please Input Book Name:","Please Input Book Category:","Please Input Book Number:"]
    case and . map isDigit $ num of 
       False -> putStrLn "Book Num must be Number(Int)!" >> addBookUI
       True -> do 
           addBook (name,cate,read num::Int) >>= showBook
           bookUI

delBookUI = do 
    putStrLn "Please Input Book ID or Exit (e) :"
    id <- getLine
    if head id == 'e' then bookUI
    else case and . map isDigit $ id of 
         False -> putStrLn "Book ID must be Number(Int)!" >> delBookUI
         True -> deleteBook id >>= \x -> case x of
              Left err -> putStrLn err >> delBookUI
              Right msg -> putStrLn msg >> bookUI

allBookUI = getBook >>= showBook >> bookUI

searchBookUI = do
    putStr "Please input Keyword:"
    books <- searchBook =<< getLine
    if null books then putStrLn "No match result"
    else showBook books >> bookUI

borrowUI = renderUI borrowDispatch

borrowBookUI = borrowReturnUI borrowBook
returnBookUI = borrowReturnUI returnBook
borrowReturnUI f = do 
    putStrLn "Please Input: UserID BookID or Exit (e) :"
    ws <- fmap words getLine
    case ws of 
         []  -> putStrLn "There is no input !" >> borrowReturnUI f
         [x] -> case head x of 
                'e'       -> borrowUI
                otherwise -> putStrLn "BookID is empty!" >> borrowReturnUI f
         (x:y:_) -> do 
            case and . map isDigit $ x of 
                False -> putStrLn "User ID must be Number(Int)" >> borrowReturnUI f
                True  -> case and . map isDigit $ y of
                     False -> putStrLn "Book ID must be Number(Int)" >> borrowReturnUI f
                     True  -> f (read x::Int,read y::Int) >>= \x -> case x of
                        Left err -> putStrLn err >> borrowUI
                        Right msg -> putStrLn msg >> borrowReturnUI f


findLen key list = case Map.lookup key (Map.fromList list) of 
  Nothing -> 10
  Just i -> i

showUser::Foldable t => t User -> IO ()
showUser users = do
    putStrLn $ ths >>= \(n,l) -> n ++ replicate (length n `subtract` l) ' '
    putStrLn $ replicate 60 '-'
    mapM_ (\User {uID=id,uName=name,password=ps,role=ro,borrow=bs} -> do 
        putStr $ show id
        putStr $ replicate (findLen "ID" ths - (length $ show id)) ' '
        putStr name
        putStr $ replicate (findLen "Name" ths - (length name)) ' '
        putStr ps
        putStr $ replicate (findLen "Password" ths - (length ps)) ' '
        putStr $ show ro
        putStr $ replicate (findLen "Role" ths - (length $ show ro)) ' '
        putStr $ bs >>= \Book{bName = name} -> name ++ " | "
        putStrLn ""
        ) users
    putStrLn $ ">> " ++ (show $ length users) ++ " users !"
    where ths = [("ID",8),("Name",12),("Password",12),("Role",10),("Borrow Books",40)]
        --   borrowBooks books = intercalate " | " (foldr (\Book {bName=name} x -> name:x) [] books)
        --   borrowBooks books = intercalate " | " [ name | Book{bName = name} <- books]

showBook::Foldable t => t Book -> IO ()
showBook books = do
    putStrLn $ ths >>= \(n,l) -> n ++ replicate (length n `subtract` l) ' '
    putStrLn $ replicate 70 '-'
    mapM_ (\Book {bID=id,bName=name,cate=cat,num=n} -> do 
        putStr $ show id
        putStr $ replicate (findLen "ID" ths - (length $ show id)) ' '
        putStr cat
        putStr $ replicate (findLen "Category" ths - (length cat)) ' '
        putStr $ show n
        putStr $ replicate (findLen "Number" ths - (length $ show n)) ' '
        putStr name
        putStrLn ""
        ) books
    putStrLn $ ">> " ++ (show $ length books) ++ " books !"
    where ths = [("ID",6),("Category",16),("Number",10),("Name",40)]


