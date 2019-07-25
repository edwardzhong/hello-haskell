module Main where

import           Control.Monad
import           Data.Char
import           Data.List
import qualified Data.Map         as Map
import           System.Directory
import           System.IO
import           System.Random

main = doReadWrite

-- forever $ do
doReadWrite = do
    putStrLn "Do you want to [read] a file [write] a file or [quit] ?"
    act <- getLine
    case act of
      "read"    -> do --多条语句必须用do包起来
                      runAction "read" doRead
                      doReadWrite
      "write"   -> do
                      runAction "write" doWrite
                      doReadWrite
      "quit"    -> return () --退出
      otherwise -> doReadWrite

runAction txt fn = do
    putStr $ "Enter a file name to " ++ txt ++": "
    path <- getLine
    fileExist <- doesFileExist path
    if fileExist
      then fn path
    else putStrLn "file no exist !"

doRead path = do
    content <- readFile path
    putStrLn content

doWrite path = do
    putStrLn "Please enter text :"
    content <- getLine
    -- writeFile path content --清空后填充
    appendFile path (content ++ "\n")  --增加内容


-- 猜数字
gussNumber = do
    num <- randomRIO(1::Int,100)
    putStrLn "Think Of A Number Between 1 And 100:"
    doGussing num
    where doGussing :: Int -> IO()
          doGussing num = do
              putStr "Enter your guess:"
              numStr <- getLine
              let guessNum = read numStr
              if guessNum < num
                then putStrLn "Too Low !" >> doGussing num
              else if guessNum > num
                then putStrLn "Too High !" >> doGussing num
              else do putStrLn "You Win !"
  
 -- 求数组的 sum 和 乘积, 同时求出每个元素的阶乘
calculate = doCalculate []
doCalculate :: [Int] -> IO()
doCalculate xs = do
    putStrLn "Give me a number (or 0 to stop)"
    str <- getLine
    if not . and . map isDigit $ str then do -- 限定只能输入数字
      putStrLn "Please Input Number !"
      doCalculate xs
    else do
      let num = read str
          factorial 0 = 1
          factorial n = n * factorial (n - 1)
      case num of
        0 -> do
          putStrLn $ replicate 20 '='
          putStrLn $ "The sum is " ++ (show $ sum xs)
          putStrLn $ "The product is " ++ (show $ foldr1 (*) xs)
          mapM_ (\x -> putStrLn $ show x ++ " factorial is " ++ (show $ factorial x)) (reverse xs)
        otherwise -> doCalculate (num:xs)

-- 目前有abcde的字符匹配
maps = Map.fromList[ 
  ('a',[ "  A  ", " A A ", "AAAAA", "A   A", "A   A" ]), 
  ('b',[ "BBBB ", "B   B", "BBBBB", "B   B", "BBBB " ]), 
  ('c',[ " CCCC", "C    ", "C    ", "C    ", " CCCC" ]), 
  ('d',[ "DDD  ", "D   D", "D   D", "D   D", "DDD  " ]), 
  ('e',[ "EEEEE", "E", "EEEEE", "E    ", "EEEEE" ])]

sayit :: String -> IO()
sayit xs = putStrLn . unlines . map (intercalate "\t") . transpose $ getList . toLower <$> xs
        -- let ws = getList . toLower <$> xs -- applcative
        -- let ws = xs >>= \x -> return $ getList $ toLower x -- monad
        -- putStrLn . unlines $ intercalate "\t" <$> transpose ws
        where getList::Char -> [String]
              getList w = case Map.lookup w maps of 
                Nothing -> ["","","","",""]
                Just v -> v
                
-- 分配locker,存在且空余则返回密码
data LockState = Taken | Free deriving(Show,Eq)
lockers = Map.fromList [(100,(Taken,"aaa")), (101,(Free,"bbb")), (102,(Free,"ccc")), (103,(Taken,"ddd"))]
lookupLocker :: Int -> Map.Map Int (LockState, b) -> Either [Char] b
lookupLocker num m = case Map.lookup num m of
        Nothing -> Left $ "Locker Number " ++ show num ++ " doesn't exist!"
        Just (state,code) -> if state /= Taken then Right code
                             else Left $ "Locker " ++ show num ++ " is already Taken !"

-- binary tree
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)  
freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
             (Node 'L'  
              (Node 'N' Empty Empty)  
              (Node 'T' Empty Empty)  
             )  
             (Node 'Y'  
              (Node 'S' Empty Empty)  
              (Node 'A' Empty Empty)  
             )  
        )  
        (Node 'L'  
             (Node 'W'  
                  (Node 'C' Empty Empty)  
                  (Node 'R' Empty Empty)  
             )  
             (Node 'A'  
                  (Node 'A' Empty Empty)  
                  (Node 'C' Empty Empty)  
             )  
        ) 
data Direction = L | R deriving (Show)  
type Directions = [Direction]  

changeToP :: Directions-> Tree Char -> Tree Char  
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r  
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)  
changeToP [] (Node _ l r) = Node 'P' l r  

elemAt :: Directions -> Tree a -> a  
elemAt (L:ds) (Node _ l _) = elemAt ds l  
elemAt (R:ds) (Node _ _ r) = elemAt ds r  
elemAt [] (Node x _ _) = x 


-- ghci> let newTree = changeToP [R,L] freeTree  
-- ghci> elemAt [R,L] newTree  

-- type Breadcrumbs = [Direction]

-- goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
-- goLeft (Node _ l _, bs) = (l, L:bs)

-- goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)  
-- goRight (Node _ _ r, bs) = (r, R:bs) 

x -: f = f x --先写值,再写函数 -:

-- ghci> (freeTree, []) -: goRight -: goLeft 

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)  
goUp (t, RightCrumb x l:bs) = (Node x l t, bs) 

type Zipper a = (Tree a, Breadcrumbs a)

modify :: (a -> a) -> Zipper a -> Zipper a  
modify f (Node x l r, bs) = (Node (f x) l r, bs)  
modify f (Empty, bs) = (Empty, bs) 

-- ghci> let newFocus = modify (\_ -> 'P') (goRight (goLeft (freeTree,[])))
-- ghci> let newFocus = (freeTree,[]) -: goLeft -: goRight -: modify (\_ -> 'P')

attach :: Tree a -> Zipper a -> Zipper a  
attach t (_, bs) = (t, bs)

-- ghci> let farLeft = (freeTree,[]) -: goLeft -: goLeft -: goLeft -: goLeft  
-- ghci> let newFocus = farLeft -: attach (Node 'Z' Empty Empty) 

topMost :: Zipper a -> Zipper a  
topMost (t,[]) = (t,[])  
topMost z = topMost (goUp z)  
