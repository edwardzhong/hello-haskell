import           Control.Monad -- MonadPlus , guard
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Monad.Identity
import           Data.Char
import qualified Data.Foldable        as F
import           Data.List
import qualified Data.Map             as Map
import           Data.Monoid -- Product , Sum
import           System.IO
-- import           System.Random
import           Debug.Trace -- 调试用的trace 和 tarceM
-- import           Text.Regex.Posix -- 正则表达式
import           Data.Map.Strict ((!)) -- 严格模式的Map
import           Data.Function  --  `on` 函数


{-===================== 算法 =====================-}
-- 插入排序
insertSort [] = []
insertSort (x:xs) = insert x $ insertSort xs
    where insert x [] = [x]
          insert x (y:ys) | x < y = x:y:ys
                          | otherwise = y:insert x ys

-- 选择排序
selectSort [] = []
selectSort xs = let x = minimum xs 
    in x:selectSort (delete x xs)

-- 冒泡排序
bubbleSort xs | swap xs == xs = xs -- 已经排好序的情况
              | otherwise = bubbleSort $ swap xs
               where swap []  = []
                     swap [x] = [x]
                     swap (x:y:ys) | x > y = y:swap (x:ys)
                                   | otherwise = x:swap (y:ys)

-- 快速排序
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = quickSort [ y | y <- xs, y <= x ] ++ [x] ++ quickSort [ y | y <- xs, y > x ]

-- 归并排序
mergeSort xs = merge (mergeSort x) (mergeSort y)
    where (x,y) = (take l xs, drop l xs)
          l = (length xs) `div` 2

merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) 
    | x <= y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

-- 8皇后问题 第n步所有可能的摆放列表
queens :: Int -> [[Int]]
queens 0 = [[]]
queens n = [x:y | y <- queens(n-1), x<-[1..8], safe x y 1]

-- y 表示第几行，[] 表示前n行已经占位的皇后数组，n 表示相对第m行相隔第几列(判断对角线是否相连)
safe:: Int -> [Int] -> Int-> Bool
safe _ [] _ = True
safe y (x:xs) n = y /= x && y /= x + n && y /= x - n && safe y xs (n + 1)

{- ====================== 高级 =========================-}

{- MonadReader -}
-- r 就是输入的全局参数
-- newtype Reader r a = Reader {runReader::r -> a}
-- -- runReader m 提取出函数fun, 再 fun r 获得 a 值 
-- instance Functor(Reader r) where
--     fmap f m = Reader $ \r -> f(runReader m r)

-- instance Applicative(Reader r) where
--     pure a = Reader $ \_ -> a
--     a <*> b = Reader $ \r -> runReader a r $ runReader b r

-- instance Monad(Reader r) where
--     return = pure
--     m >>= k = Reader $ \r -> runReader (k (runReader m r)) r

{- 使用reader的方式, Reader a b 与 a->b 是同构的 -}
-- headT::Reader String String
-- headT =Reader $ \name -> "Welcome "++ name ++ ".\n"
-- bodyT::Reader String String
-- bodyT= Reader $ \name -> "Welcome to my home, "++name ++". this is best home you can ever find on this planet.\n"
-- footT::Reader String String
-- footT= Reader $ \name -> "Now, help yourself, "++name++".\n"

-- data Greet = Greet{
--     greetHead::String,
--     greetBody::String,
--     greetFoot::String
-- } deriving Show

-- renderGreeting = do
--     h<-headT
--     b<-bodyT
--     f<-footT
--     return $ Greet h b f

-- Applicative also  can work !
-- renderGreeting = Greet <$> headT <*> bodyT <*> footT
-- runReader renderGreeting "alex"

{- --使用单子变换中的readerT -}
headT::ReaderT String Identity String
headT = do 
    name <- ask 
    return $ "Welcome "++ name ++ ".\n"

bodyT::ReaderT String Identity String
bodyT = do 
    name <- ask 
    return $ "Welcome to my home, "++ name ++". this is best home you can ever find on this planet.\n"

footT::ReaderT String Identity String
footT =  do 
    name <- ask 
    return $ "Now, help yourself, "++ name ++".\n"

renderGreeting::ReaderT String Identity String
renderGreeting = do
    h<- headT
    b<- bodyT
    f<- local("Mr an Mrs. " ++ )footT
    return $ h ++ b ++ f

-- 依次用使用 runIdentity 和 runReaderT 将结果提取出来
greet::String
greet = runIdentity $ runReaderT renderGreeting "david"

{-
MonadWriter
class (Monoid w, Monad m) => MonadWriter w m | m -> w where
    tell :: w -> m ()
    listen :: m a -> m (a, w)
    pass :: m (a, w -> w) -> m a

tell 不做任何计算，直接将记录追加到末尾
listen 返回计算的结果，同时返回记录的值
pass 会对记录器Monad输入一个函数
-}

-- 使用字符合并方式记录的Writer
move:: Int -> Writer String Int
-- move i = writer(i-1,"left\n") >>= \x -> writer (x+1,"right\n")
move i = do -- do改写
    tell "left\n"
    let x = i -1
    tell "right\n"
    return (x + 1)
-- 使用runWriter解包函数
-- runWriter (move 4)

--使用数组方式记录的Writer
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcd' b (a `mod` b)

-- fst $ runWriter (gcd' 8 3)

{- MondState -}
-- State s a 与 s -> (a,s') 是同构
-- newtype State s a = State { runState::s -> (a,s)}
-- trace:调试打印，traceM 是 trace 的 monadic 版本
intState :: State Int ()  
intState = do  
    s <- get 
    traceM $ show s 
    if s == 1 then put 2
    else put 3

-- runState intState 1

stack::State [Int] ()
stack = do
    s <- get
    traceM $ "current state is :" ++ show s -- 在do block中调试输出
    case (3-) $ length s of
        0 -> return ()
        i -> put $ [1..i]++s

-- runState stack [1,2,3]

-- state 应用
sortCard::State [Int] ()
sortCard = do
    s <- get
    put $ sort s
-- runState sortCard [2,1,4,3]

pushCard::Int -> State [Int] ()
pushCard c = do
    s <- get
    put $ c:s
-- (runState $ pushCard 1) [] -- runState提取出函数后，再输入参数[]进行下一步运算 

popCard::State [Int] (Int)
popCard = do
    s <- get
    put $ tail s
    return (head s)
-- runState popCard []

-- do 表示法 组合子运算
cardOp :: State [Int] ()
cardOp = do
    pushCard 2
    pushCard 6
    pushCard 3
    popCard
    sortCard
-- runState cardOp [1]

{- 使用StateT单子变换 -}
-- modify 使用函数修改状态
-- 四则运算
caculator::StateT Double IO()
caculator = do
    result <- get
    lift $ print result
    (op:input) <- lift getLine
    let opFn = case op of
            '+' -> sAdd
            '-' -> sMinus
            '*' -> sTime
            '/' -> sDiv
            _ -> const $ return ()
    opFn $ read input
    where 
        sAdd x = modify (+x)
        sMinus x = modify (\y -> y - x)
        sTime x = modify (* x)
        sDiv x = modify (/ x)

op ::IO(a,Double)
op = runStateT (forever caculator) 0
-- > +1
-- > /2
-- > *3

{- ============== codewars ============= -}
{- 歌词解码 -}
-- songDecoder'::String->String
-- songDecoder' s = dropWhile (==' ') $ dec s 
--     where dec [] = []
--           dec xs = case xs =~"WUB"::(String,String,String) of
--               ("",_,ys) -> dec ys
--               (x,_,ys) -> " " ++ x ++ (dec ys)

songDecoder :: String -> String
songDecoder = unwords . words . go
    where go []               = []
          go ('W':'U':'B':xs) = ' ' : go xs
          go (x:xs)           =   x : go xs


-- songDecoder :: String -> String
-- songDecoder = unwords . filter (not . null) . splitOn "WUB"

-- songDecoder "WUBWEWUBAREWUBWUBTHEWUBCHAMPIONSWUBMYWUBFRIENDWUB"
--   `shouldBe` "WE ARE THE CHAMPIONS MY FRIEND"


{- 摩尔斯码解码 -}
morseCodes::String -> String
morseCodes = map (\x -> if x == '.' then 'D' else 'E')

decodeMorse :: String -> String
decodeMorse = decode . trim 
    where decode ::String->String
          decode [] = []
          decode (' ':' ':' ':xs) = ' ' : decode xs
          decode (' ':xs) = decode xs
          decode xs = (morseCodes $ takeWhile (/=' ') xs) ++ (decode $ dropWhile (/=' ') xs)
          trim :: String -> String
          trim = dropWhile (==' ') . dropWhileEnd (==' ') 

-- decodeMorse = unwords
--             . filter (not . null)
--             . map (concatMap (morseCodes !) . words)
--             . splitOn "   "

-- decodeMorse ".... . -.--   .--- ..- -.. ."
--should return "HEY JUDE"

{- 获取波峰值和位置 -}
data PickedPeaks = PickedPeaks { pos :: [Int], peaks :: [Int]} deriving (Eq, Show)

pickPeaks :: [Int] -> PickedPeaks
pickPeaks s = do
    let (a,b) = unzip $ pick (zip [0..] s)
    PickedPeaks a b
    where 
        pick::[(Int,Int)]->[(Int,Int)]
        pick [] = []
        pick [_] = []
        pick [_,_] = []
        pick (x:y:z:xs) 
            | snd x < snd y && snd y > snd z = y:pick (z:xs)
            | snd y == snd z = pick (x:y:xs)
            | otherwise = pick (y:z:xs)
            
-- pickPeaks([3, 2, 3, 6, 4, 1, 2, 3, 2, 1, 2, 3]) should return {pos: [3, 7], peaks: [6, 3]} 


-- orderWeight = unwords . sortBy (\x y -> compare (weight x) (weight y)) . words 
orderWeight = unwords . sortBy (compare `on` weight) . sort . words
    where weight =  sum . map digitToInt

-- orderWeight "56 65 74 100 99 68 86 180 90"
-- should be "100 180 90 56 65 74 68 86 99"    

longestConsec :: [String] -> Int -> String
longestConsec s k 
        | length s == 0 || k > length s || k <= 0 = ""
        | otherwise = case go 0 s of
            []-> ""
            rs -> last rs
            where go::Int->[String]->[String]
                  go _ [] = []
                  go m (x:xs) = if length w > m then w:go (length w) xs
                            else go m xs
                            where w = concat $ take k (x:xs)  

-- longestConsec ["itvayloxrp","wkppqsztdkmvcuwvereiupccauycnjutlv","vweqilsfytihvrzlaodfixoyxvyuyvgpck"] 2
-- longestConsec ["it","wkppv","ixoyx", "3452", "zzzzzzzzzzzz"] 3 
-- longestConsec [] 3 
-- longestConsec  ["it","wkppv","ixoyx", "3452", "zzzzzzzzzzzz"] 15  

yourOrderPlease :: String -> String
yourOrderPlease  = unwords . sortOn(find isDigit) . words 

-- yourOrderPlease "4of Fo1r pe6ople g3ood th5e the2"

-- get unique number
getUnique::[Int]->Int
getUnique = head . concat . filter ((== 1) . length) . group . sort 



-- legMass :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Bool -> String
-- legMass (yd,ys) (t1d,t1s) (t2d,t2s) isBroken = if isBroken 
--         then fst . last $ sortOn (\(_,x) -> x ) [("t2",t2s),("t1",t1s),("y",ys)]
--         else fst . head $ sortOn (\(_,x) -> x ) [("y",y),("t1",t1),("t2",t2)]
--         where 
--             fdiv :: Int -> Int -> Float
--             fdiv d s = if s == 0 then 0.0 else fromIntegral d / fromIntegral s
--             y = fdiv yd ys
--             t1 = fdiv t1d t1s
--             t2 = fdiv t2d t2s

legMass :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Bool -> String
legMass (yd,ys) (t1d,t1s) (t2d,t2s) isBroken = if isBroken
        then str maximum [ys,t1s,t2s]
        else str minimum [fdiv yd ys,fdiv t1d t1s,fdiv t2d t2s]
        where 
            fdiv :: Int -> Int -> Float
            fdiv d s = fromIntegral d / fromIntegral s
            str ::Ord a => ( [a] -> a ) -> [a] -> String
            str f xs = case f xs `elemIndex` xs of Just i -> ["y","t1","t2"] !! i