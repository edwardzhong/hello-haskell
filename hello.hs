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

-- Haskell 数据结构即控制结构

-- name@() 模式,表示按模式分割之后仍然保留对整体的引用
capital ""         = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- 获取列表第二项元组的第一个元素, 使用 Maybe 避免空的情况
-- _ 不绑定参数，表示任意值
sec :: [(a,b)] -> Maybe a
sec []      = Nothing
sec [_]     = Nothing
sec (_:x:_) = Just $ fst x


-- if else 表达式 else部分不能省略, 限制了被判断表达式只能是 Bool 
sign x = if x == 0 then 0
         else if x < 0 then -1
         else 1

-- case of 模式匹配，非常有用，模式匹配比 if else 好，因为可以匹配各种数据类型
head' xs = case xs of
           []    -> "No head for empty lists!"
           (x:_) -> show x

-- 函数模式匹配, 模式匹配本质上就是 case of 的语法糖
-- head' [] = "No head for empty lists!"
-- head' (x:_) = x

-- guards 左侧接的是表达式，解决了 case of 不能连接表达式的问题
bmiTell weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2

-- let in 表达式, let 中绑定的名字仅对 in 部分可见. 在 do 语句块中 可以不用 in
-- let 表达式 放在语句块的前面, where 则放在语句块的后面，同时 where 更加自由. 一般需要的绑定很短可以考虑使用let in，其他情况推荐使用 where 
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea

-- let 也可以定义局部函数：
-- let square x = x * x in (square 5, square 3, square 2)

-- 使用模式匹配:
-- (let (a,b,c) = (1,2,3) in a + b + c) * 100

-- 用在list中:
-- calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- list comprihansion
length' xs = sum [1 | _ <- xs] --获取xs列表的长度，其中_表示不关心当前值。
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] --除去字符串中所有非大写字母的函数

{- ================ 递归 ================= -}
{-
递归思考
递归的固定模式：先定义一个边界条件，再定义函数，让它从一堆元素中取一个并做点事情后，把余下的元素重新交给这个函数。 这一模式对 List、Tree 等数据结构都是适用的。
例如，sum 函数就是一个 List 头部与其尾部的 sum 的和。一个 List 的积便是该 List 的头与其尾部的积相乘的积，一个 List 的长度就是 1 与其尾部长度的和. 等等
再者就是边界条件。一般而言，边界条件就是为避免进程出错而设置的保护措施，处理 List 时的边界条件大部分都是空 List，而处理 Tree 时的边界条件就是没有子元素的节点。

Factorial 数又都是非负数，边界条件便可以定为 1，即乘法的单比特。 因为任何数乘以 1 的结果还是这个数。而在 sum 中，加法的单比特就是 0。在快速排序中，边界条件和单比特都是空 List，因为任一 List 与空 List 相加的结果依然是原 List。

使用递归来解决问题时应当先考虑递归会在什么样的条件下不可用, 然后再找出它的边界条件和单比特, 考虑参数应该在何时切开(如对 List 使用模式匹配), 以及在何处运行递归.
-}

-- 斐波那契数列
fab 1 = 1
fab 2 = 1
fab n = fab (n-1) + fab (n-2)

-- foldr/foldl实现
-- fac n = foldl (*) 1 [1..n]
-- fac n = foldr (*) 1 [1..n]

-- Lazy evaluation
-- facs = scanl (*) [1..]
-- fac n = facs !! n


-- 阶乘
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- take函数,前置条件为 n<=0, 列表为空
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

-- 快速排序
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

-- x:xs模式,x表示第一项,xs表示除去第一项之后的部分
-- _表示任意值
sum' []     = 0
sum' (x:xs) = x + sum' xs

map' f []     = []
map' f (x:xs) = f x : map' f xs

filter' _ [] = [] -- _代表任意值
filter' f (x:xs) 
    | f x = x : filter' f xs 
    | otherwise = filter' f xs

replicate' x n = xs n
    where xs 0 = []
          xs n = x:xs (n-1)
{- ================= 遍历-折叠函数 ================== -}

-- 遍历列表是一个非常普遍的需求，用折叠函数代替显式递归进行遍历明显更加易于理解和实现。其中 foldl 是左结合，foldr 是右结合，一般右折叠效率比较高，同时 foldr 也可以用于无限列表，所以应尽量使用 foldr。
-- 折叠函数调用格式: fold 处理函数 初始值(累加值) 需要折叠的列表
-- 另外还提供了和 foldl/foldr 相似的 foldl1/foldr1，它们默认使用列表第一项为初始值，所以可以省略初始值。
-- scanl/scanr 与 foldl/foldr 相似,只是它会记录下累加值所有状态到一个list, 同时也有scanl1和scanr1
-- 还有 foldl' 和 foldl1' 是它们各自惰性实现的严格版本。在用 fold 处理较大的 List 时，经常会遇到堆栈溢出的问题。而这罪魁祸首就是 fold 的惰性: 在执行 fold 时，累加器的值并不会被立即更新，而是做一个"在必要时会取得所需的结果"的承诺。每过一遍累加器，这一行为就重复一次。而所有的这堆"承诺"最终就会塞满你的堆栈。严格的 fold 就不会有这一问题，它们不会作"承诺"，而是直接计算中间值的结果并继续执行下去。如果用惰性 fold 时经常遇到溢出错误，就应换用它们的严格版。
-- foldl (+) 0 [1,2,3]  推导为 ((0 + 1) + 2) + 3
-- foldr (+) 0 [1,2,3]  推导为 ((3 + 2) + 1) + 0

maxNum x = foldr max 0 x --推导为 max a (max b (max c 0)) 如果左结合推导为 max (max ((max 0 a) b) c)
-- maxNum' x =  foldr1 max x
maxNum' xs = foldr (\x acc -> if x > acc then x else acc) 0 xs
-- maxNum' xs = foldr1 (\x acc -> if x > acc then x else acc) xs

-- sum' xs = foldl (+) 0 xs
-- filter' f = foldr (\x acc -> if f x then x:acc else acc) []
-- 等号的两端都有 xs。由于有柯里化 (Currying)，我们可以省掉两端的 xs,foldl (+) 0 回传的就是一个取一 List 作参数的函数。
-- 所以可以修改为 sum' = foldl (+) 0，这就是 point free style。
-- map' f = foldr (\x acc -> f x:acc) []
elem' y = foldl (\acc x -> if y==x then True else acc) False
and' :: [Bool]->Bool
and' = foldr1 (\x y->if not y then False else x && y)


{- =============== 类型定义 ==================-}

-- 由3个任意类型组成的数据类型
data Triple a b c= Triple a b c deriving(Show)
tripleFst (Triple a _ _) = a
tripleSnd (Triple _ b _) = b
tripleThr (Triple _ _ c) = c
-- tr = Triple 1 2 "aa"


-- 限定了类型的数据类型
data Quadruple = Quadruple Int Int String String deriving(Show)
firstTwo (Quadruple a b _ _) = [a,b]
lastTwo (Quadruple _ _ c d) = [c,d]
-- qu = Quadruple 1 2 "aa" "bb"

-- Record Syntax 类似js中的对象字面量
data Person = Person {name::String,age::Int,phoneNumber::String} deriving(Show)
-- let person = Person {name = "alex", age = 20, phoneNumber = "123"}
-- name person -- 取某个属性值

-- Enum类型
data Day = Monday|Tuesday|Wednsday|Thursday|Friday|Saturday|Sunday deriving(Show,Read,Eq,Ord,Bounded,Enum)
-- succ Monday
-- pred Friday
-- [Thursday..Sunday]
-- [minBound..maxBound]::[Day]


-- 递归定义数据类型
data Tree a = Empty|Node a (Tree a) (Tree a) deriving(Show,Read,Eq)

treeInsert x Empty = Node x Empty Empty
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem x Empty = False
treeElem x (Node a left right)
    | x ==a = True
    | x > a = treeElem x right
    | x < a = treeElem x left

-- Functor 定义, 可以运行 fmap 函数
instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node x left right) =
        Node (f x) (fmap f left) (fmap f right)

-- Foldable 定义, 可以运行折叠函数进行遍历
instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                f x `mappend`
                F.foldMap f r

-- let a = foldr treeInsert Empty [5,1,2,7,8]
-- F.foldMap (\x -> [x]) a
-- F.foldl1 (+) a
-- getAny $ F.foldMap (\x -> Any $ x == 3) a

{-
凡是拥有容器性质的类型都可以视作 functor（函子）。
fmap 函数做的事情就是相当于，把一个装着 a 类型的盒子，打开然后把里面的 a 类型拿出来变成 b 类型，之后在返回原来的盒子。
我们可以把函子值是看做有上下文的值。
用盒子模型来解释的话，盒子里面的 a 就是函子值，而装着 a 的盒子就是 a 的上下文。所以 fmap 函数是在保持上下文不变的条件下，把一个函数应用到值上面。

pure 函数：接受一个类型 a 返回一个把这个 a 放入上下文（盒子）中的类型。意思就是把 a 打包成具有上下文的类型。用盒子模型来理解就是把 a 装入一个盒子里面。
<*> 函数：接受一个具有上下文的函数(a -> b)和一个具有上下文的 a （就是 f a）为参数，然后返回具有上下文的 b (就是 f b)的值。
上下文（也就是盒子）是什么东西都可以装的，也就是说上下文（盒子）里面也是可以装函数的。这里就是 applicative 应用的地方了。
分别把函数(a -> b)和参数( a )从盒子中取出来，然后把函数应用到参数上，把得到的值再装回盒子里面。

return 函数：就是把 a 放入一个上下文中。和 applicative 里面的 pure 函数的作用一样
>>= 函数：这个函数就是绑定。他接受一个带有上下文的值和一个返回具有上下文的值的函数为参数，返回最后参数的函数中的返回值。
绑定的过程就是首先把 a 从盒子中取出来，然后把 a 作用到函数，改函数直接返回了一个装有 b 的盒子。
-}

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