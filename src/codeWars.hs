module CodeWars where
import Data.List
import Data.Char
import Data.Ord
import Data.Time
import Data.Time.Format
import Data.List.Split
import Data.Maybe

findShortest :: String -> Integer
findShortest = fromIntegral.minimum.map length.words

getMiddle :: String -> String
getMiddle "" = ""
getMiddle [a] = [a]
getMiddle [a,b] = [a,b]
getMiddle (x:xs) = getMiddle (init xs)

persistence :: Int -> Int
persistence n = if n < 10 then 0 else 1 + persistence (product $ map digitToInt $ show n)

doubleChar :: String -> String
doubleChar = concatMap $ replicate 2

isSquare :: Integral n => n -> Bool
isSquare n = (n >= 0) && elem n (map (^2) [0..n])

deleteNth :: [Int] -> Int -> [Int]
deleteNth lst n = map (lst !!) $ sort .
  concatMap (\e -> take n $ elemIndices e lst) $ nub lst

add3 :: Num a => a -> a
add3 = (+3)


describeList :: [a] -> String
describeList lst = case lst of []  ->  "empty"
                               [x] ->  "singleton"
                               lst ->  "longer"

number :: [(Int, Int)] -> Int
number =  foldl (\acc (a,b) -> acc + (a-b)) 0


longestConsec :: [String] -> Int -> String
longestConsec strarr k
    | null strarr || k > length strarr || k <= 0 = ""
    | otherwise = longest sammansatta
    where sammansatta = reverse $ map (concat . take k . flip drop strarr) indices
          longest     = maximumBy (comparing length)
          indices     = [0,1..(length strarr - 1)]

-- IVAN VERSION
-- longestConsec :: [String] -> Int -> String
-- longestConsec strarr k
--     | null strarr || k > length strarr || k <= 0 = ""
--     | otherwise = maximumBy (comparing length) $ reverse $ map (concat . take k . flip drop strarr) [0,1..(length strarr - 1)]


-- rowSumOddNumbers :: Integer -> Integer
-- rowSumOddNumbers n = sum $ take fromIntegral n $ drop (sum [1..fromIntegral n-1]) odds
--   where
--   odds = filter odd [1..]

isVampire :: Integer -> Integer -> Bool
isVampire a b =  sort (show a++show b) == (sort . show $ a*b)

-- isVampire :: Integer -> Integer -> Bool
-- isVampire a b =
--   let faktorer = sort $ show a ++ show b
--       produkt  = sort $ show $ a * b
--   in  faktorer == produkt

breakChocolate :: Int -> Int -> Int
breakChocolate n m = if n*m <= 0 then 0 else n*m-1

main = do
  putStrLn "Is Martin a homophobe?"
  x <- getLine
  if map toLower x == "yes"
    then putStrLn "correct"
    else putStrLn "incorrect"

nbrOfStates b = product $ replicate b 2



-- dateNbDays :: Double -> Double -> Double -> String
-- dateNbDays a0 a p = takeWhile (<a) . iterate (+).(*p/36000) a0



--data ScoreType = Strike | Spare | Other Int

-- bowlingScore :: String -> Int
-- bowlingScore myFrames =

-- bowlingParser :: [String] -> [Int]
-- bowlingParser [a] = 0
-- bowlingParser lst@(x:xs)
--   | x=="X"        = bowlingConverter (take 3 lst) : bowlingParser (tail lst)
--   | elem '/' x    = bowlingConverter (take 2 lst) : bowlingParser (tail lst)
--   | otherwise     = bowlingConverter (take 1 lst) : bowlingParser (tail lst)
--
-- bowlingConverter :: [String] -> Int
-- bowlingConverter lst
--   | length == 1 = ord (lst!!0!!0) + ord (lst!!0!!1)
--   | length == 2 = 10 + ord (lst!!1!!0)
--   | length == 3 = 10 +


-- numberOfLuckyTickets :: (Int,Int) -> Int
-- numberOfLuckyTickets (nStart, nEnd) =

findUnique :: [Int] -> Int
findUnique = (head.concat).filter ((<2).length).group.sort

isPangram :: String -> Bool
isPangram = (26==) . length . group . sort . map toLower . filter isAlpha


-- reverseWords :: String -> String
-- reverseWords = (map reverse) . words



type Point = (Double,Double)

vrtxLst :: Double -> Double -> [Point]
vrtxLst a b = [( a/2,  b/2 ), (-a/2 , b/2 ), ( -a/2, -b/2 ), ( a/2 ,-b/2 )]

rotatePoint :: Double -> Point -> Point
rotatePoint r (x,y) = (x*cos r - y*sin r , x*sin r + y*cos r)

maxDY :: [Point] -> [Int]
maxDY pLst =
 let lowrVrtx = truncate $ snd $ pLst !! 2
     upprVrtx = truncate $ snd $ head pLst
 in  [lowrVrtx..upprVrtx]

rectCoords :: (Double -> Double -> Double) -> Point -> Point -> Int -> Double
rectCoords maxOrMin (a,b) (c,d) y =
  maxOrMin (fromIntegral y - (b-a)) ((d+c) - fromIntegral y)

rectangleRot :: Int -> Int -> Int
rectangleRot a b =
 let  rightX  = floor   . rectCoords min (rotaVrtx !! 2) (rotaVrtx!!3)
      leftX   = ceiling . rectCoords max (head rotaVrtx) (rotaVrtx!!1)
      rotaVrtx = map (rotatePoint (pi/4)) $ vrtxLst (fromIntegral a) (fromIntegral b)
      dX y    = (length . takeWhile (<= rightX y)) [(leftX y),(leftX y + 1)..]
 in                 sum . map dX $ maxDY rotaVrtx


-- parseMolecule :: String -> Either String [(String,Int)]
-- parseMolecule formula =

-- high :: String -> Int
-- high = sum.map fromEnum . words


--snygga versionen
-- rowSumOddNumbers :: Integer -> Integer
-- rowSumOddNumbers = sum . (splitPlaces [1..] [1,3..] !!).fromIntegral
--

rowSumOddNumbers :: Integer -> Integer
rowSumOddNumbers i = sum [i*(i-1)+1 , i*(i-1)+1+2 .. i*(i+1)-1]

-- doubleAngleTeller :: IO ()
-- doubleAngleTeller = do
--   putStrLn "Good morning! Which formula would you like to see?"
--   prompt
--   input       <- getLine
--   typeOfAngle <- sinOrCos input
--   sign        <- addOrSub input

-- doubleAngle :: String -> String
-- doubleAngle str = let
--     intro = sinOrCos str
--     in intro ++ "cos(y)" ++ sign ++ outro ++ "sin(y)"
--   where
--     intro =

sinOrCos :: String -> Maybe String
sinOrCos = flip find ["cos","sin"] . ((==) . map toLower) . take 3

addOrSub :: String -> Maybe Char
addOrSub str = flip find ['-','+'] . (filter ((=='-')||(=='+'))) str
