{-# LANGUAGE OverloadedStrings #-}

import Haste.Foreign
import Data.List
import Data.Maybe

import qualified Data.Map as M

data RDir = CW | CCW deriving Eq
data SDir = SLeft | STop | SRight | SDown deriving (Eq, Show)
data Matrix a = Matrix [[a]] deriving (Eq, Show)

class Rotable m where
	getCount :: m -> Int
	--singleRotation
	srot :: m -> RDir -> m
	srot m CW = cwrot m
	srot m CCW = cwwrot m

	cwrot :: m -> m
	cwrot m = rotate m CCW ((getCount m) - 1) 

	cwwrot :: m -> m
	cwwrot m = rotate m CW ((getCount m) - 1) 


	rotate :: m -> RDir -> Int -> m
	rotate m d n = foldr (\x -> \m' -> x m') m (replicate (n `mod` (getCount m)) ((flip srot) d))


instance Rotable (Matrix a) where
	getCount _ = 4
	cwrot (Matrix m) = Matrix $ (transpose . reverse) m

instance Rotable SDir where	 
	getCount _ = 4
	cwrot SLeft = STop
	cwrot STop = SRight
	cwrot SRight = SDown
	cwrot SDown = SLeft

  
--apply function until it makes changes
appMax :: (Eq a) => (a -> a) -> a -> a
appMax f a | a' == a = a'
	   | otherwise = appMax f a'
	where a' = f a

--maximal reduction of matrix in given direction
reduce :: Matrix Int -> SDir -> Matrix Int
reduce m d = appMax (flip swipe $ d) m



argmin f l@(x:xs) = fst $ foldr mn (x, (f x)) (map (\t -> (t, f t)) l)
	where mn p1@(a, b) p2@(a', b') = if b < b' then p1 else p2

fld :: [Int] -> [Int]
fld xs = l' ++ (replicate ((length xs) - (length l')) 0) 
	where l' = reverse $ foldr f [] (reverse xs)
	      f :: Int -> [Int]	-> [Int]
	      f b []  = [b]
	      f b [0] = [b]	
	      f 0 as = as
	      f b (a:as) = if (b == a) then (a + b:as) else (b:a:as)

transp :: Matrix a -> Matrix a
transp (Matrix m) = Matrix (transpose m)

getSnake :: Matrix a -> [a]
getSnake (Matrix as) = concat $ map f (zip as [1, 2 ..])
	where f (a,b) = if (b `mod` 2) /= 0 then a else reverse a

dist :: SDir -> SDir -> Int
dist d1 d2 = (fromMaybe 0 $ findIndex (== d2) [rotate d1 CW n | n <- [0 .. getCount d2]])

swipe :: Matrix Int -> SDir -> Matrix Int
swipe m d = rotate m' CW dst
	where m' = swipeLeft $ rotate m CCW dst
	      dst = dist SLeft d	
	      swipeLeft (Matrix tm) = Matrix $ map fld tm

getAllSnakes :: Matrix a -> [[a]]
getAllSnakes m = [getSnake m' | m' <- (origRots ++ transposeRots) ]
	where origRots = map (rotate m CW) [0..(getCount m) - 1]
	      transposeRots = map (rotate (transp m) CW) [0..(getCount m) - 1]

trimZeros :: [Int] -> [Int]
trimZeros [] = []
trimZeros as = (reverse . snd . ((flip splitAt) (reverse as)) . (fromMaybe ((length as) - 1)) . (findIndex (/=0)) . reverse) as

	      
notEmptysCount :: Matrix Int -> Int
notEmptysCount (Matrix m) = length $ filter (/=0) (concat m)

--getDirections for reduction
getRedDirs :: SDir -> [SDir]
getRedDirs d = map (\x -> rotate x CW dst) [SLeft, STop]
	where dst = dist SLeft d


snakeCost :: [Int] -> Int
snakeCost [] = 0 
snakeCost s@(h:xs) = (fst $ snd  $ foldr f ((0, 0), (0, h)) (reverse s))
	where f a ((pos, holes), (sm, hd)) = let cst = (pairCost pos holes hd a) in ((pos + 1, holes'), ((sm + cst), a))
	     	   where holes' | hd >= a = holes
			        | otherwise = holes + 1
	      --lengthPenalty = 2 * (length (trimZeros s))^2

pairCost :: Int -> Int -> Int -> Int -> Int
pairCost pos holes a b 	| a >= b = 0
		        | otherwise =   (b - a) * ((18 - pos)^1)

--measure of irreducibility of given matrix
irreducibilityCost :: Matrix Int -> [SDir] -> Int
irreducibilityCost m@(Matrix mat) ds = minimum $ map cst $ map (reduce m) ds
	where cst mt = n'^(n `div` 4)
		where n'= (notEmptysCount mt)
	      n = (notEmptysCount m)


cost :: Matrix Int -> SDir -> Int
cost m d = (orderCost m') + (irreducibilityCost m' (getRedDirs d))
	where m' = swipe m d

orderCost :: Matrix Int -> Int
orderCost m = minimum $ map snakeCost (getAllSnakes m) 

isAvailable :: Matrix Int -> SDir -> Bool
isAvailable m d = m /= (swipe m d)

availableMoves :: Matrix Int -> [SDir]
availableMoves m = filter (isAvailable m) [SLeft, STop, SRight, SDown]

getMove :: Matrix Int -> Maybe SDir
getMove m | ((length movs) == 0) = Nothing 
	  | otherwise = Just $ argmin (cost m) movs
	where movs = availableMoves m



getM :: [[Int]] -> IO Int
getM m = return $ case getMove (Matrix m) of 
	Just d -> toJsCode d
	Nothing -> -1

toJsCode d = case d of 
	SLeft -> 3
	STop -> 0
	SRight -> 1
	SDown -> 2

main = do 
	export "getM" getM


 	





