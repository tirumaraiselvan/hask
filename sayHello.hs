doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else doubleMe x

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

length' xs = sum [1 | _ <- xs]

removeNonUppercase:: [Char] -> [Char]
removeNonUppercase st = [ c | c <-st , c `elem` ['A' .. 'Z']]

lucky:: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, no luck"

factorial :: (Integral a) => a -> a

factorial 0 = 1
factorial n = n * factorial (n-1)

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++  show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ ", " ++ show y

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "Thin"
    | bmi <= normal = "Normal"
    | bmi <= fat = "Fat"
    | otherwise = "You are done"
    where bmi = weight/height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

initial :: String -> String -> String
initial fn ln = [f] ++"."++ [l]
    where (f: _) = fn
          (l: _) = ln

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w,h) <- xs]
    where bmi weight height = weight/height ^ 2

calcBmis' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis' xs =
    let bmi weight height = weight/height ^ 2
    in [bmi w h | (w, h) <- xs]


describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [x] -> "a singleton"
                                               xs -> "longer list"

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list is not defined"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Integral i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | n == 1 = [x]
  | n  > 1 = x: (replicate' (n-1) x)

take' :: (Integral i) => i -> [a] -> [a]
take' n xx
  | n <= 0 = []
  | null xx = []
  | otherwise =  x: (take' (n-1) xs)
                  where (x:xs) = xx

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys 

elem' :: (Eq a) => a -> [a] -> Bool
elem' n xx
  | null xx = False
  | otherwise = (n == x) || elem' n xs
                  where (x:xs) = xx 

insertsort :: (Ord a) => [a] -> [a]
insertsort [] = []
insertsort [x] = [x]
insertsort (x:xs) = 
  let yy = insertsort xs
      y:ys = yy
  in 
      if x >= y
      then y: insertsort (x:ys)
      else x: yy

compareWithHundred:: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y: zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

squares = map (map (^2) ) [[1,2], [4,5]]

greaterThanThree = filter ( > 3) [1,2,3,4,5,6]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let leftSorted  = quicksort (filter (<=x) xs )
      rightSorted = quicksort (filter (> x) xs )
  in leftSorted ++ [x] ++ rightSorted 

largestDivisible :: (Integral a) => a
largestDivisible = head ( filter p [100000, 99999..] )
                    where p x = x `mod` 3829 == 0

isCollatzLong :: (Integral a) => a -> Bool
isCollatzLong n = 
  (collatzLen n) > 15
  where collatzLen n 
          | n ==1 = 1
          | odd n = 1 + collatzLen (3*n + 1)
          | even n = 1 + collatzLen (n `div` 2)

longCollatz = filter isCollatzLong [1..100]



