
applyMaybe :: Maybe a -> ( a -> Maybe b) -> Maybe b
applyMaybe Nothing f =  Nothing
applyMaybe (Just x) f = f x

type Birds = Int
type Pole = (Birds, Birds)

landleft :: Birds -> Pole -> Maybe Pole
landleft n (l, r)
  | abs((l + n) - r) < 4 = Just (l + n , r)
  | otherwise            = Nothing

landright :: Birds -> Pole -> Maybe Pole
landright n (l, r)
  | abs((r + n) - l) < 4 = Just (l , r + n)
  | otherwise            = Nothing


foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y -> 
      Just (show x ++y ) ) )

foo2 :: Maybe String
foo2 = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)
