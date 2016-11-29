data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface ( Circle _  r ) = pi * (r ^ 2)
surface ( Rectangle (Point x1 y1) (Point x2 y2) ) = ( abs $ x2 - x1 ) * ( abs $ y2 - y1 )

nudgeShape :: Shape -> Float -> Float -> Shape
nudgeShape (Circle (Point x1 y1) r) offX offY = Circle (Point (x1 + offX) (y1 + offY) ) r
nudgeShape (Rectangle (Point x1 y1) (Point x2 y2) ) offX offY = Rectangle ( Point (x1 + offX) (y1 + offY) )  ( Point (x2 + offX) (y2 + offY) )

data Person = Person {  firstName :: String,
                        lastName :: String,
                        age :: Int,
                        height :: Float,
                        phoneNumber :: String,
                        flavor :: String
                      } deriving (Show)





