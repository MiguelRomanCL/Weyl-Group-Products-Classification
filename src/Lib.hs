module Lib
    ( someFunc
    ) where

import Data.List (nub, sort) 


someFunc :: IO ()
someFunc = putStrLn "someFunc"



type Cardinality = Integer
type SumDimIrreps = Integer
data Orders = Orders [Integer] deriving (Show)
data Group = Group Cardinality SumDimIrreps Orders deriving (Show)




oC3 = Orders [2, 3, 4, 6]
oC4 = Orders [2, 3, 4, 6, 8]
oC5 = Orders [2, 3, 4, 5, 6, 8, 10, 12]
oC6 = Orders [2, 3, 4, 5, 6, 8, 10, 12]
oD3 = Orders [2, 3, 4]
oD4 = Orders [2, 3, 4, 6]
oD5 = Orders [2, 3, 4, 5, 6, 8, 12]
oD6 = Orders [2, 3, 4, 5, 6, 8, 10, 12]
oD7 = Orders [2, 3, 4, 5, 6, 7, 8, 10, 12, 20, 24]
oS3 = Orders [2, 3]
oS4 = Orders [2, 3, 4]
oS5 = Orders [2, 3, 4, 5, 6]
oS6 = Orders [2, 3, 4, 5, 6]
oS7 = Orders [2, 3, 4, 5, 6, 7, 10, 12]
oS8 = Orders [2, 3, 4, 5, 6, 7, 8, 10, 12, 15]
oF4 = Orders [2, 3, 4, 6, 8, 12]
oG2 = Orders [2, 3, 6]
oE6 = Orders [2, 3, 4, 5, 6, 8, 9, 10, 12]
oE7 = Orders [2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 15, 18, 30]
oE8 = Orders [2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 15, 18, 20, 24, 30]

gC3 = Group        48     20 oC3
gC4 = Group       384     76 oC4
gC5 = Group      3840    312 oC5
gC6 = Group     46080   1384 oC6
gD3 = Group        24     10 oD3
gD4 = Group       192     44 oD4
gD5 = Group      1920    156 oD5
gD6 = Group     23040    752 oD6
gD7 = Group    322560   3256 oD7
gF4 = Group      1152    140 oF4
gG2 = Group        12      8 oG2
gS3 = Group         6      4 oS3
gS5 = Group       120     26 oS5
gS6 = Group       720     76 oS6
gS7 = Group      5040    232 oS7
gE6 = Group     51840    892 oE6
gE7 = Group   2903040  10208 oE7
gE8 = Group 696729600 199952 oE8



sord a = sort(nub(a))

cartord :: Integral a => [[a]] -> [a]
cartord [] = []
cartord [x] = x
cartord [xs,ys] = sord [ lcm x y | x <- xs , y<- ys]
cartord (x:xs) = cartord [x,(cartord xs)]


cart :: [Group] -> Group
cart []= undefined
cart [g]= g
cart [(Group gc gn (Orders go)),(Group hc hn (Orders ho))]= Group (gc*hc) (gn*hn) (Orders (cartord [go,ho]))
cart (g:gs) = cart [g,(cart gs)]



