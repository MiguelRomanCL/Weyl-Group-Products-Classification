module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


import Data.List (nub, sort) 


ordersC3= map (fromIntegral) ([2,3,4,6]);
ordersC4= map (fromIntegral) ([2,3,4,6,8]);
ordersC5= map (fromIntegral) ([2,3,4,5,6,8,10,12]);
ordersC6= map (fromIntegral) ([2,3,4,5,6,8,10,12]);
ordersD3= map (fromIntegral) ([2,3,4]);
ordersD4= map (fromIntegral) ([2,3,4,6]);
ordersD5= map (fromIntegral) ([2,3,4,5,6,8,12]);
ordersD6= map (fromIntegral) ([2,3,4,5,6,8,10,12]);
ordersD7= map (fromIntegral) ([2,3,4,5,6,7,8,10,12,20,24]);
ordersS3= map (fromIntegral) ([2,3]);
ordersS4= map (fromIntegral) ([2,3,4]);
ordersS5= map (fromIntegral) ([2,3,4,5,6]);
ordersS6= map (fromIntegral) ([2,3,4,5,6]);
ordersS7= map (fromIntegral) ([2,3,4,5,6,7,10,12]);
ordersS8= map (fromIntegral) ([2,3,4,5,6,7,8,10,12,15]);
ordersF4= map (fromIntegral) ([2,3,4,6,8,12]);
ordersG2= map (fromIntegral) ([2,3,6]);
ordersE6= map (fromIntegral) ([2,3,4,5,6,8,9,10,12]);
ordersE7= map (fromIntegral) ([2,3,4,5,6,7,8,9,10,12,14,15,18,30]);
ordersE8= map (fromIntegral) ([2,3,4,5,6,7,8,9,10,12,14,15,18,20,24,30]);


gC3= (ordersC3, 20,48);
gC4= (ordersC4, 76,384);
gC5= (ordersC5, 312,3840);
gC6= (ordersC6, 1384,46080);
gD3= (ordersD3, 10,24);
gD4= (ordersD4, 44,192);
gD5= (ordersD5, 156,1920);
gD6= (ordersD6, 752,23040);
gD7= (ordersD7, 3256,322560);
gF4= (ordersF4, 140,1152);
gG2= (ordersG2, 8,12);
gS3= (ordersS3, 4,6);
gS5= (ordersS5, 26,120);
gS6= (ordersS6, 76,720);
gS7= (ordersS7, 232,5040);
gE6= (ordersE6, 892,51840);
gE7= (ordersE7, 10208,2903040);
gE8= (ordersE8, 199952,696729600);

sord a = sort(nub(a))

cart :: Integral a => [[a]] -> [a]
cart [] = []
cart [x] = x
cart [a,b] = sord [ lcm x y | x <- a , y<- b]
cart (x:xs) = cart [x,(cart xs)]

