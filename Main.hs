{-# LANGUAGE TupleSections #-}

module Main where

import Control.Comonad

data U a = U [a] a [a]

data U2 a = U2 (U (U a))

focusL :: U a -> U a
focusL (U (l:ls) x rs) = U ls l (x:rs)

focusR :: U a -> U a
focusR (U ls x (r:rs)) = U (x:ls) r rs

instance Functor U where
  fmap f (U ls x rs) = U (f <$> ls) (f x) (f <$> rs)

instance Comonad U where
  extract (U _ x _) = x
  duplicate w = U (tail $ iterate focusL w) w (tail $ iterate focusR w)

focusL2 :: U2 a -> U2 a
focusL2 (U2 u) = U2 (focusL <$> u)

focusR2 :: U2 a -> U2 a
focusR2 (U2 u) = U2 (focusR <$> u)

focusD2 :: U2 a -> U2 a
focusD2 (U2 u) = U2 (focusL u)

focusU2 :: U2 a -> U2 a
focusU2 (U2 u) = U2 (focusR u)

instance Functor U2 where
  fmap f (U2 u) = U2 (fmap f <$> u)

instance Comonad U2 where
  extract (U2 (U _ (U _ x _) _)) = x
  duplicate w = U2 vh where
    h = U (tail $ iterate focusL2 w) w (tail $ iterate focusR2 w)
    vh = U (tail $ iterate (fmap focusD2) h) h (tail $ iterate (fmap focusU2) h) 

focus :: Int -> Int -> U2 a -> U2 a
focus 0 0 u = u
focus m 0 u | m < 0 = focus (m+1) 0     (focusL2 u)
            | m > 0 = focus (m-1) 0     (focusR2 u)
focus m n u | n < 0 = focus m     (n+1) (focusD2 u)
            | n > 0 = focus m     (n-1) (focusU2 u)

setFocus :: a -> U a -> U a
setFocus x (U ls _ rs) = U ls x rs

setFocus2 :: a -> U2 a -> U2 a
setFocus2 x (U2 u) = U2 $ setFocus (setFocus x $ extract u) u

repeatU :: a -> U a
repeatU x = U (repeat x) x (repeat x)

repeatU2 :: a -> U2 a
repeatU2 x = U2 $ U (repeat u) u (repeat u) where
  u = repeatU x

set :: [(Int,Int,a)] -> U2 a -> U2 a
set ps u = go 0 0 ps u where
  go x y [] u = focus (-x) (-y) u
  go x y ((x',y',v):ps) u = go x' y' ps u' where
    dx = x' - x
    dy = y' - y
    u' = setFocus2 v . focus dx dy $ u 

showU2 :: (Int,Int) -> (Int,Int) -> U2 Bool -> String
showU2 (x,y) (x',y') u = out where
  convert True  = "##"
  convert False = "  "

  xss = map (\(x,y) -> extract $ focus x y u) <$> [(,_y) <$> [x..x'] | _y <-[y..y']] 
  ls  = reverse $ "":((concat . map convert) <$> xss)
  _:out = concat $ (map ('\n':) ls)

conwayRule :: U2 Bool -> Bool
conwayRule (U2 (U (ds:_) xs (us:_)) ) = x' where
  U (dl:_) d (dr:_) = ds
  U (l:_)  x (r:_)  = xs
  U (ul:_) u (ur:_) = us

  ns = [dl,d,dr,l,r,ul,u,ur]
  count = length [() | True <- ns]

  x' = if x
    then count == 2 || count == 3
    else count == 3

example :: [(Int,Int,Bool)]
example =
  [ ( 0 , 0, True)
  , (-1 , 0, True)
  , ( 1 , 0, True)
  , ( 0, -1, True)
  , ( 0,  1, True)
  ]

main :: IO ()
main = loop 11 u0 where
  u0 = set example (repeatU2 False)
  printu = putStrLn . showU2 (-5, -5) (5, 5)
  loop 0 u = printu u
  loop n u = printu u >> loop (n-1) (extend conwayRule u)

