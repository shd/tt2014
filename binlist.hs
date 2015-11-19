data BL a = Nil | Zero (BL (a,a)) | One a (BL(a,a)) deriving (Show)

add :: a -> BL a -> BL a
add x Nil = One x Nil
add x (Zero a) = One x a
add x (One t a) = Zero (add (x,t) a)

nth :: Int -> BL a -> a
nth n (Zero l) = (if n `mod` 2 == 0 then fst else snd) $ nth (n `div` 2) l
nth 0 (One x _) = x
nth n (One a l) = (if n `mod` 2 /= 0 then fst else snd) $ nth ((n-1) `div` 2) l

ll = foldl (\x y -> add y x) Nil [1..100]

main = do
	print $ ll
	print $ zip [nth i ll| i <- [0..99]] [1..100]