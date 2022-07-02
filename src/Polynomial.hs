module Polynomial where

type Polynomial a = [a]

zipWithPoly :: (a -> b -> c) -> a -> b -> Polynomial a -> Polynomial b -> Polynomial c
zipWithPoly _ _  _  []     []     = []
zipWithPoly f a' b' (x:xs) []     = f x  b' : zipWithPoly f a' b' xs []
zipWithPoly f a' b' []     (y:ys) = f a' y  : zipWithPoly f a' b' [] ys
zipWithPoly f a' b' (x:xs) (y:ys) = f x  y  : zipWithPoly f a' b' xs ys

addPoly :: Num a => Polynomial a -> Polynomial a -> Polynomial a
addPoly = zipWithPoly (+) 0 0

subPoly :: Num a => Polynomial a -> Polynomial a -> Polynomial a
subPoly = zipWithPoly (-) 0 0

multPoly :: Num a => Polynomial a -> Polynomial a -> Polynomial a
multPoly []     _  = []
multPoly _      [] = []
multPoly (x:xs) ys = addPoly (mult x ys) (multPoly xs (0:ys))
    where
        mult x []     = []
        mult x (y:ys) = x * y : mult x ys
