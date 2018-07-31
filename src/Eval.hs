{-# LANGUAGE DeriveFoldable, DeriveFunctor, FlexibleContexts #-}

module Eval where

import Data.Char


data CFun = Zero | Succ | Enc | Lft | Rgt
  deriving (Enum,Show)

data Fun v = BltIn CFun
           | Proj Integer
           | Comp (Fun v) [Fun v]
           | Prim (Fun v) (Fun v)
           | Mu (Fun v)
  deriving Show

data GNum v = Const v | GNum v :. GNum v
  deriving (Eq,Foldable,Functor,Show)

pretty a = if a then prettyA else prettyP where
  prettyA = foldr ((:).chr.(`mod`128).fromIntegral) []
  prettyP (Const v) = show v
  prettyP (x :. y)  = '(' : prettyP x ++ ',' : prettyP y ++ ")"

eval :: (Enum v,Eq v,Num v) => Fun v -> [GNum v] -> GNum v
eval f xs = case f of
  Proj i    -> xs !! i
  Comp f gs -> eval f $ (`eval` xs) <$> gs
  Prim f g  -> prim f g xs
  Mu   f    -> head [ n | n <- Const <$> [0..], eval f (n:xs) == Const 0 ]
  BltIn f -> case f of
    Zero   -> Const 0
    Succ   -> (1+) <$> head' xs
    Enc    -> enc xs
    Lft    -> enc $ lft <$> xs
    Rgt    -> enc $ rgt <$> xs

  where prim f _ (Const 0:xs) = eval f xs
        prim f g (x:xs) | p <- withFst (subtract 1) x
                        = eval g $ [p,prim f g $ p : xs]++xs
        prim f _ [] = eval f []

        withFst f (Const n) = Const (f n)
        withFst f (x :. y)  = withFst f x :. y

        enc [] = Const 0
        enc xs = foldr1 (:.) xs

        lft (x :. _) = x
        lft x = x

        rgt (_ :. y) = y
        rgt x = x

        head' (x:_) = x
        head' [] = Const 0

        (x:_) !! 0 = x
        (_:xs) !! i = xs !! (i-1)
        _ !! _ = Const 0
