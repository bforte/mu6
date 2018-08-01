{-# LANGUAGE DeriveFoldable, DeriveFunctor, FlexibleContexts #-}

module Eval where

import Data.Char
import Data.List
import Numeric

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

pretty a b6 = if a then prettyA else prettyP where

  prettyA = foldr ((:).chr.(`mod`128).fromIntegral) []

  prettyP (Const v)
    | b6 = showIntAtBase 6 ("012345"!!) v ""
    | otherwise = show v
  prettyP (x :. y)  = '(' : prettyP x ++ ',' : prettyP y ++ ")"

eval :: (Enum v,Eq v,Integral v,Show v,Num v) => Fun v -> [GNum v] -> GNum v
eval f xs = case f of
  Proj i    -> xs !! i
  Comp f gs -> eval f $ (`eval` xs) <$> gs
  Prim f g  -> prim f g xs
  Mu   f    -> head [ n | n <- Const <$> [0..], eval f (n:xs) == Const 0 ]
  BltIn f -> case f of
    Zero   -> Const 0
    Succ   -> (1+) <$> head' xs
    Enc    -> enc xs
    Lft    -> lft $ head' xs
    Rgt    -> rgt $ head' xs

  where prim f _ (Const 0:xs) = eval f xs
        prim f g (x:xs) | p <- withFst (subtract 1) x
                        = eval g $ [p,prim f g $ p : xs]++xs
        prim f _ [] = eval f []

        withFst f (Const n) = Const (f n)
        withFst f (x :. y)  = withFst f x :. y

        enc []  = Const 0
        enc [x] = Const $ fromGN x
        enc xs  = foldr1 (:.) xs

        lft (Const x) = fromNat x
        lft (x :.  _) = x

        rgt (Const x) = fromNat x
        rgt (_ :.  y) = y

        head' (x:_) = x
        head' [] = Const 0

        (x:_) !! 0 = x
        (_:xs) !! i = xs !! (i-1)
        _ !! _ = Const 0


nKtoN = foldl1' (\x y -> 2^x * (2*y+1) - 1)

fromGN :: Integral a => GNum a -> a
fromGN t = nKtoN [gnEnc t, nKtoN $ foldr (:) [] t] where

  nKtoN = foldl1' (\x y -> 2^x * (2*y+1) - 1)

  gnEnc (Const _) = 0
  gnEnc (x :.  y) = 1 + nKtoN [gnEnc x, gnEnc y]


fromNat :: Integral p => p -> GNum p
fromNat n = fst $ t # ntoNK c es where
  [s,es] = ntoNK 2 n
  (c,t)  = gnDec s

  ntoNK k = reverse . go k where
    go 1 n = [n]
    go i n | (g,x) <- go' 0 (n+1) = x : go (i-1) g

    go' l n | even n = go' (l+1) (n `div` 2)
            | otherwise = (l, n `div` 2)

  gnDec 0 = (1,Const ())
  gnDec n = (lc+rc, lt :. rt) where
    [l,r] = ntoNK 2 (n-1)
    (lc,lt) = gnDec l
    (rc,rt) = gnDec r

  (Const _) # (z:zs) = (Const z,zs)
  (Const _) # _ = error "won't happen"
  (x :.  y) # zs | (l,zs') <- x # zs, (r,zs'') <- y # zs'
                 = (l :. r, zs'')
