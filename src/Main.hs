{-# LANGUAGE DataKinds
      , LambdaCase
      , ScopedTypeVariables
      , TemplateHaskell
      , TypeApplications
      , TypeOperators #-}

module Main where

import Eval
import Parser

import Data.Modular
import Data.Proxy
import GHC.TypeLits
import Lens.Micro
import Lens.Micro.TH
import System.Console.GetOpt
import System.Environment

usage = " usage: mu6 (-h | -e expr | file) [-a] [-v] INPUTS"

data Fmt = Str | Bin
data Flags = F
  { _expr      :: Bool
  , _format    :: Fmt
  , _translate :: Bool
  , _modulus   :: Maybe (Err Integer)
  , _base6     :: Bool
  , _ascii     :: Bool
  , _help      :: Bool
  }

defaults = F False Bin False Nothing False False False

makeLenses ''Flags

runFlags (F _ _ _ _ _ _ True) _ = putStrLn $ usageInfo usage options
runFlags (F e _ _ _ _ _ _) [] | wat <- if e then "expression" else "file"
                            = die $ "no " ++ wat ++ " provided"
runFlags (F e f t m b a _) (x:xs) = do
  src <- if e then pure x else readFile x
  either (ioError . userError) id $
    if t then pure $ translateFmt f src
    else do
      let run typ = do (p,ci) <- parseFmt f src
                       inputs <- mapM (parseInput b typ) xs
                       pure . putStrLn . pretty a b . eval p $ ci ++ inputs
      sequence m >>= \case
        Nothing -> run (Proxy @Integer)
        Just m  -> case someNatVal m of
          Nothing -> Left $ "modulus must be positive: " ++ show m
          Just (SomeNat (_ :: Proxy modulus)) -> run (Proxy @(Integer/modulus))

  where parseFmt Str = parseStr
        parseFmt _   = parseBin

        translateFmt Str = str2bin
        translateFmt _   = bin2str

main = getOpt Permute options <$> getArgs >>= \case
  (fs,as,[]) -> runFlags (foldr ($) defaults fs) as
  (_,_,err)  -> die $ concat err

options =
  [ Option "e" ["expr"] (NoArg $ expr .~ True) "evaluate an expression"
  , Option "a" ["ascii"] (NoArg $ ascii .~ True) "set ascii output mode"
  , Option "6" ["heximal"] (NoArg $ base6 .~ True) "heximal I/O mode"
  , Option "v" ["verbose"] (NoArg $ format .~ Str) "treat source as ascii string"
  , Option "t" ["translate"] (NoArg $ translate .~ True) "translate source code"
  , Option "m" ["modulus"] (ReqArg ((modulus ?~) . readNum) "M") "operate in N/M"
  , Option "h" ["help"] (NoArg $ help .~ True) "print this help"
  ]

die m = ioError . userError $ m ++ "\n" ++ usageInfo usage options
