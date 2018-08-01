{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Parser
  (Err, parseStr, parseBin, parseInput, readNum, str2bin, bin2str) where

import Eval

import Data.Char
import Data.List
import Data.Maybe
import Data.Proxy
import Text.Parsec hiding ( satisfy, between, oneOf )

import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Text.Parsec                as P


comment = ';'
sigma = "012345[]/.+,<>#@"

type Err a = Either String a

parseTokens :: Num v => [Tok] -> Err (Fun v, [GNum v])
parseTokens = eShow . parse ((,) <$> fun <*> constInput <* eof) "source"

parseStr, parseBin :: Num v => String -> Err (Fun v, [GNum v])
parseStr = parseTokens . str2tok
parseBin = parseTokens . bin2tok

str2bin = C.putStr . tok2bin . str2tok
bin2str = putStrLn . tok2str . bin2tok


str2tok = mapMaybe (fmap toEnum . (`elemIndex` sigma))
        . unlines
        . map (takeWhile (comment/=))
        . lines

tok2str = (show =<<)


bin2tok = dropWhile (N0==)
        . ((\x-> let (a,b)=divMod x 16 in toEnum . fromIntegral <$> [a,b])=<<)
        . B.unpack
        . C.pack

tok2bin = B.pack
        . map (foldr1 $ (+).(16*))
        . takeWhile (not.null)
        . unfoldr (Just . splitAt 2)
        . map (fromIntegral . fromEnum)
        . (\ts -> if even (length ts) then ts else N0 : ts)


parseInput :: forall v . (Num v, Read v)
  => Bool -> Proxy v -> String -> Err (GNum v)
parseInput b6 _ = eShow . parse (input <* eof) "input" where

  tRead = read :: String -> v

  input = number <|> tuple

  number
    | b6 = Const . fromB6 . map (tRead . pure) <$> many1 (oneOf "012345")
    | otherwise = Const . tRead <$> many1 digit

  tuple = do
    spaces *> char '(' *> spaces
    x <- input
    spaces *> char ',' *> spaces
    y <- input
    spaces *> char ')'
    pure $ x :. y

readNum num | all isDigit num = Right (read num)
            | otherwise = Left $ '\'' : num ++ "' is an invalid number"


eShow (Left s)  = Left (show s)
eShow (Right x) = Right x


data Tok = N0 | N1 | N2 | N3 | N4 | N5 | CO | CC | I | Z | S | E | L | R | P | U
  deriving (Enum,Eq)

instance Show Tok where show t = [sigma !! fromEnum t]

fun = bltIn <|> proj <|> comp <|> prim <|> mu where

  bltIn = BltIn . \case
     Z -> Zero; S -> Succ; E -> Enc; L -> Lft; R -> Rgt; _ -> redundant
    <$> oneOf [Z .. R]

  proj = tok I >> Proj . readNX <$> many1 (oneOf [N0 .. N5])

  comp = between CO CC $ Comp <$> fun <*> many fun

  prim = tok P >> Prim <$> fun <*> fun

  mu = tok U >> Mu <$> fun

  redundant = error "redundant pattern match"

constInput :: Num v => Parsec [Tok] () [GNum v]
constInput = sepBy input (tok E) where
  input = Const . readNX <$> many1 (oneOf [N0 .. N5])

readNX :: Num v => [Tok] -> v
readNX = fromB6 . map (fromIntegral . fromEnum)

fromB6 :: Num v => [v] -> v
fromB6 = foldl' ((+).(6*)) 0

satisfy pred = tokenPrim showTok nextPos test
  where showTok x = '\'' : show x ++ "'"
        test x | pred x    = Just x
               | otherwise = Nothing
        nextPos pos _ _ = incSourceColumn pos 1

tok t = satisfy (t==)

between o c = P.between (tok o) (tok c)
oneOf ts = satisfy (`elem` ts)
