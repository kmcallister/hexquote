{-# LANGUAGE
    TemplateHaskell
  , NamedFieldPuns
  , CPP #-}
module Data.Hex.Quote
    ( hex
    ) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Word
import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Text.Parsec hiding ( (<|>), many )
import Text.Parsec.Token
import Text.Parsec.String
import Text.Parsec.Language

import qualified Data.ByteString as B
import qualified Data.IntMap     as IM

dropComments :: String -> String
dropComments = go where
    go ('-':'-':xs) = go (dropWhile (/= '\n') xs)
    go (x:xs) = x : go xs
    go [] = []

hexMap :: IM.IntMap Word8
hexMap = IM.fromList . map (first ord) $ concat
    [ zip ['0'..'9'] [0..9]
    , zip ['A'..'F'] [10..15]
    , zip ['a'..'f'] [10..15] ]

parseHex :: String -> [Word8]
parseHex = pair . catMaybes . map get . dropComments where
    get v = IM.lookup (ord v) hexMap
    pair (h:l:xs) = (h*16 + l) : pair xs
    pair _ = []

liftBS :: [Word8] -> Q Exp
liftBS xs = lift $ map conv xs where
    -- We can't lift Word8, but Int lifts to a polymorphic literal
    conv :: Word8 -> Int
    conv = fromIntegral

hexExp :: String -> Q Exp
hexExp xs = [| B.pack $(liftBS $ parseHex xs) |]

data Tok
    = Lit  [Word8]
    | Take String (Maybe Integer)
    deriving (Show)

parseToks :: Parser [Tok]
parseToks = whiteSpace >> lexeme (many parseTok) <* eof where
    parseTok :: Parser Tok
    parseTok = (angles (Take <$> identifier <* symbol ":" <*> len))
           <|> ((Lit . parseHex) <$> lexeme (many1 hexDigit))
    len = lexeme (
            (Nothing <$  symbol "rest")
        <|> (Just    <$> decimal))
    TokenParser { whiteSpace, identifier, decimal,
                  symbol, angles, lexeme }
        = makeTokenParser emptyDef
              { identStart    = letter   <|> char '_'
              , identLetter   = alphaNum <|> oneOf "_'"
              , caseSensitive = True }

mkExtract :: [Tok] -> Q Exp
mkExtract [] = [| \x -> guard (B.null x) >> Just [] |]
mkExtract (Lit xs : ts) = let n = length xs in
    [| \x -> case B.splitAt n x of
                 (y,z) | B.unpack y == $(liftBS xs) -> $(mkExtract ts) z
                       | otherwise -> Nothing |]
mkExtract (Take _ (Just n) : ts) = let nn = fromIntegral n in
    [| \x -> case B.splitAt nn x of
                 (y,z) | B.length y == nn -> (y:) <$> $(mkExtract ts) z
                       | otherwise -> Nothing |]
mkExtract (Take _ _ : ts)  = [| \x -> (x:) <$> $(mkExtract ts) B.empty |]

mkPat :: [Tok] -> Q Pat
mkPat ts = viewP (mkExtract ts) (conP 'Just [listP vars]) where
    mkV "_" = wildP
    mkV n   = varP (mkName n)
    vars = [ mkV n | Take n _ <- ts ]

hexPat :: String -> Q Pat
hexPat xs = case parse parseToks "Data.Hex.Quote pattern" (dropComments xs) of
    Left  e -> error (show e)
    Right v -> mkPat v

hex :: QuasiQuoter
hex = QuasiQuoter
    { quoteExp  = hexExp
    , quotePat  = hexPat
#if MIN_VERSION_template_haskell(2,5,0)
    , quoteType = const (error "no type quote for Data.Hex.Quote")
    , quoteDec  = const (error "no decl quote for Data.Hex.Quote")
#endif
    }
