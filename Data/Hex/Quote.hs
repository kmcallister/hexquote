{-# LANGUAGE
    TemplateHaskell
  , NamedFieldPuns
  , CPP #-}
module Data.Hex.Quote
    ( -- * The quasiquoter
      hex
      -- * Helper functions
    , parseHex
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

-- | The hexadecimal parser used for @'hex'@ expressions.
parseHex :: String -> [Word8]
parseHex = pair . catMaybes . map get . dropComments where
    get v = IM.lookup (ord v) hexMap
    pair (h:l:xs) = (h*16 + l) : pair xs
    pair _ = []

-- We can't lift Word8, but Int lifts to a polymorphic literal
liftBS :: [Word8] -> Q Exp
liftBS xs = lift (map fromIntegral xs :: [Int])

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
mkExtract (Take _ Nothing : ts) = [| \x -> [x] <$ $(mkExtract ts) B.empty |]

mkPat :: [Tok] -> Q Pat
mkPat ts = viewP (mkExtract ts) (conP 'Just [listP vars]) where
    mkV "_" = wildP
    mkV n   = varP (mkName n)
    vars = [ mkV n | Take n _ <- ts ]

hexPat :: String -> Q Pat
hexPat xs = case parse parseToks "Data.Hex.Quote pattern" (dropComments xs) of
    Left  e -> error (show e)
    Right v -> mkPat v

{- |

As an expression, the @'hex'@ quasiquoter provides hexadecimal @'B.ByteString'@
literals:

>import Data.Hex.Quote
>import qualified Data.ByteString as B
>
>main = B.putStr [hex|
>    57 65 2c 20 61 6c 6f 6e 65 20 6f 6e 20 65 61 72
>    74 68 2c 20 63 61 6e 20 72 65 62 65 6c 20 61 67
>    61 69 6e 73 74 20 74 68 65 20 74 79 72 61 6e 6e
>    79 20 6f 66 20 74 68 65 20 73 65 6c 66 69 73 68
>    20 72 65 70 6c 69 63 61 74 6f 72 73 2e 0a |]

All characters other than @0123456789abcdefABCDEF@ are ignored, including
whitespace.  Comments start with \"@--@\" and continue to end-of-line:

>code = [hex|
>    7e3a          -- jle  0x3c
>    4889f5        -- mov  rbp, rsi
>    bb01000000    -- mov  ebx, 0x1
>    488b7d08 |]   -- mov  rdi, [rbp+0x8]

When using @'hex'@ as a pattern, you can include placeholders of the form
@\<name:size\>@, where

* @name@ is a Haskell identifier, or the wildcard pattern \"@_@\"

* @size@ is the size of the field in bytes, or the word @rest@ to consume
  the rest of the @'B.ByteString'@.

The named placeholders bind local variables of type @'B.ByteString'@.  Here's
an example of pattern-matching an IPv4-over-Ethernet-II frame:

>import Data.Hex.Quote
>
>describe [hex|
>    <src_mac:6> <dst_mac:6> 08 00  -- ethernet header
>    45 <_:1> <len:2>               -- start of IP header
>    <_:rest>                       -- discard remaining frame
>  |] = (src_mac, dst_mac, len)
>
>describe _ = error "unknown frame"

Quasiquotes require the @QuasiQuotes@ extension.  In pattern context, @'hex'@ also
requires the @ViewPatterns@ extension.

-}

hex :: QuasiQuoter
hex = QuasiQuoter
    { quoteExp  = hexExp
    , quotePat  = hexPat
#if MIN_VERSION_template_haskell(2,5,0)
    , quoteType = const (error "no type quote for Data.Hex.Quote")
    , quoteDec  = const (error "no decl quote for Data.Hex.Quote")
#endif
    }
