{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.Math
  ( HasMath(..)
  , HTMLMathMethod(..)
  , mathSpec )
where
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.TokParsers
import Commonmark.Html
import Data.Typeable
import GHC.Generics
import Text.Parsec
import Data.Maybe (mapMaybe)
import Data.Text (Text, pack)
import qualified Text.TeXMath as TeXMath
import qualified Text.XML.Light as XML
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))
#endif

data HTMLMathMethod = PlainMath
                    | MathML
                    deriving (Show, Read, Eq, Typeable, Generic)

mathSpec :: (Monad m, IsBlock il bl, IsInline il, HasMath il)
         => HTMLMathMethod -> SyntaxSpec m il bl
mathSpec mathMethod = mempty
  { syntaxInlineParsers = [withAttributes (parseMath mathMethod)]
  }

class HasMath a where
  inlineMath  :: forall m.Monad m => HTMLMathMethod -> Text  -> InlineParser m a
  displayMath :: forall m.Monad m => HTMLMathMethod -> Text -> InlineParser m a

instance HasMath (Html a) where
  inlineMath mathMethod t =
    case mathMethod of
      PlainMath -> pure $ addAttribute ("class", "math inline") $ htmlInline "span" $ Just $ htmlRaw ("\\(" <> t <> "\\)")
      MathML    ->
        -- addAttribute ("xmlns", "http://www.w3.org/1998/Math/MathML") .
        -- htmlInline "math" . Just <$>
        pMathML TeXMath.DisplayInline t
  displayMath mathMethod t =
    case mathMethod of
      PlainMath -> pure $ addAttribute ("class", "math display") $ htmlInline "span" $ Just $ htmlRaw ("\\[" <> t <> "\\]")
      MathML    -> pMathML TeXMath.DisplayBlock t

instance (HasMath i, Monoid i) => HasMath (WithSourceMap i) where
  inlineMath mathMethod t  = fmap (<$ addName "inlineMath" ) (inlineMath  mathMethod t)
  displayMath mathMethod t = fmap (<$ addName "displayMath") (displayMath mathMethod t)

parseMath :: (Monad m, HasMath a) => HTMLMathMethod -> InlineParser m a
parseMath mathMethod = pDisplayMath mathMethod <|> pInlineMath mathMethod

pInlineMath :: (Monad m, HasMath a) => HTMLMathMethod -> InlineParser m a
pInlineMath mathMethod = try $ do
  symbol '$'
  notFollowedBy whitespace
  (_, toks) <- withRaw $ many1 $
                  choice [ () <$ symbol '\\' >> anyTok
                         , whitespace >> lookAhead (noneOfToks [Symbol '$'])
                         , noneOfToks [Symbol '$']
                         ]
  symbol '$'
  inlineMath mathMethod (untokenize toks)

pDisplayMath :: (Monad m, HasMath a) => HTMLMathMethod -> InlineParser m a
pDisplayMath mathMethod = try $ do
  count 2 $ symbol '$'
  (_, toks) <- withRaw $ many1 $
                  choice [ () <$ symbol '\\' >> anyTok
                         , noneOfToks [Symbol '$']
                         ]
  count 2 $ symbol '$'
  displayMath mathMethod (untokenize toks)

pMathML :: Monad m => TeXMath.DisplayType -> Text -> InlineParser m (Html a)
pMathML dt tex =
  case TeXMath.readTeX tex of
    Left e     -> pure $ HtmlText e
    Right expr -> pure $ elToHtml (TeXMath.writeMathML dt expr)

elToHtml :: XML.Element -> Html a
elToHtml el =
  addAttributes attrs $
    -- HtmlConcat (htmlInline "p" (Just $ HtmlText (pack $ show el)))
    (htmlInline tag children)
  where
    tag      = pack . XML.qName . XML.elName $ el
    attrs    = fmap convertAttr (XML.elAttribs el)
    children = case XML.elContent el of
      []      -> Nothing
      nodes@_ -> Just $
        foldr HtmlConcat HtmlNull (mapMaybe contentToHtml nodes)
    contentToHtml c = case c of
      XML.Elem e -> Just $ elToHtml e
      XML.Text (cd) -> Just $ HtmlText (pack $ XML.cdData cd)
      _          -> Nothing
    convertAttr (XML.Attr k v) = (pack (XML.qName k), pack v)
