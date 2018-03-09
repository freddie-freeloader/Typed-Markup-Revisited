module Typed where

import Data.Text (Text,append,pack)
import Data.Maybe (isNothing)
import Data.Bifunctor (bimap)
import Data.Function (fix)
import Data.Maybe (catMaybes)
import Data.Either (rights)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (join)


(<>) :: Text -> Text -> Text
(<>) = append

newtype Html = Html { getHtml :: Text }
  deriving (Show)

instance Monoid Html where
  mappend a b = Html $ getHtml a <> getHtml b
  mempty = Html ""

instance Monoid Markdown where
  mappend a b = Markdown $ getMd a <> getMd b
  mempty = Markdown ""


newtype Markdown = Markdown { getMd :: Text }
  deriving (Show)

class Monoid a => Doc a where
  h1 :: [a] -> a
  item :: [a] -> a
  par :: [a] -> a

class StyleExt a where
  bold :: Text -> a

instance (Doc a, Doc a') => Doc (a,a') where
  h1 = bimap h1 h1 . unzip
  item = bimap item item . unzip
  par = bimap par par . unzip

instance (StyleExt a, StyleExt a') => StyleExt (a,a') where
  bold t = (bold t, bold t)

-------
-- HTML

instance Doc Html where
  par  = wrapHtml "<par>" "</par>"
  item = wrapHtml "<item>" "</item>"
  h1   = wrapHtml "<h1>" "</h1>"

wrapHtml :: Text -> Text -> [Html] -> Html
wrapHtml left right content = Html $ left <> getHtml (mconcat content) <> right

instance StyleExt Html where
  bold t = Html $ "<b>" <> t <> "</b>"

-----------
-- Markdown

instance Doc Markdown where
  par = wrapMarkdown "\n" "\n"
  item = wrapMarkdown "Author: " mempty
  h1 = wrapMarkdown "# " mempty

wrapMarkdown :: Text -> Text -> [Markdown] -> Markdown
wrapMarkdown left right content = Markdown $ left <> getMd (mconcat content) <> right

instance StyleExt Markdown where
  bold t = Markdown $ "*" <> t <> "*"


-------------
-- Parser
-------------

type Open a = a -> a

type Parser = Parsec (ErrorFancy Void) Text

tag :: Text -> Parser a -> Parser a
tag name innerParser =
  string ("<" <> name <> ">") *> innerParser <* string ("</" <> name <> ">")

parseDoc :: (Show a, Doc a) => Open (Parser a)
parseDoc nextParser = try (fmap h1 (tag "h1" (some innerParser)))
                  <|> try (fmap item (tag "item" (some innerParser)))
                  <|> try (fmap par (tag "par" (some innerParser)))
  where
    innerParser = parseDoc nextParser <|> nextParser

parseStyle :: (Show a, StyleExt a) => Open (Parser a)
parseStyle nextParser = try (bold . pack <$> tag "bold" (many (notChar '<')))
                    <|> nextParser

htmlParser = parseDoc $ parseStyle (fail "All parsers failed")

htmlToHtml :: Text -> Maybe Html
htmlToHtml = parseMaybe htmlParser

htmlToMarkdown :: Text -> Maybe (Markdown,Html)
htmlToMarkdown = parseMaybe htmlParser
