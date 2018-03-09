module Context where

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

class Doc a where
  h1   :: [a] -> a
  par  :: [a] -> a

class ItemExt a where
  item :: [a] -> a

class StyleExt a where
  bold :: Text -> a

instance (Doc a, Doc a') => Doc (a,a') where
  h1 = bimap h1 h1 . unzip
  par = bimap par par . unzip

instance (StyleExt a, StyleExt a') => StyleExt (a,a') where
  bold t = (bold t, bold t)

instance (ItemExt a, ItemExt a') => ItemExt (a,a') where
  item = bimap item item . unzip

-------
-- HTML

instance Doc Html where
  par  = wrapHtml "<par>" "</par>"
  h1   = wrapHtml "<h1>" "</h1>"

wrapHtml :: Text -> Text -> [Html] -> Html
wrapHtml left right content = Html $ left <> getHtml (mconcat content) <> right

instance StyleExt Html where
  bold t = Html $ "<b>" <> t <> "</b>"

instance ItemExt Html where
  item = wrapHtml "<item>" "</item>"
-----------
-- Markdown

instance Doc Markdown where
  par = wrapMarkdown "\n" "\n"
  h1 = wrapMarkdown "# " mempty

wrapMarkdown :: Text -> Text -> [Markdown] -> Markdown
wrapMarkdown left right content = Markdown $ left <> getMd (mconcat content) <> right

instance StyleExt Markdown where
  bold t = Markdown $ "*" <> t <> "*"

instance ItemExt Markdown where
  item = wrapMarkdown "Author: " mempty

-------------
-- Parser
-------------

type Open a = a -> a

type Parser = Parsec (ErrorFancy Void) Text

tag :: Text -> Parser a -> Parser a
tag name innerParser =
  string ("<" <> name <> ">") *> innerParser <* string ("</" <> name <> ">")

parseDoc :: Doc a => Open (Parser a)
parseDoc nextParser = try (fmap h1 (tag "h1" (some innerParser)))
                  <|> try (fmap par (tag "par" (some innerParser)))
  where
    innerParser = parseDoc nextParser <|> nextParser

parseItem :: ItemExt a => Open (Parser a)
parseItem nextParser = try (fmap item (tag "item" (some innerParser)))
  where
    innerParser = parseItem nextParser <|> nextParser

parseStyle :: StyleExt a => Open (Parser a)
parseStyle nextParser = try (bold . pack <$> tag "bold" (many (notChar '<')))
                    <|> nextParser

htmlParser = parseDoc <||> parseItem $ parseStyle (fail "All parsers failed")

-- TODO: Is this too ugly?
(<||>) :: Open (Parser a) -> Open (Parser a) -> Open (Parser a)
(<||>) p1 p2 p3 = p1 p3 <|> p2 p3

htmlToHtml :: Text -> Maybe Html
htmlToHtml = parseMaybe htmlParser

htmlToMarkdown :: Text -> Maybe (Markdown,Html)
htmlToMarkdown = parseMaybe htmlParser
