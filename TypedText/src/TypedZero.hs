module TypedZero () where

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
  h1 :: a -> a
  item :: a -> a
  par :: a -> a

class StyleExt a where
  bold :: Text -> a

instance (Doc a, Doc a') => Doc (a,a') where
  h1 = bimap h1 h1
  item = bimap item item
  par = bimap par par

instance (StyleExt a, StyleExt a') => StyleExt (a,a') where
  bold t = (bold t, bold t)

instance Doc Html where
  par content = Html $ "<par>" <> getHtml content <> "</par>"
  item text = Html $ "<item>" <> getHtml text <> "</item>"
  h1 content = Html $ "<h1>" <> getHtml content <> "</h1>"

instance StyleExt Html where
  bold t = Html $ "<b>" <> t <> "</b>"

instance Doc Markdown where
  par content = Markdown $ "\n" <> getMd content <> "\n"
  item text = Markdown $ "Author: " <> getMd text
  h1 content = Markdown $ "# " <> getMd content

instance StyleExt Markdown where
  bold t = Markdown $ "*" <> t <> "*"

data Tree a
  = Node a [Tree a]
  | Leaf a

type JsonTree = Tree Text

type Open a = a -> a

-------------
-- Parser
-------------

type Parser = Parsec (ErrorFancy Void) Text

tag :: Text -> Parser a -> Parser a
tag name innerParser =
  string ("<" <> name <> ">") *> innerParser <* string ("</" <> name <> ">")

parseDoc :: (Show a, Doc a) => Open (Parser a)
parseDoc nextParser = try (fmap h1 (tag "h1" nextParser))
                  <|> try (fmap item (tag "item" nextParser))
                  <|> try (fmap par (tag "par" nextParser))

parseStyle :: (Show a, StyleExt a) => Open (Parser a)
parseStyle nextParser = try (bold . pack <$> tag "bold" (many (notChar '<')))
                    <|> nextParser

htmlParser = parseDoc $ parseStyle (fail "All parsers failed")

htmlToHtml :: Text -> Maybe Html
htmlToHtml = parseMaybe htmlParser

htmlToMarkdown :: Text -> Maybe (Markdown,Html)
htmlToMarkdown = parseMaybe htmlParser
-------------
-- Tree stuff
-------------

type NodeAlg a = (Text -> [Maybe (Either Text a)] -> Maybe (Either Text a))

decodeDoc :: Doc a => Open (NodeAlg a)
decodeDoc _ "item" = Just . Right . item . foldl mappend mempty . rights . catMaybes
decodeDoc _ "h1" = Just . Right . h1 . foldl mappend mempty . rights . catMaybes
decodeDoc _ "par" = Just . Right . par . foldl mappend mempty . rights . catMaybes
decodeDoc decoder unknown = decoder unknown

decodeStyleExt :: StyleExt a => Open (NodeAlg a)
decodeStyleExt _ "bold" [Just (Left text)] = pure . pure $ bold text
decodeStyleExt decoder unknown subtree = decoder unknown subtree

decodeFailure :: NodeAlg a
decodeFailure _ _ = Nothing

unifiedNodeAlg = unify [decodeDoc, decodeStyleExt] decodeFailure

leafAlg :: Text -> Maybe (Either Text a)
leafAlg text = Just (Left text)

unify :: [Open a] -> a -> a
unify xs finally = foldr ($) finally xs

-- createBoth :: JsonTree -> Maybe (Markdown,Html)
-- createBoth t = createDuplicate <$> maybeRes
--   where
--     maybeRes = unify t

createDuplicate :: (a,b) -> (a,b)
createDuplicate = id
