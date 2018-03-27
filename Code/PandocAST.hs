{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module PandocAST where

import           Data.String

main :: IO ()
main = undefined

data Block
  = Paragraph   [Inline] -- ^ Paragraph
  | BulletList  [Block]  -- ^ Bullet list (list of items, each a block)
  | Heading Int [Inline] -- ^ Heading - level (int) and text (inlines)

data Inline
  = Str String      -- ^ Text (string)
  | Emph   [Inline] -- ^ Emphasized text (list of inlines)
  | Strong [Inline] -- ^ Strongly emphasized text (list of inlines)

instance IsString Inline where
  fromString = Str

-------
-- Observations

newtype Markdown = Markdown String deriving (Monoid, IsString)
newtype LaTeX    = LaTeX    String deriving (Monoid, IsString)

-- Markdown observations

blockToCMark :: Block -> Markdown
blockToCMark (Paragraph text)     = mconcatMap inlineToCMark text
blockToCMark (BulletList docs)    = addLineBreak $ mconcatMap (mappend "- " . blockToCMark) docs
blockToCMark (Heading level text) = addLineBreak $ headingPrefix `mappend` mconcatMap inlineToCMark text
 where
  headingPrefix = mconcat $ replicate level "#"

addLineBreak :: Markdown -> Markdown
addLineBreak text = text `mappend` "\n"

inlineToCMark :: Inline -> Markdown
inlineToCMark (Str content)     = fromString content
inlineToCMark (Emph contents)   = "*" `mappend` mconcatMap inlineToCMark contents `mappend` "*"
inlineToCMark (Strong contents) = "**" `mappend` mconcatMap inlineToCMark contents `mappend` "**"

mconcatMap :: Monoid m => (a -> m) -> [a] -> m
mconcatMap f = mconcat . map f

-- LaTeX observations

blockToLaTeX :: Block -> LaTeX
blockToLaTeX = undefined

inlineToLaTeX :: Inline -> LaTeX
inlineToLaTeX = undefined

-------
-- Examples

groceryList :: [Block]
groceryList
  = [ Heading 1  [ Str "Grocery list"]
    , BulletList [ Paragraph [ Str "1 Banana"]
                 , Paragraph [ Str "2 "
                             , Emph [Str "organic"]
                             , Str " Apples"]]]

groceryListCM :: Markdown
groceryListCM = mconcatMap blockToCMark groceryList

groceryListLaTeX :: LaTeX
groceryListLaTeX = mconcatMap blockToLaTeX groceryList

groceryList' :: [Block]
groceryList'
  = [ Heading 1  [ "Grocery list"]
    , BulletList [ Paragraph [ "1 Banana"]
                 , Paragraph [ "2 " , Emph ["organic"] , " Apples"]]]

-- Malformed doc
-- badHeading = [ Heading 1  [ Heading 2 [ "Headingception!!"] ] ]
