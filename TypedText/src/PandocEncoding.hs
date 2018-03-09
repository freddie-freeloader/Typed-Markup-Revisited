{-# LANGUAGE OverloadedStrings #-}

module PandocEncoding where

import           CommonMark
import           Data.String
import           Utils       (mconcatMap)

main = undefined

data Doc
  = Paragraph [Inline]   -- ^ Paragraph
  | BulletList [Doc]     -- ^ Bullet list (list of items, each a block)
  | Heading Int [Inline] -- ^ Heading - level (integer) and text (inlines)

data Inline
  = Str String      -- ^ Text (string)
  | EmDash          -- ^ em dash
  | Emph  [Inline]  -- ^ Emphasized text (list of inlines)
  | Strong [Inline] -- ^ Strongly emphasized text (list of inlines)

instance IsString Inline where
  fromString = Str

docToCMark :: Doc -> CommonMark
docToCMark (Paragraph text) = mconcatMap inlineToCMark text
docToCMark (BulletList docs) = mconcatMap (mappend "- " . docToCMark) docs
docToCMark (Heading level text) = headingPrefix `mappend` mconcatMap inlineToCMark text
  where
    headingPrefix = mconcat $ replicate level "#"

inlineToCMark :: Inline -> CommonMark
inlineToCMark (Str content) = fromString content
inlineToCMARK (Emph contents) = "*" `mappend` mconcatMap inlineToCMark contents `mappend` "*"
inlineToCMARK (Strong contents) = "**" `mappend` mconcatMap inlineToCMark contents `mappend` "**"
inlineToCMARK EmDash = "---"

groceryList :: [Doc]
groceryList
  = [ Heading 1  [Str "Grocery list"]
    , BulletList [ Paragraph [Str "1 Banana"]
                 , Paragraph [Str "2 ", Emph [Str "fresh"], Str " Apples"]]]

groceryListShort :: [Doc]
groceryListShort
  = [ Heading 1  ["Grocery list"]
    , BulletList [ Paragraph ["1 Banana"]
                 , Paragraph ["2 ", Emph ["fresh"], " Apples"] ]]


-- Malformed doc
-- badHeading = [ Heading 1 [Heading 2 ["foo"]] ]
