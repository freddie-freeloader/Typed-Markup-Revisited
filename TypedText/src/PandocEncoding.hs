{-# LANGUAGE OverloadedStrings #-}

module PandocEncoding where

import           CommonMark
import           Data.String
import           Utils       (mconcatMap)

main = undefined

data Block
  = Paragraph   [Inline] -- ^ Paragraph
  | BulletList  [Block]  -- ^ Bullet list (list of items, each a block)
  | Heading Int [Inline] -- ^ Heading - level (integer) and text (inlines)

data Inline
  = Str String      -- ^ Text (string)
  | EmDash          -- ^ em dash
  | Emph   [Inline] -- ^ Emphasized text (list of inlines)
  | Strong [Inline] -- ^ Strongly emphasized text (list of inlines)

instance IsString Inline where
  fromString = Str

-- CommonMark

docToCMark :: Block -> CommonMark
docToCMark (Paragraph text)     = mconcatMap inlineToCMark text
docToCMark (BulletList docs)    = addLineBreak $ mconcatMap (mappend "- " . docToCMark) docs
docToCMark (Heading level text) = addLineBreak $ headingPrefix `mappend` mconcatMap inlineToCMark text
 where
  headingPrefix = mconcat $ replicate level "#"

addLineBreak :: CommonMark -> CommonMark
addLineBreak text = text `mappend` "\n"

inlineToCMark :: Inline -> CommonMark
inlineToCMark (Str content)     = fromString content
inlineToCMARK (Emph contents)   = "*" `mappend` mconcatMap inlineToCMark contents `mappend` "*"
inlineToCMARK (Strong contents) = "**" `mappend` mconcatMap inlineToCMark contents `mappend` "**"
inlineToCMARK EmDash            = "---"

-- LaTeX

type LaTeX = String

docToLaTeX :: Block -> LaTeX
docToLaTeX = undefined

inlineToLaTeX :: Inline -> LaTeX
inlineToLaTeX = undefined

-- Examples

groceryList :: [Block]
groceryList
  = [ Heading 1  [Str "Grocery list"]
    , BulletList [ Paragraph [Str "1 Banana"]
                 , Paragraph [Str "2 ", Emph [Str "organic"], Str " Apples"]]]

groceryListCM :: CommonMark
groceryListCM = mconcatMap docToCMark groceryList

groceryListLaTeX :: LaTeX
groceryListLaTeX = mconcatMap docToLaTeX groceryList

groceryListShort :: [Block]
groceryListShort
  = [ Heading 1  ["Grocery list"]
    , BulletList [ Paragraph ["1 Banana"]
                 , Paragraph ["2 ", Emph ["organic"], " Apples"] ]]


-- Malformed doc
-- badHeading = [ Heading 1 [Heading 2 ["foo"]] ]
