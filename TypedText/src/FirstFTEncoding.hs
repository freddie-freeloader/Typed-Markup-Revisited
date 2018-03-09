{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module FirstFTEncoding where

import           Data.String
import CommonMark

main = undefined

-- Doc Attributes defined using ConstraintKinds
type DocAtts doc = (Monoid doc, IsString doc)

data Doc doc where
  Doc :: DocAtts doc => doc -> Doc doc

instance DocAtts doc => -- Have to restrict for the use of 'mempty'
  Monoid (Doc doc) where
  mappend (Doc doc1) (Doc doc2) = Doc $ doc1 `mappend` doc2
  mempty = Doc mempty

-- Algebras

class Block a where
  paragraph  ::        [Doc a] -> Doc a
  bulletList ::        [Doc a] -> Doc a
  heading    :: Int -> [Doc a] -> Doc a

class DocAtts a =>
  Inline a where
  emDash ::           Doc a
  str    :: String -> Doc a
  str = Doc . fromString

class Inline a => Styles a where
  emph   :: [Doc a] -> Doc a
  strong :: [Doc a] -> Doc a

instance (Inline doc) => IsString (Doc doc) where
  fromString = str

{-- Make 'CommonMark' a 'Doc' -}

instance Block CommonMark where
  paragraph     = mconcat
  bulletList    = addLineBreak . mconcat . map (mappend "\n- ")
  heading level = addLineBreak . mappend (mconcat $ replicate level "#") . mconcat

addLineBreak :: DocAtts doc => doc -> doc
addLineBreak text = text `mappend` "\n"

instance Inline CommonMark where
  emDash = "---"

instance Styles CommonMark where
  emph   texts = "*"  `mappend` mconcat texts `mappend` "*"
  strong texts = "**" `mappend` mconcat texts `mappend` "**"

--groceryList :: (Block a, Inline a) => [a]
groceryList
  = [ heading 1  ["Grocery list"]
    , bulletList [ paragraph ["1 Banana"]]
                 , paragraph ["2 ", emph ["fresh"], " Apples"] ]

badHeading = [ heading 1  [ heading 2 [ "Headingception!!"] ] ]

