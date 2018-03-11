{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module FirstFTEncoding where

import           Data.String
import CommonMark

main :: IO ()
main = undefined

-- Doc Attributes defined using ConstraintKinds
type DocConstraint doc = (Monoid doc, IsString doc)

newtype Doc doc = Doc doc

instance DocConstraint doc => -- Have to restrict for the use of 'mempty'
  Monoid (Doc doc) where
  mappend (Doc doc1) (Doc doc2) = Doc $ doc1 `mappend` doc2
  mempty = Doc mempty

-- Algebras

class Block a where
  paragraph  ::        [Doc a] -> Doc a
  bulletList ::        [Doc a] -> Doc a
  heading    :: Int -> [Doc a] -> Doc a

class DocConstraint a =>
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
  bulletList    = addLineBreak . mconcat . map (mappend "- ")
  heading level = addLineBreak . mappend headingPrefix . mconcat
   where
    headingPrefix = mconcat $ replicate level "#"

addLineBreak :: DocConstraint doc => doc -> doc
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
                 , paragraph ["2 ", emph ["organic"], " Apples"] ]

badHeading = [ heading 1  [ heading 2 [ "Headingception!!"] ] ]

