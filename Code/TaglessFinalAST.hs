{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module TaglessFinal where

import           Data.String

--------
-- Doc definition

newtype Doc doc = Doc doc

-- DocConstraint defined using ConstraintKinds
type DocConstraint doc = (Monoid doc, IsString doc)

instance DocConstraint doc => -- Have to restrict for the use of 'mempty'
  Monoid (Doc doc) where
  mappend (Doc doc1) (Doc doc2) = Doc (doc1 `mappend` doc2)
  mempty = Doc mempty

instance IsString doc =>
  IsString (Doc doc) where
  fromString = Doc . fromString

-------
-- Algebra specification

class Block a where
  paragraph  ::        [Doc a] -> Doc a
  bulletList ::        [Doc a] -> Doc a
  heading    :: Int -> [Doc a] -> Doc a

class DocConstraint a =>
  Inline a where
  str    :: String -> Doc a
  str = Doc . fromString

class Styles a where
  emph   :: [Doc a] -> Doc a
  strong :: [Doc a] -> Doc a

-------
-- Observations

newtype Markdown = Markdown String deriving (Monoid, IsString)
newtype LaTeX    = LaTeX    String deriving (Monoid, IsString)

-- Markdown observations

instance Block Markdown where
  paragraph = mconcat
  bulletList = undefined
  heading level = undefined

instance Inline Markdown

instance Styles Markdown where
  emph   texts = "*"  `mappend` mconcat texts `mappend` "*"
  strong texts = undefined

-- LaTeX observations

instance Block LaTeX where

instance Inline LaTeX where

instance Styles LaTeX where

-------
-- Examples

stylishNote :: (Inline a, Styles a) => Doc a
stylishNote = strong ["Green Tea keeps me awake"]
