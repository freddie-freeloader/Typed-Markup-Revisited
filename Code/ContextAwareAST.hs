{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module ContextAwareAST where

import           Data.String

--------
-- Doc definition

newtype DocWithCtx ctx doc = DocWithCtx doc

-- DocConstraint defined using ConstraintKinds
type DocConstraint doc = (Monoid doc, IsString doc)

instance DocConstraint doc => -- Have to restrict for the use of 'mempty'
  Monoid (DocWithCtx ctx doc) where
  mappend (DocWithCtx doc1) (DocWithCtx doc2) = DocWithCtx (doc1 `mappend` doc2)
  mempty = DocWithCtx mempty

instance IsString doc =>
  IsString (DocWithCtx ctx doc) where
  fromString = DocWithCtx . fromString

--------
-- Context definitions

data InlineCtx
data BlockCtx

class FromInline ctx where
  fromInline :: DocWithCtx InlineCtx doc -> DocWithCtx ctx doc
  fromInline (DocWithCtx doc) = DocWithCtx doc

instance FromInline BlockCtx

--------
-- Algebra specification


class Block doc where
  paragraph  ::        [DocWithCtx InlineCtx doc] -> DocWithCtx BlockCtx doc
  bulletList ::        [DocWithCtx BlockCtx  doc] -> DocWithCtx BlockCtx doc
  heading    :: Int -> [DocWithCtx InlineCtx doc] -> DocWithCtx BlockCtx doc

class DocConstraint doc => -- Restricting on 'Doc' so that we can use 'fromString'
  Inline doc where
  str    :: String -> DocWithCtx InlineCtx doc
  str = DocWithCtx . fromString

class Styles doc where
  emph   :: [DocWithCtx InlineCtx doc] -> DocWithCtx InlineCtx doc
  strong :: [DocWithCtx InlineCtx doc] -> DocWithCtx InlineCtx doc

-------
-- Observations

newtype Markdown = Markdown String deriving (Monoid, IsString)
newtype LaTeX    = LaTeX    String deriving (Monoid, IsString)

-- Markdown observations

instance Block Markdown where
  paragraph = fromInline . mconcat
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

stylishNote :: (Inline a, Styles a) => DocWithCtx InlineCtx a
stylishNote = strong ["Green Tea keeps me awake"]
