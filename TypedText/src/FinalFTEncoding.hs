{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module FinalFTEncoding where

import Data.String

main = undefined

-- Doc definitions

type DocAtts doc = (Monoid doc, IsString doc)

data DocWithCtx ctx doc where
  DocWithCtx :: DocAtts doc => doc -> DocWithCtx ctx doc
  -- record version with caveats mentioned below
  -- DocWithCtx :: DocAtts doc => { getDoc :: doc } -> DocWithCtx ctx doc

-- For some reason the restrictions on 'doc' get lost, even when defining
-- 'getDoc' with explicit restrictions
-- getDoc :: DocAtts doc => DocWithCtx ctx doc -> doc
-- getDoc (DocWithCtx doc) = doc

instance DocAtts doc => -- Have to restrict for the use of 'mempty'
  Monoid (DocWithCtx ctx doc) where
  mappend (DocWithCtx doc1) (DocWithCtx doc2) = DocWithCtx $ doc1 `mappend` doc2
  mempty = DocWithCtx mempty

-- Contexts

data InlineCtx
data BlockCtx

class FromInline ctx where
  fromInline :: DocAtts doc => DocWithCtx InlineCtx doc -> DocWithCtx ctx doc
  fromInline (DocWithCtx doc) = DocWithCtx doc

instance FromInline BlockCtx

class FromBlock ctx where
  fromBlock :: DocWithCtx BlockCtx doc -> DocWithCtx ctx doc
  fromBlock (DocWithCtx doc) = DocWithCtx doc

instance FromBlock InlineCtx

-- Algebras

class Block doc where
  paragraph  ::        [DocWithCtx InlineCtx doc] -> DocWithCtx BlockCtx doc
  bulletList ::        [DocWithCtx BlockCtx  doc] -> DocWithCtx BlockCtx doc
  heading    :: Int -> [DocWithCtx InlineCtx doc] -> DocWithCtx BlockCtx doc

class DocAtts doc => -- Restricting on 'Doc' so that we can use 'fromString'
  Inline doc where
  emDash ::           DocWithCtx InlineCtx doc
  str    :: String -> DocWithCtx InlineCtx doc
  str =
    DocWithCtx . fromString

instance (Inline doc) => IsString (DocWithCtx InlineCtx doc) where
  fromString = str

class Styles doc where
  emph   :: [DocWithCtx InlineCtx doc] -> DocWithCtx InlineCtx doc
  strong :: [DocWithCtx InlineCtx doc] -> DocWithCtx InlineCtx doc

{-- Make 'CommonMark' a 'Doc' -}

newtype CommonMark =
  CommonMark String
  deriving (Show)

instance Monoid CommonMark where
  mappend (CommonMark l) (CommonMark r) = CommonMark $ l ++ r
  mempty = CommonMark ""

instance IsString CommonMark where
  fromString = CommonMark

instance Block CommonMark where
  paragraph = fromInline . mconcat
  bulletList = addLineBreak . mconcat . map (mappend (fromInline "\n- "))
  heading level = addLineBreak . fromInline . mappend (mconcat $ replicate level "#") . mconcat

-- Should not have to write 'Doc doc'. Is this a compiler bug?
addLineBreak :: DocAtts doc => DocWithCtx ctx doc -> DocWithCtx ctx doc
addLineBreak (DocWithCtx doc) = DocWithCtx $ doc `mappend` "\n"

instance Inline CommonMark where
  emDash = "---"

instance Styles CommonMark where
  emph   texts = "*"  `mappend` mconcat texts `mappend` "*"
  strong texts = "**" `mappend` mconcat texts `mappend` "**"

-- groceryList :: (Block doc, Style doc, Inline doc) => [DocWithCtx BlockCtx doc]
groceryList
  = [ heading 1  ["Grocery list"]
    , bulletList [ paragraph ["1 Banana"]]
                 , paragraph ["2 ", emph ["fresh"], " Apples"] ]

forgetCtx :: DocWithCtx ctx doc -> doc
forgetCtx (DocWithCtx doc) = doc

renderedGroceryList :: CommonMark
renderedGroceryList = mconcat $ map forgetCtx groceryList

-- Malformed doc
-- badHeading = [ heading 1 [heading 2 ["foo"]] ]
