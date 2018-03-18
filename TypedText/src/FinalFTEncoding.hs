{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module FinalFTEncoding ( DocAtts
                       , DocWithCtx(..)
                       , InlineCtx
                       , BlockCtx
                       , FromBlock (..)
                       , FromInline (..)
                       , Block (..)
                       , Inline (..)
                       , Styles (..)
                       , CommonMark (..)
                       , renderCommonMark
                       ) where

import           CommonMark
import           Data.String

main :: IO ()
main = undefined

-- Doc definitions

type DocAtts doc = (Monoid doc, IsString doc)

newtype DocWithCtx ctx doc = DocWithCtx doc

  -- Haskell does nothing useful with this restriction...
  -- docwithctx :: docatts doc => doc -> docwithctx ctx doc
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

-- Context definitions

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

----------------------------
-- Make 'CommonMark' instance of algebras
--

instance Block CommonMark where
  paragraph = fromInline . mconcat
  bulletList = addLineBreak . mconcat . map (mappend (fromInline "\n- "))
  heading level = addLineBreak . fromInline . mappend (mconcat $ replicate level "#") . mconcat

addLineBreak :: DocAtts doc => DocWithCtx ctx doc -> DocWithCtx ctx doc
addLineBreak (DocWithCtx doc) = DocWithCtx $ doc `mappend` "\n"

instance Inline CommonMark where
  emDash = "---"

instance Styles CommonMark where
  emph   texts = "*"  `mappend` mconcat texts `mappend` "*"
  strong texts = "**" `mappend` mconcat texts `mappend` "**"

------------
-- Examples
--

-- groceryList :: (Block doc, Style doc, Inline doc) => [DocWithCtx BlockCtx doc]
groceryList
  = [ heading 1  ["Grocery list"]
    , bulletList [ paragraph ["1 Banana"]]
                 , paragraph ["2 ", emph ["organic"], " Apples"] ]

forgetCtx :: DocWithCtx ctx doc -> doc
forgetCtx (DocWithCtx doc) = doc

renderCommonMark :: DocWithCtx ctx CommonMark -> CommonMark
renderCommonMark = forgetCtx

renderedGroceryList :: CommonMark
renderedGroceryList = mconcat $ map renderCommonMark groceryList

-- Malformed doc
-- badHeading = [ heading 1 [heading 2 ["foo"]] ]
