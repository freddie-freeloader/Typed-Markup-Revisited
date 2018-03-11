{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleInstances #-}

-- SXML in Haskell, in the tagless-final style

-- This module defines text documents, which are monoids over strings
-- A marked-up document has operations to turn a string into
-- a document and to create the markup (elements, attributes).

module HSXML_doc ( StringMonoid(..)
                 , Url(..)
                 , urlToStr
                 , TextD(..)
                 , DOC_SEP(..)
                 , cdataSep
                 , TextDoc
                 , runTextDoc
                 , liftTextDoc
                 , MDoc(..)
                 , DocString
                 , docstring
                 , module Data.Monoid) where

import Data.Monoid
import System.IO (FilePath)

-- A monoid over strings
class Monoid m => StringMonoid m where
    emitLit :: String -> m
    emitUrl :: Url    -> m
    emitPreamble :: m

data Url
  = Url String       -- full Url (with the schema)
  | FileURL FilePath -- a file on the current host (relative Url)
  | FileURLA FilePath String -- FilePath + anchor in the file
  deriving (Show)

urlToStr :: Url -> String
urlToStr (Url x)        = x
urlToStr (FileURL x)    = x
urlToStr (FileURLA x y) = x ++ "#" ++ y


instance StringMonoid String where
    emitLit = id
    emitUrl = urlToStr
    emitPreamble = mempty

-- Marking of documents to help decide which separator,
-- if any, has to be inserted when two documents are
-- concatenated.
-- According to W3C serialization recommendations, we should
-- emit a space between adjacent PCDATA
-- We also insert a newline between adjancent block elements
data DOC_SEP
  = SEP_CDATA   -- treat the document as CDATA
                -- When combined with other CDATA, a separating space
                -- should be inserted
                -- Think of a string data
  | SEP_NEUTRAL -- the document is not CDATA, but it preseves the CDATA
                -- status of the documents it combines with.
                -- Think of an empty document
  | SEP_NONE    -- No separator before or after that document
                -- Think of newline
  | SEP_BLOCK   -- Block-like documents, to be separated with newlines.
                -- Think of paragraphs
  deriving Show

-- The class of documents with an implicit separator
class StringMonoid d => TextD d where
    set_cdata_sep :: DOC_SEP -> d -> d   -- add the separator marker

data TextDoc d = TextDoc DOC_SEP d

runTextDoc :: TextDoc d -> d
runTextDoc (TextDoc _ d) = d

liftTextDoc :: StringMonoid d => d -> TextDoc d
liftTextDoc = TextDoc SEP_NEUTRAL

instance StringMonoid d => Monoid (TextDoc d) where
    mempty = TextDoc SEP_NEUTRAL mempty
    mappend (TextDoc f1 d1) (TextDoc f2 d2) = TextDoc new_sep (d1 `mappend` sep `mappend` d2)
      where
        (sep, new_sep) = decide_sep f1 f2
        decide_sep SEP_NEUTRAL x = (mempty, x) -- empty documents, unit
        decide_sep x SEP_NEUTRAL = (mempty, x)
        decide_sep SEP_NONE x    = (mempty, x)
        decide_sep SEP_CDATA SEP_CDATA  = (emitLit " ", SEP_CDATA)
        decide_sep SEP_CDATA SEP_NONE   = (mempty, SEP_NONE)
        decide_sep SEP_CDATA SEP_BLOCK  = (mempty, SEP_BLOCK)
        decide_sep SEP_BLOCK SEP_CDATA  = (mempty, SEP_CDATA)
        decide_sep SEP_BLOCK SEP_NONE   = (mempty, SEP_NONE)
        decide_sep SEP_BLOCK SEP_BLOCK  = (emitLit "\n", SEP_BLOCK)


instance StringMonoid d => StringMonoid (TextDoc d) where
    emitLit = TextDoc SEP_NEUTRAL . emitLit
    emitUrl = TextDoc SEP_NEUTRAL . emitUrl
    emitPreamble = TextDoc SEP_NEUTRAL emitPreamble

instance StringMonoid d => TextD (TextDoc d) where
    set_cdata_sep sep (TextDoc _ d) = TextDoc sep d

-- Marked-up document

class TextD d => MDoc d where
    emit      :: String -> d  -- Emit the string with HTML or other encoding
    emit_elem :: String -> Maybe d -> Maybe d -> d
    emit_attr :: String -> d -> d

-- consider the document as CDATA
cdataSep :: MDoc d => d -> d
cdataSep = set_cdata_sep SEP_CDATA

data RenderHint = HintNl
  deriving (Eq)


-- A simple document, just for Show
type DocString = TextDoc String

docstring :: DocString -> String
docstring = runTextDoc

instance MDoc DocString where
    emit    = emitLit
    emit_elem name attr body = set_cdata_sep SEP_BLOCK $ mconcat
         [ emitLit name, maybe mempty do_attrs attr
         , emitLit "["
         , maybe mempty id body
         , emitLit "]"]
      where
        do_attrs d       = emitLit "{" `mappend` d `mappend` emitLit "}"
        emit_attr name d = set_cdata_sep SEP_NONE $
                           emitLit (" " ++ name ++ "='")
                           `mappend` d
                           `mappend` emitLit "'"
