{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}


-- SXML in Haskell, in the tagless-final style
-- Please refer to Description.txt in this directory for explanations.
-- HTML 4.0 Reference:
-- http://www.w3.org/TR/html40/sgml/dtd.html

-- Principles
-- A document is a monoid (represented as a list or an IO action)
-- A marked-up document has operations to turn a string into
-- a document and to create the markup (elements, attributes).
-- A document is present in a context.
-- The user-extensible set of contexts includes at least the
-- standard HTML contexts: inline, block, attribute.
-- When a marked-up document is being created, the context
-- propagates outside in. The element determines the context
-- for its children.
-- Within a sequence (of inline data, of paragraphs within a section, etc)
-- the context stays the same.


-- This module exports a lot, for the benefit of HSXML_ext.
-- Further exporters should restrict the visibility.

module HSXML (
              -- contexts
              DC(..),
              CT_inline, CT_block, CT_attr, CT_battr, CT_doc, CT_root,
              -- two sample documents
              dc_empty, dc_newline,
              -- Context assignment functions
              as_block, as_inline,
              fromInline, inlineInline, blocklock, blockInline,
              -- Check predicates
              Check_ia, Check_iab,
              -- builders
              Build(..),
              -- rendering function for a piece of markup
              MarkupStr(..),
              -- run_root,

              -- -- various HTML-like builders
              -- mk_inline, code, h1, h2, h3, h4, h5, h6,
              -- nbsp, nosp, br, hr,
              -- li, ul, ol,
              -- dl, dt, dd, blockquote, blockcode,
              -- attr, align_center, href,
              -- tdiv, tspan,
              -- inlineE, em, strong, html_cite, html_q,
              -- html_sup, html_sub, font_size,
              -- p, tp, div,
              -- description, meta_tag,
              -- document, head, body
             ) where

import Prelude hiding (head, div)
import HSXML_doc
import GHC.Exts( IsString(..) )

-- A document, a string monoid, in a context
newtype DC ctx d = DC{unDC :: d}
   deriving (Monoid, MDoc, TextD, StringMonoid)

-- The contexts in this module (more can be added at any time)
data CT_inline    -- character content of an element
data CT_block     -- element content
data CT_attr      -- character content of an attribute
data CT_battr     -- attribute within an attr list
data CT_doc       -- document-level elements (such as head, root, etc)
data CT_root      -- the root element


-- Context assignment functions.
-- Because DC is a newtype, the following functions are operationally
-- the identity
as_block  :: DC CT_block d -> DC CT_block d
as_block = id
as_inline  :: DC CT_inline d -> DC CT_inline d
as_inline = id

dc_empty :: Monoid d => DC ct d
dc_empty = mempty

dc_newline :: TextD d => DC ct d
dc_newline = set_cdata_sep SEP_NONE (emitLit "\n")

-- Typeclasses that check that 'ctx' is in some set

class Check_iab name ctx
instance Check_iab name CT_inline
instance Check_iab name CT_attr
instance Check_iab name CT_block

class Check_ia name ctx
instance Check_ia name CT_inline
instance Check_ia name CT_attr

-- How to treat a string, and in which contexts
class MarkupStr ctx where
    cdata :: MDoc d => String -> DC ctx d
    cdata = cdataSep . emit

-- A string may appear only in inline and attr contexts
instance MarkupStr CT_inline
instance MarkupStr CT_attr

-- Overloaded string literals
instance MDoc d => IsString (DC CT_inline d) where
    fromString = cdataSep . emit
instance MDoc d => IsString (DC CT_attr d) where
    fromString = cdataSep . emit

-- Building the content

class Monoid input => Build input output output' | output' -> output where
    build :: (input -> output) -> input -> output'

-- The result document may be different from the children document input.
-- The children of an element may be in the context
-- different from the context of the element. In other words,
-- children may be processed differently from the
-- parent. RSS.hs shows several examples of that.
instance Monoid input => Build input (DC ctx dx) (DC ctx dx) where
    build tr = tr

-- instance (Build input output output', input' ~ input) => Build input output (input' -> output') where
--     build :: (input -> output) -> input -> (input' -> output')
--     build tr input = build tr . mappend input

-- Specialized instances
-- Build an element in a context whose children are inline
fromInline :: (Build (DC CT_inline d) (DC ctx d) output') =>
                (DC CT_inline d -> DC ctx d) ->
                 DC CT_inline d -> output'
fromInline = build

inlineInline (tr :: DC CT_inline d -> DC CT_inline d) = fromInline tr

-- Build an element in a block context whose children are inline
blockInline (tr :: DC CT_inline d -> DC CT_block d) = fromInline tr

-- Build an element in a block context whose children are block
blocklock :: (Build (DC CT_block d) (DC CT_block d) output') =>
                (DC CT_block d -> DC CT_block d) ->
                 DC CT_block d -> output'
blocklock = build


-- `Transparent' SPAN: a grouping of inline-level elements.
-- This is an administrative element, which is
-- equivalent to the sequence of inline-level elements in its content.
-- It is useful as a return value from a function, if a function needs
-- to return several inline-level elements.
-- The body of TSPAN has the content Inline, and the TSPAN element itself
-- is in the Inline context

-- -- An inline element with inline children
-- tspan x = inlineInline id mempty x

-- run_ds_inline :: DC CT_inline DocString -> String
-- run_ds_inline = docstring . unDC

-- run_ds :: DC CT_block DocString -> String
-- run_ds = docstring . unDC

-- tsp1 = run_ds_inline (tspan "str1" "str2" "str3")
-- -- "str1 str2 str3"

-- -- The first inline element: Code
-- -- it is an inline element, whose content is also inline
-- code x = inlineInline (mk_inline "code") mempty x

-- -- Used in Quote.hs
-- mk_inline :: MDoc d => String -> DC CT_inline d -> DC CT_inline d
-- mk_inline tag body = cdataSep $ emit_elem tag Nothing (Just body)

-- tcd1 = run_ds_inline (code "s1" "s2")
-- -- "code[s1 s2]"

-- tcd2 = run_ds_inline (tspan "str1" (code "code1" "code2") "str3")
-- -- "str1 code[code1 code2] str3"

-- -- `Transparent' DIV: grouping of block-level elements
-- -- This is an administrative element, which is
-- -- equivalent to the sequence of block-level elements in its content
-- -- It is useful as a return value from a function, if a function needs
-- -- to return several block-level elements.
-- -- The body of TDIV has the content Block, and the TDIV element itself
-- -- is in the Block context
-- tdiv x = blocklock id mempty x

-- -- The body of H1 has the content Inline, but the H1 element itself
-- -- is in the Block context
-- hn n = blockInline htag mempty
--  where
--  htag (DC body) = DC (emit_elem ("h"++show (n::Int)) Nothing (Just body))
--                    `mappend` dc_newline

-- h1 x = hn 1 x
-- h2 x = hn 2 x
-- h3 x = hn 3 x
-- h4 x = hn 4 x
-- h5 x = hn 5 x
-- h6 x = hn 6 x

-- th1 = run_ds (h1 "header is this" (code "yes"))
-- -- "h1[header is this code[yes]]\n"

-- test_tdiv = run_ds $ tdiv (p "1" "2") (p "1" "2") (p "1" "2")
-- -- Block element can't show up in inline context...
-- -- th1' = run_ds (h1 "header is this" (h1 "yes"))
-- {-
--     Couldn't match type `CT_inline' with `CT_block'
--     In the expression: h1 "yes"
--     In the expression: [h1 "yes"]
--     In the second argument of `h1', namely `[[h1 "yes"]]'
-- -}


-- -- Adding (many) more `tags', for the large subset of HTML/XHTML

-- -- non-breaking space
-- nbsp :: (Check_ia nosp ctx, MDoc d) => DC ctx d
-- nbsp = emitLit "&nbsp;"

-- -- When used among cdata, suppresses the separating space
-- nosp :: (Check_ia nosp ctx, MDoc d) => DC ctx d
-- nosp = set_cdata_sep SEP_NONE mempty

-- tsp2 = run_ds (h1 "header is this" nbsp (code "yes")
--                   "str1" nosp "str2" "str3" br "str4")
-- -- "h1[header is this&nbsp; code[yes] str1str2 str3\nbr[]\nstr4]\n"

-- -- A generic inline element with the inline content
-- -- The function inlineE should be restricted in other modules.
-- inlineE tag x = inlineInline (mk_inline tag) mempty x

-- -- EM, like Code, is an inline element with inline content
-- em x = inlineE "em" x

-- -- Ditto for the others
-- strong x    = inlineE "strong" x
-- html_cite x = inlineE "cite" x
-- html_q x    = inlineE "q" x

-- -- Ditto for subscripts and superscripts
-- html_sup x = inlineE "sup" x
-- html_sub x = inlineE "sub" x

-- -- Ditto for the font size change
-- font_size size x = inlineInline tr mempty x
--  where
--  tr body = cdataSep $
--             emit_elem "font" (Just $ emit_attr "size" (emitLit $ show size))
--               (Just body)

-- -- BR: both block and inline element
-- -- with rendering depending on the context
-- class MkBR ctx where
--     br :: MDoc d => DC ctx d

-- instance MkBR CT_inline where
--     br = dc_newline `mappend`
--                  emit_elem "br" Nothing Nothing `mappend` dc_newline

-- instance MkBR CT_attr where
--     br = dc_newline

-- instance MkBR CT_block where
--     br = emit_elem "br" Nothing Nothing


-- -- Block-level elements

-- -- A generic wrapper (no attributes)
-- wrap_tag tag (DC body) = DC $ emit_elem tag Nothing (Just body)

-- -- The body of P has the content Inline, but the P element itself
-- -- is in the Block context
-- p x = blockInline (wrap_tag "p") mempty x

-- -- The TP element is a `transparent' P. It is sort of an
-- -- administrative element. According to the HTML 4.0 DTD, an inline
-- -- element is also considered part of the `flow' (i.e., the block
-- -- content). We are imposing a bit of order: in order for inline
-- -- elements to be considered `block', they have to be wrapped
-- -- in a `tp' element (whose rendering is just the rendering of its body).
-- tp x = blockInline (\ (DC body) -> DC body) mempty x


-- -- HR: horizontal line. No extra attributes at present
-- hr :: MDoc d => DC CT_block d
-- hr = emit_elem "hr" Nothing Nothing


-- -- The body of LI has the content Inline, but the LI element itself
-- -- is in the Block context
-- li x = blockInline (wrap_tag "li") mempty  x


-- -- Generic list container: UL, OL, DL, ...

-- ul x = blocklock (wrap_tag "ul") mempty  x
-- ol x = blocklock (wrap_tag "ol") mempty  x
-- dl x = blocklock (wrap_tag "dl") mempty  x

-- tl1 = run_ds (ul (li "1" "2") (li "3" "4"))
-- -- "ul[li[1 2]\nli[3 4]]"

-- tl2 = run_ds $
--         ul (li "t1" (code "url1" "anchor" (code "code1") "a2"))
--            (li "t2")
--            (li  (code "code" "code2") "s2" "s3")
-- -- "ul[li[t1 code[url1 anchor code[code1] a2]]\nli[t2]\nli[code[code code2] s2 s3]]"

-- -- Definition lists
-- -- the body of a definition term is in the inline context
-- dt x = blockInline (wrap_tag "dt") mempty x

-- -- definition description is a block-level element with block context
-- dd x = blocklock (wrap_tag "dd") mempty x

-- -- Extended quotation
-- blockquote x = blocklock (wrap_tag "blockquote") mempty x


-- -- A collection of attributes
-- -- Like TP, it accumulates the content in the CT_battr context
-- -- and casts the result into the block context
-- attr :: (MDoc d, Build (DC CT_battr d) (DC CT_block d) output') =>
--         DC CT_battr d -> output'
-- attr x = build (\(DC attrs) -> DC attrs) mempty x

-- align_center :: MDoc d => DC CT_battr d
-- align_center = DC $ emit_attr "align" (emitLit "center")


-- -- Attributes
-- href :: MDoc d => Url -> DC CT_battr d
-- href url = DC $ emit_attr "href" (emitUrl url)

-- battr_attr :: (Build (DC CT_attr d) (DC CT_battr d) output') =>
--               (DC CT_attr d -> DC CT_battr d) ->
--               DC CT_attr d -> output'
-- battr_attr = build
-- description x = battr_attr tr mempty x
--  where tr (DC body) =
--         DC (emit_attr "name" (emitLit "description") `mappend`
--             emit_attr "content" body)


-- -- The META element, usually used in the HEAD
-- meta_tag :: (MDoc d, Build (DC CT_battr d) (DC CT_block d) output') =>
--             DC CT_battr d -> output'
-- meta_tag x = build (\(DC attrs) -> DC $ emit_elem "meta" (Just attrs) Nothing)
--                mempty x

-- tmeta = run_ds $
--         meta_tag (description "d1" "d2" br "d3")
--                  (href (Url "url"))

-- -- "meta{ name='description' content='d1 d2\nd3' href='url'}[]"

-- -- the following won't work: code cannot appear in attrs
-- {-
-- tmeta' = run_ds $
--          meta_tag (description "d1" "d2" br (code "d3"))
--                   (href (Url "url"))
--     Couldn't match type `CT_attr' with `CT_inline'
--     In the expression: code "d3"
--     In the expression: [code "d3"]
--     In the fourth argument of `description', namely `[[code "d3"]]'
-- -}



-- -- HTML-visible blocking of block-level elements.
-- -- It is useful to set various attributes/styles on the enclosed
-- -- elements
-- div (DC attrs::DC CT_block d) body = blocklock tr mempty body
--  where tr (DC body) =
--             DC $ emit_elem "div" (Just attrs) (Just body)

-- -- The following elements are in the `document' context
-- head :: (MDoc d, Build (DC CT_block d) (DC CT_doc d) output') =>
--         DC CT_block d -> output'
-- head x = build (\ (DC hd) -> DC $ emit_elem "head" Nothing (Just hd))
--           mempty x

-- body :: (MDoc d, Build (DC CT_block d) (DC CT_doc d) output') =>
--         DC CT_block d -> output'
-- body x = build tr mempty x
--  where
--  tr (DC body) =
--         DC $ emit_elem "body"
--                (Just (emit_attr "bgcolor" $ emitLit "#FFFFFF"))
--                (Just body)

-- -- the root element
-- document :: MDoc d => DC CT_doc d -> DC CT_doc d -> DC CT_root d
-- document (DC h) (DC b) = DC $
--     emitPreamble `mappend`
--      emit_elem "html" Nothing (Just (h `mappend` b)) `mappend`
--      (unDC dc_newline)

-- run_root :: DC CT_root d -> d
-- run_root = unDC

-- -- see sample1C.hs for a larger example

-- -- blockcode -- the block of code (such an element is part of the XHTML 2.0
-- -- proposal). It is a block-level element with an inline-level content,
-- -- which is actually a sequence of lines.
-- -- We define a new context in which strings are interpreted as
-- -- indented lines

-- data CT_blockcode
-- instance MarkupStr CT_blockcode where
--     cdata x = emitLit "     " `mappend` emit x `mappend` dc_newline

-- block_blockcode :: (Build (DC CT_blockcode d) (DC CT_block d) output') =>
--                    (DC CT_blockcode d -> DC CT_block d) ->
--                    DC CT_blockcode d -> output'
-- block_blockcode = build

-- blockcode x = block_blockcode
--                 (\(DC body) -> DC $ emit_elem "pre" Nothing (Just body))
--                 mempty x
