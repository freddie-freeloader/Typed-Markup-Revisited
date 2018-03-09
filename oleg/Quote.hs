{-# LANGUAGE TemplateHaskell #-}

-- Quotations for multi-line strings that may contain inline
-- markup.
-- We also support the quotations for code blocks.
--
-- A plain-text quotation is a multi-line string.
-- It may contain the following markup:
--  |str|  -- encoding of the [[code "str"]] element
--  _str_  -- encoding of the [[em "str"]] element
-- The markup is only recognized if the opening delimiter is preceded
-- by the white space or the beginning of the string
-- The character ~ is regarded as whitespace, of zero width.
-- It is useful when we wish to mark-up a part of the word:
--   |mark|ed
-- will be rendered with a space between `mark' and `ed', but
--   |mark|~ed
-- will be rendered as desired. The character is necessary
-- when we mark-up a suffix of a word: in
--   un_marked_
-- the _ markup will not be recognized since the preceding character
-- is not a white space. In contrast,
--   un~_marked_
-- will be emphasized as desired.

-- The quotation can be used as:
-- [$mup_text|<mup_text>|]
--
-- A code-block quotation
-- [$mup_code|
--   line1
--   line2 |]
-- generates a blockcode HSXML element. We skip the first empty line,
-- if any. We remove the indentation, letting us indent the quoted code.
-- We determine the indentation from the first non-empty line of the
-- quoted code. We do not change the code at all, so that we
-- paste the real code with no changes other than the indentation.

module Quote (mup_text, mup_code) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Control.Monad.Error
import Data.List (stripPrefix)
import Data.Monoid

import HSXML (mk_inline, nosp, cdata, blockcode)

type SourceName = String
type Line       = Int
type Column     = Int

-- First, we parse the mup_text's content into the list of PCdata items
-- of the following form.

data PCdata  =  PCStr String            -- plain string, perhaps multiline
             |  PCMark MarkupEL String  -- marked-up string
             |  PCNosp                  -- zero-width space
--             |  PCAnti ExpQ
    deriving Show

data MarkupEL = MK_code | MK_em
    deriving Show

-- We use zipper, shifting the characters from the input string into
-- the accumulator string, where they accumulate, in reverse order.
-- We write a finite state machine
mup_parse :: Monad m =>
             SourceName -> (Line,Column) -> String ->
             m [PCdata]
mup_parse sn loc input = fsm loc [] "" input
 where
 fsm loc acc stash "" =
     return . reverse $ stash_away stash acc
 fsm loc acc stash ('\n':inp) =
     fsm (nline loc) acc ('\n':stash) inp
 fsm loc acc stash (c:inp)
     | c `elem` "|_" && (stash == "" || head stash `elem` " \t\n\r") =
         fmarkup c loc (nchar loc)
           (stash_away (if stash == "" then "" else tail stash) acc) "" inp
 fsm loc acc stash ('~':inp) =
     fsm (nchar loc) (PCNosp : stash_away stash acc) "" inp
 fsm loc acc stash (c:inp) = fsm (nchar loc) acc (c:stash) inp

 fmarkup sc sloc loc acc stash "" =
     fail $ unwords ["Fail to find the closing delimiter", show sc,
                     "for the markup", show sloc,"-",show loc]
 fmarkup sc sloc loc acc stash ('\n':inp) =
     fmarkup sc sloc (nline loc) acc ('\n':stash) inp
 fmarkup sc sloc loc acc stash (c:inp) | c == sc =
     fsm (nchar loc) (PCNosp:mark_it c stash : acc) "" inp
 fmarkup sc sloc loc acc stash (c:inp) =
     fmarkup sc sloc (nchar loc) acc (c:stash) inp

 stash_away "" acc    = acc
 stash_away stash acc = PCStr (reverse stash) : acc

 mark_it '|' stash = PCMark MK_code $ reverse stash
 mark_it '_' stash = PCMark MK_em   $ reverse stash

nchar, nline :: (Line,Column) -> (Line,Column)
nchar (line,col) = (line,col+1)
nline (line,col) = (line+1,0)

-- Compile [PCdata] to the inline text (tspan or a single string)
-- for HSXML

-- monomorphizing mappend before using it repeatedly;
-- taking advantage that let in GHC is now monomorphic. This
-- does seem to speed up the type checking a bit.
mup_compile :: [PCdata] -> ExpQ
mup_compile []  = fail "Empty mup_text quote"
mup_compile [x] = pcdata_to_expq x
mup_compile lst = [e| let mapp = mappend in
                      $(foldl1 (\x y -> [e| mapp $x $y|])
                         (map pcdata_to_expq lst)) |]

pcdata_to_expq :: PCdata -> ExpQ
pcdata_to_expq (PCStr str)          = str_to_expq str
pcdata_to_expq (PCMark MK_code str) = [e| mk_inline "code" $(str_to_expq str) |]
pcdata_to_expq (PCMark MK_em str)   = [e| mk_inline "em"   $(str_to_expq str) |]
pcdata_to_expq (PCNosp)             = [e| nosp |]

str_to_expq :: String -> ExpQ
str_to_expq str = [e| cdata $(stringE str) |]

mup_text =
  QuasiQuoter {
   quoteExp  = qq_wrapper $ \sn pos str -> mup_parse sn pos str >>= mup_compile,
   quotePat  = error "pcdata cannot be used as a pattern",
   quoteType = error "pcdata cannot be used as a type",
   quoteDec  = error "pcdata cannot be used as a Decl"}

qq_wrapper :: (SourceName -> (Line,Column) -> String -> ExpQ) -> String -> ExpQ
qq_wrapper qf str = do
  loc <- location
  let sn  = loc_filename loc
      pos = loc_start loc
  qf sn pos str

mup_code =
  QuasiQuoter {
   quoteExp  = qq_wrapper $ \sn pos str -> bc_parse sn pos str >>= bc_compile,
   quotePat  = error "mup_code cannot be used as a pattern",
   quoteType = error "mup_code cannot be used as a type",
   quoteDec  = error "mup_code cannot be used as a Decl" }

bc_parse :: Monad m =>
            SourceName -> (Line,Column) -> String -> m [String]
bc_parse sn pos str = p1 pos (lines str)
 where
 -- remove the leading empty string, if any
 p1 pos ("":lines) = p2 (nline pos) lines
 p1 pos lines      = p2 pos lines

 -- determine the indent
 p2 pos [] = return []
 p2 pos (line:lines) =
   let (indent,line') = span (== ' ') line
   in p3 pos indent (line':lines)

 p3 pos indent lines = return $
   map (\l -> maybe l id $ stripPrefix indent l) lines

-- We use local let to monomorphize mappend
bc_compile :: [String] -> ExpQ
bc_compile []   = fail "Empty mup_code quote"
bc_compile [x]  = [e| $(varE 'blockcode) $(str_to_expq x)|]
bc_compile lines = [e|let mapp = mappend in $bc
 $(foldl1 (\x y -> [e| mapp $x $y|])
          (map str_to_expq lines))|]
 where bc = varE 'blockcode
