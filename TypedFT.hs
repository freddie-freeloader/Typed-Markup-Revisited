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


import Data.String

main = undefined

data InlineCtx = InlineCtx
data BlockCtx = BlockCtx

type Doc repr ctx = (IsString (repr ctx), Monoid (repr ctx))

class (Doc repr BlockCtx, Doc repr InlineCtx) => Base repr where
  paragraph  ::        [repr BlockCtx ] -> repr BlockCtx
  bulletList ::        [repr BlockCtx ] -> repr BlockCtx
  heading    :: Int -> [repr InlineCtx] -> repr BlockCtx

class Inline repr where
  str  :: String           -> repr InlineCtx
  emph :: [repr InlineCtx] -> repr InlineCtx

class MoreStyles repr where
  strong        :: [repr InlineCtx] -> repr InlineCtx
  strikethrough :: [repr InlineCtx] -> repr InlineCtx

newtype CommonMark ctx =
  CommonMark String
  deriving (Show)

instance Monoid (CommonMark ctx) where
  mappend (CommonMark l) (CommonMark r) = CommonMark $ l ++ r
  mempty = CommonMark ""

(<>) :: Monoid repr => repr -> repr -> repr
(<>) = mappend

instance IsString (CommonMark ctx) where
  fromString = CommonMark


instance Base CommonMark where
  paragraph = mconcat
  bulletList = addLineBreak . mconcat . map (mappend "\n- ")
  heading level = addLineBreak . f level

class InlineToBlock repr where
  changeCtx :: repr InlineCtx -> repr BlockCtx

f :: (Doc repr BlockCtx, Doc repr InlineCtx) => Int -> [repr InlineCtx] -> repr BlockCtx
f level = mappend (mconcat $ replicate level "#") . mconcat . g

g :: Functor repr => [repr InlineCtx] -> [repr BlockCtx]
g = map (fmap transform)
  where
    transform :: InlineCtx -> BlockCtx
    transform = const BlockCtx

addLineBreak :: Doc repr ctx => repr ctx -> repr ctx
addLineBreak text = text `mappend` "\n"

instance Inline CommonMark where
  str = CommonMark
  emph texts = "*" `mappend` mconcat texts `mappend` "*"

--groceryList :: (Base repr, Inline repr) => [repr]
groceryList
  = [ heading 1  ["Grocery list"]
    , bulletList [ paragraph ["1 Banana"]]
                 , paragraph ["2 ", emph ["fresh"], " Apples"] ]

badHeading = [ heading 1 (strong "foo") ]

