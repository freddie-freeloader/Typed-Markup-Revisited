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


import Data.String

main = undefined

data InlineCtx
data BlockCtx

class WithCtx ctx a

type CtxIO ctxI input ctxO out a = ( WithCtx ctxI input
                                   , WithCtx ctxO out
                                   , input ~ a
                                   , out ~ a
                                   , ctxI ~ ctxO)
type BlockOut  ctxIn input out a = CtxIO ctxIn input BlockCtx out a
type InlineOut ctxIn input out a = CtxIO ctxIn input InlineCtx out a

class Doc a where
  paragraph  :: BlockOut InlineCtx input out a =>        [input] -> out
  bulletList :: BlockOut BlockCtx  input out a =>        [input] -> out
  heading    :: BlockOut InlineCtx input out a => Int -> [input] -> out

class Inline a where
  str  :: (WithCtx InlineCtx out, out ~ a) => String -> out
  emph :: InlineOut InlineCtx input out a => [input] -> out

class IsString a => MoreStyles a where
  strong :: [a] -> a
  strikethrough :: [a] -> a

newtype CommonMark =
  CommonMark String
  deriving (Show)

instance Monoid CommonMark where
  mappend (CommonMark l) (CommonMark r) = CommonMark $ l ++ r
  mempty = CommonMark ""

(<>) :: Monoid a => a -> a -> a
(<>) = mappend

instance IsString CommonMark where
  fromString = CommonMark


instance Doc CommonMark where
  paragraph = mconcat
  bulletList = addLineBreak . mconcat . map (mappend "\n- ")
  heading level = addLineBreak . mappend (mconcat $ replicate level "#") . mconcat

addLineBreak :: (IsString a, Monoid a) => a -> a
addLineBreak text = text `mappend` "\n"

instance Inline CommonMark where
  str = CommonMark
  emph texts = "*" `mappend` mconcat texts `mappend` "*"

--groceryList :: (Doc a, Inline a) => [a]
groceryList
  = [ heading 1  ["Grocery list"]
    , bulletList [ paragraph ["1 Banana"]]
                 , paragraph ["2 ", emph ["fresh"], " Apples"] ]

badHeading = [ heading 1 (strong "foo") ]

