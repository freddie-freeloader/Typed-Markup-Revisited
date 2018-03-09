{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.String

main = undefined

class Inline a =>
      Doc a where
  paragraph :: [a] -> a
  bulletList :: [a] -> a
  heading :: Int -> [a] -> a

class Inline a where
  str :: String -> a
  emph :: [a] -> a


newtype CommonMark =
  CommonMark String

instance Monoid CommonMark where
  mappend (CommonMark l) (CommonMark r) = CommonMark $ l ++ r

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
  str = fromString
  emph texts = "*" `mappend` mconcat texts `mappend` "*"

groceryList :: Doc a => [a]
groceryList
  = [ heading 1  [str "Grocery list"
    , bulletList [ paragraph [str "1 Banana"]]
                 , paragraph [str "2 ", emph [str "fresh"], str " Apples"] ]]
