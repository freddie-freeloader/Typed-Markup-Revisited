module CommonMark where

import Data.String

instance IsString CommonMark where
  fromString = CommonMark

instance Monoid CommonMark where
  mappend (CommonMark l) (CommonMark r) = CommonMark $ l ++ r
  mempty = CommonMark ""

newtype CommonMark = CommonMark String
