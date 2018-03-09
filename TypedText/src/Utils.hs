module Utils (mconcatMap) where

mconcatMap :: Monoid m => (a -> m) -> [a] -> m
mconcatMap f = mconcat . map f
