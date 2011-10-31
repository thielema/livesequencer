module Utility where

-- | for compatibility with GHC-6.12, is in Control.Monad since GHC-7
void :: Functor f => f a -> f ()
void = fmap (const ())
