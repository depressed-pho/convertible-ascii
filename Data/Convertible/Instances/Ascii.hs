{-# LANGUAGE
    FlexibleContexts
  , MultiParamTypeClasses
  , UnicodeSyntax
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |Instances to convert amongst 'Ascii' strings and other things.
--
-- Note that data types are re-exported only for the documentation
-- purpose. They'll disappear as soon as Haddock's issue #54
-- (<http://trac.haskell.org/haddock/ticket/54>) gets resolved. So you
-- should always import this module as:
--
-- > import Data.Convertible.Instances.Ascii ()
module Data.Convertible.Instances.Ascii
    ( Ascii
    , CIAscii
    , AsciiBuilder
    ) where
import Control.Failure
import Data.Ascii (Ascii, CIAscii, AsciiBuilder)
import qualified Data.Ascii as A
import Data.ByteString (ByteString)
import Data.Convertible.Base
import Data.Convertible.Utils
import Prelude.Unicode

instance ConvertAttempt ByteString Ascii where
    {-# INLINE convertAttempt #-}
    convertAttempt bs
        = case A.fromByteString bs of
            Just a  → return a
            Nothing → nonAscii bs

nonAscii ∷ Failure (ConvertBoundsException Char α) f ⇒ α → f β
{-# INLINE nonAscii #-}
nonAscii = failure ∘ ConvertBoundsException '\0' '\x7F'
