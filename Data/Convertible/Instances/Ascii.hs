{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , TemplateHaskell
  , TypeSynonymInstances
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
import Blaze.ByteString.Builder (Builder)
import Control.Failure
import Data.Ascii (Ascii, CIAscii, AsciiBuilder)
import qualified Data.Ascii as A
import Data.ByteString (ByteString)
import Data.Convertible.Base
import Data.Convertible.Utils
import Data.Text (Text)
import Prelude.Unicode

instance ConvertSuccess Ascii AsciiBuilder where
    {-# INLINE convertSuccess #-}
    convertSuccess = A.toAsciiBuilder

instance ConvertSuccess Ascii ByteString where
    {-# INLINE convertSuccess #-}
    convertSuccess = A.toByteString

instance ConvertSuccess Ascii CIAscii where
    {-# INLINE convertSuccess #-}
    convertSuccess = A.toCIAscii

instance ConvertSuccess Ascii String where
    {-# INLINE convertSuccess #-}
    convertSuccess = A.toString

instance ConvertSuccess Ascii Text where
    {-# INLINE convertSuccess #-}
    convertSuccess = A.toText

instance ConvertSuccess AsciiBuilder Ascii where
    {-# INLINE convertSuccess #-}
    convertSuccess = A.fromAsciiBuilder

instance ConvertSuccess AsciiBuilder Builder where
    {-# INLINE convertSuccess #-}
    convertSuccess = A.toBuilder

instance ConvertSuccess AsciiBuilder ByteString where
    {-# INLINE convertSuccess #-}
    convertSuccess = convertSuccessVia ((⊥) ∷ Ascii)

instance ConvertSuccess AsciiBuilder CIAscii where
    {-# INLINE convertSuccess #-}
    convertSuccess = convertSuccessVia ((⊥) ∷ Ascii)

instance ConvertSuccess AsciiBuilder String where
    {-# INLINE convertSuccess #-}
    convertSuccess = convertSuccessVia ((⊥) ∷ Ascii)

instance ConvertSuccess AsciiBuilder Text where
    {-# INLINE convertSuccess #-}
    convertSuccess = convertSuccessVia ((⊥) ∷ Ascii)

instance ConvertAttempt ByteString Ascii where
    {-# INLINEABLE convertAttempt #-}
    convertAttempt bs
        = case A.fromByteString bs of
            Just a  → return a
            Nothing → nonAscii bs

instance ConvertAttempt ByteString AsciiBuilder where
    {-# INLINEABLE convertAttempt #-}
    convertAttempt = convertAttemptVia ((⊥) ∷ Ascii)

instance ConvertAttempt ByteString CIAscii where
    {-# INLINEABLE convertAttempt #-}
    convertAttempt = convertAttemptVia ((⊥) ∷ Ascii)

instance ConvertSuccess CIAscii Ascii where
    {-# INLINE convertSuccess #-}
    convertSuccess = A.fromCIAscii

instance ConvertSuccess CIAscii AsciiBuilder where
    {-# INLINE convertSuccess #-}
    convertSuccess = convertSuccessVia ((⊥) ∷ Ascii)

instance ConvertSuccess CIAscii ByteString where
    {-# INLINE convertSuccess #-}
    convertSuccess = convertSuccessVia ((⊥) ∷ Ascii)

instance ConvertSuccess CIAscii String where
    {-# INLINE convertSuccess #-}
    convertSuccess = convertSuccessVia ((⊥) ∷ Ascii)

instance ConvertSuccess CIAscii Text where
    {-# INLINE convertSuccess #-}
    convertSuccess = convertSuccessVia ((⊥) ∷ Ascii)

instance ConvertAttempt String Ascii where
    {-# INLINEABLE convertAttempt #-}
    convertAttempt s
        = case A.fromChars s of
            Just a  → return a
            Nothing → nonAscii s

instance ConvertAttempt String AsciiBuilder where
    {-# INLINEABLE convertAttempt #-}
    convertAttempt = convertAttemptVia ((⊥) ∷ Ascii)

instance ConvertAttempt String CIAscii where
    {-# INLINEABLE convertAttempt #-}
    convertAttempt = convertAttemptVia ((⊥) ∷ Ascii)

instance ConvertAttempt Text Ascii where
    {-# INLINEABLE convertAttempt #-}
    convertAttempt t
        = case A.fromText t of
            Just a  → return a
            Nothing → nonAscii t

instance ConvertAttempt Text AsciiBuilder where
    {-# INLINEABLE convertAttempt #-}
    convertAttempt = convertAttemptVia ((⊥) ∷ Ascii)

instance ConvertAttempt Text CIAscii where
    {-# INLINEABLE convertAttempt #-}
    convertAttempt = convertAttemptVia ((⊥) ∷ Ascii)

nonAscii ∷ Failure (ConvertBoundsException Char α) f ⇒ α → f β
{-# INLINE nonAscii #-}
nonAscii = failure ∘ ConvertBoundsException '\0' '\x7F'

deriveAttempts [ ([t| Ascii        |], [t| AsciiBuilder |])
               , ([t| Ascii        |], [t| ByteString   |])
               , ([t| Ascii        |], [t| CIAscii      |])
               , ([t| Ascii        |], [t| String       |])
               , ([t| Ascii        |], [t| Text         |])
               , ([t| AsciiBuilder |], [t| Ascii        |])
               , ([t| AsciiBuilder |], [t| Builder      |])
               , ([t| AsciiBuilder |], [t| ByteString   |])
               , ([t| AsciiBuilder |], [t| CIAscii      |])
               , ([t| AsciiBuilder |], [t| String       |])
               , ([t| AsciiBuilder |], [t| Text         |])
               , ([t| CIAscii      |], [t| Ascii        |])
               , ([t| CIAscii      |], [t| AsciiBuilder |])
               , ([t| CIAscii      |], [t| ByteString   |])
               , ([t| CIAscii      |], [t| String       |])
               , ([t| CIAscii      |], [t| Text         |])
               ]
