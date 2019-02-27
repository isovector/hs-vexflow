{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE TupleSections       #-}

module Language.Music where

import BasePrelude hiding (Category (..), Min (..))
import Generics.SYB hiding (Generic)
import Language.Music.Types
import Language.Music.Utils


data Stave
  = Stave (Maybe Clef) (Maybe Note) (Maybe (Int, Int)) Element
  | SGroup [Stave]
  deriving (Eq, Ord, Show, Read, Data, Typeable)


instance Semigroup Stave where
  SGroup s1 <> SGroup s2 = SGroup $ s1 <> s2
  SGroup s1 <> s2        = SGroup $ s1 ++ [s2]
  s1 <> SGroup s2        = SGroup $ s1 : s2
  s1 <> s2               = SGroup [s1, s2]


data Element
  = EChord (ChordV Note) Int Inversion [([Int], Dur)]
  | ENote Note Int Dur
  | ERest Dur
  | ERhythm Dur
  | EGroup [Element]
  -- ETranspose
  deriving (Eq, Ord, Show, Read, Data, Typeable)


data Dur
  = D1
  | D2
  | D4
  | D8
  | D16
  | D32
  | Dotted Dur
  deriving (Eq, Ord, Read, Data, Typeable)


instance Show Dur where
  show D1 = "w"
  show D2 = "h"
  show D4 = "4"
  show D8 = "8"
  show D16 = "16"
  show D32 = "32"
  show (Dotted d) = show d ++ "d"

instance Semigroup Element where
  EGroup s1 <> EGroup s2 = EGroup $ s1 <> s2
  EGroup s1 <> s2        = EGroup $ s1 ++ [s2]
  s1 <> EGroup s2        = EGroup $ s1 : s2
  s1 <> s2               = EGroup [s1, s2]

instance Monoid Element where
  mempty = EGroup []


withClef :: Clef -> Stave -> Stave
withClef c = everywhere $ mkT $ const $ Just c


withKey :: Note -> Stave -> Stave
withKey k = everywhere $ mkT $ \(Stave c _ t e) -> Stave c (Just k) t e


withTimeSig :: Int -> Int -> Stave -> Stave
withTimeSig t b = everywhere $ mkT $ const $ Just (t, b)


chord :: ChordV Note -> Int -> Inversion -> Element
chord c o i = EChord c o i $ pure $ (, D4) $ [0 .. length (notesOf c) - 1]


note :: Note -> Int -> Element
note n o = ENote n o D4


rest :: Element
rest = ERest D4


dur :: Dur -> Element -> Element
dur d = everywhere $ mkT $ const d


annotate :: String -> Element -> Element
annotate = undefined


bars :: Element -> Stave
bars = Stave Nothing Nothing Nothing

