{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}

module Language.Music.Types where

import Data.Data
import GHC.Generics
import Data.Monoid ((<>))


data Note
  = Cbb | Cb | C | Cs | Css
  | Dbb | Db | D | Ds | Dss
  | Ebb | Eb | E | Es | Ess
  | Fbb | Fb | F | Fs | Fss
  | Gbb | Gb | G | Gs | Gss
  | Abb | Ab | A | As | Ass
  | Bbb | Bb | B | Bs | Bss
  deriving (Eq, Ord, Enum, Bounded, Generic, Read, Data)

instance Show Note where
  show C = "C"
  show D = "D"
  show E = "E"
  show F = "F"
  show G = "G"
  show A = "A"
  show B = "B"
  show a = foldNote (show . fromNoteClass . toNoteClass)
                    (++ "♭")
                    (++ "♯")
                    a


------------------------------------------------------------------------------
-- | Fold a note by doing something for each flat and sharp.
foldNote
    :: (Note -> a)
    -> (a -> a)  -- ^ Do this for every flat.
    -> (a -> a)  -- ^ Do this for every sharp.
    -> Note
    -> a
foldNote f _ _ C   =         f C
foldNote f _ s Cs  = s     $ f C
foldNote f _ s Css = s . s $ f C
foldNote f b _ Cb  = b     $ f C
foldNote f b _ Cbb = b . b $ f C
foldNote f _ _ D   =         f D
foldNote f _ s Ds  = s     $ f D
foldNote f _ s Dss = s . s $ f D
foldNote f b _ Db  = b     $ f D
foldNote f b _ Dbb = b . b $ f D
foldNote f _ _ E   =         f E
foldNote f _ s Es  = s     $ f E
foldNote f _ s Ess = s . s $ f E
foldNote f b _ Eb  = b     $ f E
foldNote f b _ Ebb = b . b $ f E
foldNote f _ _ F   =         f F
foldNote f _ s Fs  = s     $ f F
foldNote f _ s Fss = s . s $ f F
foldNote f b _ Fb  = b     $ f F
foldNote f b _ Fbb = b . b $ f F
foldNote f _ _ G   =         f G
foldNote f _ s Gs  = s     $ f G
foldNote f _ s Gss = s . s $ f G
foldNote f b _ Gb  = b     $ f G
foldNote f b _ Gbb = b . b $ f G
foldNote f _ _ A   =         f A
foldNote f _ s As  = s     $ f A
foldNote f _ s Ass = s . s $ f A
foldNote f b _ Ab  = b     $ f A
foldNote f b _ Abb = b . b $ f A
foldNote f _ _ B   =         f B
foldNote f _ s Bs  = s     $ f B
foldNote f _ s Bss = s . s $ f B
foldNote f b _ Bb  = b     $ f B
foldNote f b _ Bbb = b . b $ f B


data Accidental = Flat | Natural | Sharp
  deriving (Eq, Ord, Enum, Bounded, Generic, Read, Data)

instance Show Accidental where
  show Flat    = "♭"
  show Natural = "♮"
  show Sharp   = "♯"

instance Semigroup Accidental where
  (<>) Flat    Flat    = error "i told you we needed double flats"
  (<>) Flat    Sharp   = Natural
  (<>) Sharp   Flat    = Natural
  (<>) Sharp   Sharp   = error "i told you we needed double sharps"
  (<>) Natural a       = a
  (<>) a       Natural = a

instance Monoid Accidental where
  mempty = Natural


data Clef
  = Treble
  | Bass
  deriving (Eq, Ord, Read, Show, Data)


------------------------------------------------------------------------------
-- | Types of chord structures.
data ChordV a
  = Maj a
  | Maj7C a
  | Min7C a
  | Dom7 a
  -- | MinMaj a
  -- | Sus a
  | Min a
  | Dim a
  -- | Alt (ChordV a)
  -- | Slash (ChordV a) a
  deriving (Eq, Ord, Functor, Generic, Read, Data)


instance Show a => Show (ChordV a) where
  show (Maj a)     = show a
  show (Maj7C a)    = show a ++ "Δ"
  show (Min7C a)    = show a ++ "m7"
  -- show (MinMaj a)  = show a ++ "Δ-"
  show (Dom7 a)    = show a ++ "7"
  -- show (Sus a)     = show a ++ "sus"
  show (Min a)     = show a ++ "m"
  show (Dim a)     = show a ++ "∅"
  -- show (Mod x c)   = show c ++ " " ++ show x
  -- show (Slash c a) = show c ++ "/" ++ show a
  -- show (Alt c)     = show c ++ "alt"


------------------------------------------------------------------------------
-- | Roman numerals.
data Roman
  = I
  | II
  | III
  | IV
  | V
  | VI
  | VII
  deriving (Eq, Ord, Enum, Show, Generic, Bounded, Read, Data)


------------------------------------------------------------------------------
-- | Chord inversions.
data Inversion = First | Second | Third | Fourth
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Read, Data)


------------------------------------------------------------------------------
-- | Pitch intervals
data Interval
  = Unison
  | Min2
  | Maj2
  | Min3
  | Maj3
  -- | Dim4
  | Perf4
  -- | Aug4
  | Dim5
  -- | Dim5
  | Perf5
  -- | Aug5
  | Min6
  | Maj6
  | Min7
  | Maj7
  deriving (Eq, Ord, Enum, Bounded, Generic, Read, Data)


instance Show Interval where
  show Unison = "U"
  show Min2 = "m2"
  show Maj2 = "M2"
  show Min3 = "m3"
  show Maj3 = "M3"
  -- show Dim4 = "4°"
  show Perf4 = "4"
  -- show Aug4 = "4+"
  show Dim5 = "5°"
  show Perf5 = "5"
  -- show Aug5 = "5+"
  show Min6 = "m6"
  show Maj6 = "M6"
  show Min7 = "m7"
  show Maj7 = "M7"


------------------------------------------------------------------------------
-- | Get the base note. For example, 'C' -> 'I', 'Cb' -> 'I', but 'Bs' ->
-- 'VII'.
toNoteClass :: Note -> Roman
toNoteClass = foldNote go id id
  where
    go = \case
      C -> I
      D -> II
      E -> III
      F -> IV
      G -> V
      A -> VI
      B -> VII


------------------------------------------------------------------------------
-- | @toNoteClass . fromNoteClass = id@
fromNoteClass :: Roman -> Note
fromNoteClass I   = C
fromNoteClass II  = D
fromNoteClass III = E
fromNoteClass IV  = F
fromNoteClass V   = G
fromNoteClass VI  = A
fromNoteClass VII = B

