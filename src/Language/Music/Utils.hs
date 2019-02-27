{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Language.Music.Utils where

import Data.Bool
import Language.Music.Types


------------------------------------------------------------------------------
-- | Show a note in a less unicode-y way.
uglyShowNote :: Note -> String
uglyShowNote = foldNote show (++ "b") (++ "#")


------------------------------------------------------------------------------
-- | Turn an adjustment into its vexflow representation.
--
-- @showAdjustment 1 = "#"@
-- @showAdjustment (-2) = "bb"@
showAdjustment :: Int -> String
showAdjustment 0 = "n"
showAdjustment 1 = "#"
showAdjustment 2 = "##"
showAdjustment (-1) = "b"
showAdjustment (-2) = "bb"
showAdjustment _ = error "bad adj"


------------------------------------------------------------------------------
-- | Compute the enharmonic pitchclass of a note. Under this scheme, 'Bs' is
-- equal to 'C'.
pitchClass :: Note -> Int
pitchClass =
  modular 12 . foldNote (semitonesAwayFromC . toNoteClass) (subtract 1) (+1)


------------------------------------------------------------------------------
-- | Compute 'n' mod 'z'.
-- TODO(sandy): this is stupid
modular :: Int -> Int -> Int
modular z n
  | n < 0     = n + z
  | n >= z    = n - z
  | otherwise = n


------------------------------------------------------------------------------
-- | How big is a tonal degree in the major scale?
semitonesAwayFromC :: Roman -> Int
semitonesAwayFromC I   = 0
semitonesAwayFromC II  = 2
semitonesAwayFromC III = 4
semitonesAwayFromC IV  = 5
semitonesAwayFromC V   = 7
semitonesAwayFromC VI  = 9
semitonesAwayFromC VII = 11


------------------------------------------------------------------------------
-- | Determine the "size" of an interval. For example 'Min2' and 'Maj2' both
-- have size 'II'.
intervalSize :: Interval -> Roman
intervalSize Unison = I
intervalSize Min2   = II
intervalSize Maj2   = II
intervalSize Min3   = III
intervalSize Maj3   = III
-- intervalSize Dim4   = IV
intervalSize Perf4  = IV
-- intervalSize Aug4   = IV
intervalSize Dim5  = V
intervalSize Perf5  = V
-- intervalSize Aug5   = V
intervalSize Min6   = VI
intervalSize Maj6   = VI
intervalSize Min7   = VII
intervalSize Maj7   = VII


------------------------------------------------------------------------------
-- | Modular arithmetic in an 'Enum'.
add :: forall a. (Enum a, Bounded a) => a -> a -> a
add a b = toEnum . modular (fromEnum (maxBound @a) + 1) $ fromEnum a + fromEnum b


------------------------------------------------------------------------------
-- | Turn an adjustment into an 'Accidental'.
adjustmentToAccidental :: Int -> Accidental
adjustmentToAccidental (-1) = Flat
adjustmentToAccidental 0 = Natural
adjustmentToAccidental 1 = Sharp
adjustmentToAccidental _ = error "DONT"


------------------------------------------------------------------------------
-- | The inverse of 'adjustmentToAccidental'.
accidentalToAdjustment :: Accidental -> Int
accidentalToAdjustment Flat = -1
accidentalToAdjustment Natural = 0
accidentalToAdjustment Sharp = 1


------------------------------------------------------------------------------
-- | Compute the adjustment on a note away from its base note. For example,
-- @adjustment Css = 2@.
adjustment :: Note -> Int
adjustment n = fromEnum n - fromEnum (fromNoteClass $ toNoteClass n)


------------------------------------------------------------------------------
-- | Add an adjustment to a 'Note'. NB: This doesn't correctly handle overflow
-- or underflow of adjustments.
adjust :: Note -> Int -> Note
adjust n a = toEnum $ fromEnum n + a


------------------------------------------------------------------------------
-- | Compute an interval above a note.
interval :: Note -> Interval -> Note
interval n i =
  let nname = fromNoteClass $ add (toNoteClass n) $ intervalSize i
      adj = modular 12 (pitchClass n + fromEnum i) - pitchClass nname
   in adjust nname adj


------------------------------------------------------------------------------
-- | Get the notes in a 'ChordV'.
notesOf :: ChordV Note -> [Note]
notesOf (Maj c)   = interval c <$> [Unison, Maj3, Perf5]
notesOf (Min c)   = interval c <$> [Unison, Min3, Perf5]
notesOf (Maj7C c) = interval c <$> [Unison, Maj3, Perf5, Maj7]
notesOf (Min7C c) = interval c <$> [Unison, Min3, Perf5, Min7]
notesOf (Dom7 c)  = interval c <$> [Unison, Maj3, Perf5, Min7]
notesOf (Dim c)   = interval c <$> [Unison, Min3, Dim5]


------------------------------------------------------------------------------
-- | Make the notes of a chord be in the correct octave.
inOctaves
    :: Int  -- ^ Octave for first note.
    -> [Note]
    -> [(Note, Int)]
inOctaves _ [] = []
inOctaves o ns@((fromEnum -> n) : _) = fmap (\z -> (z, bool o (o+1) $ fromEnum z < n)) ns


------------------------------------------------------------------------------
-- | Rotate a list.
rotate :: [a] -> [a]
rotate [] = []
rotate (a : as) = as ++ [a]


------------------------------------------------------------------------------
-- | Compute the inversion of a chord.
invert :: Inversion -> [a] -> [a]
invert i n = iterate rotate n !! fromEnum i


------------------------------------------------------------------------------
-- | eg. @Cs -> C@
baseNote :: Note -> Note
baseNote = fromNoteClass . toNoteClass

