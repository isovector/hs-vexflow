{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wall            #-}

module Language.Music.Vexflow where

import           BasePrelude hiding (Category (..), Min (..))
import           Control.Lens hiding (elementsOf)
import           Control.Monad.State
import           Data.Generics.Product
import qualified Data.Map as M
import           Language.Music.Types
import           Language.Music
import           Language.Music.Utils
import           Text.InterpolatedString.Perl6 (qc)


newtype JS t = JS
  { getJS :: String
  }
  deriving (Eq, Ord, Read, Data)

instance Show (JS t) where
  show = getJS


data Line deriving (Typeable)
data Voice deriving (Typeable)
data StaveNote deriving (Typeable)
data Formatter deriving (Typeable)
data Bar deriving (Typeable)
data Beam deriving (Typeable)
data Tuplet deriving (Typeable)

type DSQ = Int

byDSQ :: Dur -> DSQ
byDSQ (Dotted D32) = error "too fine grained"
byDSQ (Dotted d) =
  let z = byDSQ d
   in div z 2 + z
byDSQ D32 = 1
byDSQ D16 = 2
byDSQ D8 = 4
byDSQ D4 = 8
byDSQ D2 = 16
byDSQ D1 = 32



type JSM = State JSState

data JSState = JSState
  { jssContent     :: String
  , jssDrawQueue   :: [String]
  , jssDrawLate    :: [String]
  , jssFresh       :: Int
  , jssAccidentals :: M.Map Note Int
  , jssClef        :: Clef
  }
  deriving (Eq, Show, Ord, Read, Generic)


runJSM :: JSM a -> String
runJSM js =
  let jss = execState js $ JSState "" [] [] 0 mempty Treble
   in mconcat
        [ jssContent jss
        , intercalate "\n" $ jssDrawQueue jss
        , "\n"
        , intercalate "\n" $ jssDrawLate jss
        ]


freshName :: forall a. Typeable a => JSM (JS a)
freshName = do
  i <- gets jssFresh
  modify $ field @"jssFresh" %~ (+1)
  pure $ JS $ '_' : typeName @a ++ show i


typeName :: forall a. Typeable a => String
typeName = fmap (\x -> bool '_' x $ isAlphaNum x)
         . show
         . typeRep
         $ Proxy @a


context :: JS ()
context = JS "ctx"


emit :: String -> JSM ()
emit e = modify $ field @"jssContent" <>~ e <> "\n"


emitDraw :: JS a -> JSM ()
emitDraw (JS name) =
  modify $ field @"jssDrawQueue" <>~ [[qc|{name}.setContext({context}).draw();|]]


emitQueue :: String -> JSM ()
emitQueue e = modify $ field @"jssDrawQueue" <>~ [e]

emitLate :: String -> JSM ()
emitLate e = modify $ field @"jssDrawLate" <>~ [e]


drawClef :: JS Bar -> Clef -> JSM ()
drawClef s c = emit [qc|{s}.addClef("{show c & _head %~ toLower}");|]


drawTimeSig :: JS Bar -> (Int, Int) -> JSM ()
drawTimeSig s (t, b) = emit [qc|{s}.addTimeSignature("{t}/{b}");|]


drawKeySig :: JS Bar -> Note -> JSM ()
drawKeySig s n = emit [qc|{s}.addKeySignature("{n}");|]

drawCompress :: Element -> Int -> Dur -> JSM [JS StaveNote]
drawCompress es i _ = do
  v <- drawElement es
  t <- freshName @Tuplet

  emit $ mconcat
    [ [qc|var {t} = new Vex.Flow.Tuplet([{intercalate "," $ fmap show v}], |]
    , "{"
    , [qc|notes_occupied: {i}|]
    , "});"
    ]

  emitLate [qc|{t}.setContext({context}).draw();|]
  pure v



drawElement :: Element -> JSM [JS StaveNote]
drawElement (ENote n o d) = fmap pure $ drawNotes False [(n, o)] d
drawElement (EChord c o i [(ns, d)]) =
  let chordNotes = inOctaves o $ invert i $ notesOf c
      notes = fmap (chordNotes !!) ns
   in fmap pure $ drawNotes False notes d
drawElement (EChord c o i ns) =
  fmap join . traverse drawElement $ fmap (\n -> EChord c o i [n]) ns
drawElement (ERest d) = fmap pure $ drawNotes True [(B, 4)] d
drawElement (ERhythm d) = fmap pure $ drawRhythm d
drawElement (EGroup es) = fmap join $ traverse drawElement es
drawElement (ECompress es i d) = drawCompress es i d

drawRhythm :: Dur -> JSM (JS StaveNote)
drawRhythm d = do
  v <- freshName
  emit $ mconcat
    [ [qc|var {v} = new VF.StaveNote(|]
    , "{"
    , [qc|keys: ["X/4"], duration: "{d}"|]
    , "});"
    ]
  case d of
    Dotted _ -> emit [qc|{v}.addDotToAll();|]
    _ -> pure ()

  pure v


drawNotes :: Bool -> [(Note, Int)] -> Dur -> JSM (JS StaveNote)
drawNotes isRest notes d = do
  v <- freshName
  let jsNotes = intercalate "\",\""
              $ fmap (\(n, o) -> [qc|{uglyShowNote n}/{o}|]) notes
  clef <- gets jssClef
  emit $ mconcat
    [ [qc|var {v} = new VF.StaveNote(|]
    , "{"
    , [qc|keys: ["{jsNotes}"], clef: "{show clef & _head %~ toLower}", duration: "{d}{bool "" "r" isRest}"|]
    , "});"
    ]
  doAccidentals v $ fmap fst notes
  case d of
    Dotted _ -> emit [qc|{v}.addDotToAll();|]
    _ -> pure ()

  pure v


drawVoice :: Int -> Float -> JS Bar -> [JS StaveNote] -> JSM (JS Voice)
drawVoice x w stave sns = do
  vv <- freshName @[StaveNote]
  v <- freshName @Voice
  let sns' = intercalate "," $ fmap show sns
  emit [qc|var {vv} = [{sns'}];|]
  emit [qc|var {v} = new VF.Voice();|]
  emit [qc|{v}.addTickables({vv});|]

  fm <- freshName @Formatter
  case x of
    0 -> emitQueue [qc|var {fm} = new VF.Formatter().joinVoices([{v}]).format([{v}], {w} - {stave}.getNoteStartX());|]
    _ -> emitQueue [qc|var {fm} = new VF.Formatter().joinVoices([{v}]).format([{v}], {w});|]
  emitQueue [qc|{v}.draw({context}, {stave});|]

  beam <- freshName @Beam
  emit [qc|var {beam} = VF.Beam.generateBeams({vv});|]
  emitQueue [qc|{beam}.forEach(beam => beam.setContext({context}).draw());|]

  pure v


doAccidentals :: JS StaveNote -> [Note] -> JSM ()
doAccidentals v notes = do
  accs <- gets jssAccidentals
  for_ (zip [0..] notes) $ \(i :: Int, n) -> do
    unless (agrees accs n) $ do
      let adj = adjustment n
      emit [qc|{v}.addAccidental({i}, new VF.Accidental("{showAdjustment adj}"));|]
      modify $ field @"jssAccidentals" %~ M.insert (baseNote n) adj


agrees :: M.Map Note Int -> Note -> Bool
agrees m n =
  adjustment n == fromMaybe 0 (M.lookup (baseNote n) m)


accidentalsForKey :: Note -> M.Map Note Int
accidentalsForKey c =
  let notes = interval c <$> [Unison, Maj2, Maj3, Perf4, Perf5, Maj6, Maj7]
   in M.fromList
    . fmap (\x -> ( baseNote x
                  , adjustment x
                  )
           )
    $ filter ((/= 0) . adjustment) notes


getActivity :: Element -> [Dur]
getActivity (EChord _ _ _ d)  = fmap snd d
getActivity (ENote _ _ d)     = pure d
getActivity (ERest d)         = pure d
getActivity (ERhythm d)       = pure d
getActivity (EGroup g)        = g >>= getActivity
getActivity (ECompress _ i d) = replicate i d


getDuration :: Element -> Sum Int
getDuration = foldMap (Sum . byDSQ) . getActivity


elementsOf :: Element -> [Element]
elementsOf (EGroup e) = e
elementsOf e          = [e]


splitMono :: Monoid m => (m -> Bool) -> (a -> m) -> [a] -> ([a], [a])
splitMono f t as = join (***) (fmap fst)
                 . span (f . snd)
                 . fmap (second $ foldMap t)
                 . zip as
                 . tail
                 $ inits as


groupMono :: Monoid m => (m -> Bool) -> (a -> m) -> [a] -> [[a]]
groupMono _ _ [] = []
groupMono f t as =
  let (a, b) = splitMono f t as
   in a : groupMono f t b


drawStave :: Stave -> JSM Int
drawStave (SGroup _) = undefined
drawStave (Stave clef key timesig e) = do
  modify $ field @"jssClef" .~ fromMaybe Treble clef

  (bs, h) <- drawBars 32 e
  let v = head bs

  for_ clef    $ drawClef v
  for_ timesig $ drawTimeSig v
  for_ key     $ \k -> do
    drawKeySig v k
    modify $ field @"jssAccidentals" .~ accidentalsForKey k
  pure h


drawEverythingYo :: String -> Stave -> JSM ()
drawEverythingYo divName s = do
  emit [qc|
var VF = Vex.Flow;
var div = document.getElementById("{divName}")
var renderer = new VF.Renderer(div, VF.Renderer.Backends.SVG);
var {context} = renderer.getContext();
{context}.setFont("Arial", 10, "").setBackgroundFillStyle("#eed");
|]
  h <- drawStave s
  emit [qc|renderer.resize({systemWidth + 20}, {firstStave + staveHeight * h})|]


------------------------------------------------------------------------------
-- | How big we assume each activity is, in pixels
activitySize :: Int
activitySize = 40


------------------------------------------------------------------------------
-- | How wide we assume each system is, in pixels
systemWidth :: Int
systemWidth = 460


buildSystems :: Int -> DSQ -> Element -> [[(Element, Int)]]
buildSystems w dsq e =
  let bs   = splitBars dsq e
      acts = fmap (\x -> (x, (* activitySize) . length $ getActivity x)) bs
   in groupMono
        ((<= w) . getSum)
        (Sum . snd)
        acts


spaceFirstBar :: Int -> Int
spaceFirstBar = round . (* id @Double 1.5) . fromIntegral

staveHeight :: Int
staveHeight = 110


firstStave :: Int
firstStave = 40


drawBars :: DSQ -> Element -> JSM ([JS Bar], Int)
drawBars dsq e = do
  let systems = buildSystems systemWidth dsq e
  fmap (, length systems) $ fmap join $ for (zip [0..] systems) $ \(y :: Int, row) -> do
    let totalSize = getSum . foldMap Sum $ fmap snd row & bool id (_head %~ spaceFirstBar) (y == 0)
        getWidth relSize = fromIntegral @_ @Float systemWidth
                         * fromIntegral relSize
                         / fromIntegral totalSize
    let widths = fmap (getWidth . snd) $ row & bool id (_head . _2 %~ spaceFirstBar) (y == 0)
        startXs = fmap (getSum . foldMap Sum) $ inits widths
        row' = zip (fmap fst row) $ zip startXs widths
    for (zip [0..] row') $ \(x, (bar, (xpos, w))) -> do
      -- TODO(sandy): reset key on each bar
      v <- freshName
      emit [qc|var {v} = new VF.Stave({10 + xpos}, {firstStave + y * staveHeight}, {w});|]
      emitDraw v

      sn <- drawElement bar
      void $ drawVoice x w v sn
      pure v


splitBars :: DSQ -> Element -> [Element]
splitBars _ e
  | elementsOf e == [] = []
splitBars dsqs e =
  let (fit, dont) = splitMono ((<= dsqs) . getSum) getDuration $ elementsOf e
      fitDur = getSum $ foldMap getDuration fit
   in case (compare fitDur dsqs, listToMaybe dont) of
        (LT, Just _n) -> error "not enough"
          -- let (fill, remaining) = breakElement (dsqs - fitDur) n
          --  in (EGroup $ fit ++ [fill])
          --   : splitBars dsqs (EGroup $ remaining : dont)
        (EQ, _) -> EGroup fit : splitBars dsqs (EGroup dont)
        _ -> error "aghigdsi"

