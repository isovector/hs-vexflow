{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Language.Music.DSL where


import BasePrelude hiding (many, Min (..), First (..), Category (..), try, some)
import Data.List.Utils (replace)
import Language.Music
import Language.Music.Types
import Language.Music.Vexflow
import Language.Music.Utils
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec MusicError String

data MusicError
  = FlatsAndSharps
  deriving (Eq, Ord, Show, Read)


parseNote :: Parser Note
parseNote = do
  n <- oneOf "ABCDEFG"
  a <- many $ oneOf "b#"

  when (length (group a) > 1) $
    customFailure FlatsAndSharps

  pure $ read $ replace "#" "s" $ n : a


parseChord :: Parser (ChordV Note)
parseChord = do
  n <- parseNote
  asum $ fmap (\(s, f) -> string s >> pure (f n))
    [ ("M7", Maj7C)
    , ("M", Maj)
    , ("m7", Min7C)
    , ("mb5", Dim)
    , ("m", Min)
    , ("7", Dom7)
    ]


parseInversion :: Parser Inversion
parseInversion =
  asum $ fmap (\(s, x) -> string s >> pure x)
    [ (">>>", Fourth)
    , (">>", Third)
    , (">", Second)
    , ("", First)
    ]


parseDuration :: Parser Dur
parseDuration =
  parseMaybeDuration >>= \case
    Nothing -> pure D4
    Just d -> optional (char '.')
          >>= pure
            . maybe d (const $ Dotted d)


parseMaybeDuration :: Parser (Maybe Dur)
parseMaybeDuration =
  optional $ asum
    [ D32 <$ string ":32"
    , D16 <$ string ":16"
    , D8  <$ string ":8"
    , D4  <$ string ":4"
    , D2  <$ string ":2"
    , D1  <$ string ":1"
    ]


parseOctave :: Parser Int
parseOctave = do
  void $ optional $ string "@"
  optional (fmap (read . pure) $ oneOf ['0'..'9'])
    >>= maybe (pure 4) pure


parseDiddle :: Parser Element
parseDiddle = do
  c <- parseChord
  i <- parseInversion
  z <- optional
     . between (char '[')
               (char ']')
     . flip sepBy (char ',') $ do
         cs  <- some $ oneOf ['0'..'9']
         d <- parseMaybeDuration
         pure (fmap (read @Int . pure) cs, d)
  o <- parseOctave
  d <- parseDuration

  let x = (, d) $ [0 .. length (notesOf c) - 1]
  pure . EChord c o i
       . maybe [x]
           ( getCompose
           . fmap (fromMaybe d)
           . Compose
           )
       $ z


parseNoteElement :: Parser Element
parseNoteElement = do
  n <- parseNote
  o <- parseOctave
  d <- parseDuration
  pure $ ENote n o d


parseRest :: Parser Element
parseRest = do
  void $ char '%'
  ERest <$> parseDuration

parseRhythm :: Parser Element
parseRhythm = do
  void $ char 'X'
  ERhythm <$> parseDuration


parseElement :: Parser Element
parseElement = do
  es <- flip sepBy1 space $ asum
    [ try $ parseDiddle
    , parseNoteElement
    , parseRhythm
    , parseRest
    ]
  pure $ mconcat es


parseClef :: Parser (Maybe Clef)
parseClef = optional $ asum
  [ Treble <$ string "treble" <* space
  , Bass   <$ string "bass"   <* space
  ]


parseKey :: Parser (Maybe Note)
parseKey = optional $ parseNote <* space


parseTimeSignature :: Parser (Maybe (Int, Int))
parseTimeSignature = optional $ do
  t <- some $ oneOf ['0'..'9']
  void $ char '/'
  b <- some $ oneOf ['0'..'9']
  space
  pure (read t, read b)


parseStave :: Parser Stave
parseStave = do
  c <- parseClef
  k <- parseKey
  t <- parseTimeSignature
  e <- parseElement
  pure $ Stave c k t e


musicParser
    :: String
    -> String
    -> Either (ParseErrorBundle String MusicError) String
musicParser divName = fmap (runJSM . drawEverythingYo divName) . runParser parseStave ""

