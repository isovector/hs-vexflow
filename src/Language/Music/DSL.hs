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
  | WrongTuplet Int Int
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
  optional (fmap (read . pure) parseDigit)
    >>= maybe (pure 4) pure


parseCompress :: Parser Element
parseCompress = do
  e <- between (char '(') (char ')') parseElement
  let act = getActivity e
  void $ char '^'
  n <- read <$> many parseDigit

  unless (n == length act)
    . customFailure
    . WrongTuplet n
    $ length act

  void $ char ':'
  i <- read <$> some parseDigit
  -- TODO(sandy): this is probably wrong
  pure $ ECompress e i $ head act


parseDiddle :: Parser Element
parseDiddle = do
  c <- parseChord
  i <- parseInversion
  z <- optional
     . between (char '[')
               (char ']')
     . flip sepBy (char ',') $ do
         cs  <- some parseDigit
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
    [ parseCompress
    , try $ parseDiddle
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
  t <- some parseDigit
  void $ char '/'
  b <- some parseDigit
  space
  pure (read t, read b)

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']


parseStave :: Parser Stave
parseStave = do
  c <- parseClef
  k <- try parseKey
  t <- try parseTimeSignature
  e <- parseElement
  pure $ Stave c k t e


musicParser
    :: String
    -> String
    -> Either (ParseErrorBundle String MusicError) String
musicParser divName = fmap (runJSM . drawEverythingYo divName) . runParser parseStave ""

