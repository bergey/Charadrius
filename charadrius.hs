{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (takeWhile)
import Data.Attoparsec.Text
import Control.Applicative
import Data.List (intersperse, isSuffixOf)
import Control.Monad
import Data.Monoid (mconcat, mappend)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.ByteString as BS
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Text.Encoding.Error (lenientDecode)

(*>|) :: Applicative f => f a -> b -> f b
(*>|) a b  = a *> (pure b)

rejectIf :: (a -> Bool) -> Parser a -> Parser a
rejectIf predicate pars = do
  res <- pars
  if predicate res
     then empty
    else return res

betweenBraces :: Parser a -> Parser a
betweenBraces p = char '{' *> p <* char '}'

-- another function from Parsec not included in attoparsec
optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = Just <$> p <|> pure Nothing

-- notFollowedBy :: Parser a -> Parser ()
-- notFollowedBy p = lookahead p *>| () <|> empty

-- isAlpha_ascii_w8 c = uppercase c || lowercase c where
--   uppercase c = (c >= 65) && (c <= 90)
--   lowercase c = (c >= 97) && (c <= 122)

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

notFollowedBy :: (Char -> Bool) -> Parser ()
notFollowedBy predicate = do
  next <- peekChar
  case next of
    Nothing -> empty
    Just  c -> case predicate c of
      True  -> empty
      False -> return ()

spaces = T.pack <$> many space

hiddenGroupPrefix :: Bool -> Parser ()
hiddenGroupPrefix True = string "\\*" *> return ()
hiddenGroupPrefix False = return ()

-- s should not have spaces, backslashes
rtfGroup :: Bool -> T.Text -> Parser a -> Parser a
rtfGroup h s p = try $ betweenBraces (hiddenGroupPrefix h *> rtfCmd s *> p)

-- accept the payload of the group, assuming no nested groups or control words
rtfLiteral :: Parser Text
rtfLiteral = takeWhile1 $ notInClass "{}\\\n\r"

-- strictly speaking, command words are different from command characters
-- this parser does both, and can also be used to parse things that
-- RTF disallows, like \:magic:
rtfCmd :: Text -> Parser Text
rtfCmd s = try $ char '\\' *> string s <* notFollowedBy isAlphaNum <* skipWhile hSpace where
  hSpace c = isSpace c && c /= '\n' && c /= '\r'

-- when the command begins with something other than alphaNum
-- it's one character long, and dosen't need space to delimit the end

rtfCmdChar :: Char -> Parser Char
rtfCmdChar c = try $ char '\\' *> char c

skipRTF :: Parser ()
skipRTF = anyRTF *> empty

-- even accepts baregroups, so that the nested anyRTF will do so
--anyGroup :: Parser Text
anyGroup :: Parser Text
anyGroup = T.concat <$> sequence [string "{", option "" (string "\\"), option "" (string "*\\"), takeWhile isAlphaNum, mconcat <$> many anyRTF, string "}"]

anyCmd :: Parser Text
anyCmd = liftA2 mappend (string "\\") (takeWhile isAlphaNum)

anyRTF :: Parser Text
anyRTF = anyGroup <|> anyCmd <|> rtfLiteral <|> bareGroup

-- Stroke left-hand-keys right-hand-keys
data Stroke = Stroke Text Text | Junk Text
              deriving (Read, Show)

data Translation = PlainText Text | Fingerspell Text | Punctuation Text | DelSpace | NewPar (Maybe Int) | Automatic Text | SpecialChar SpecialChar | CapNext | CharGroup Char | Stitch Text | DelStroke | Unknown Text | Ignored
                                    deriving (Read, Show)

-- This is only a small fraction of those specified by RTF
-- Adding them as I encounter them in dictionaries in the wild
-- Representing as UTF-8 chars might be a better approach
data SpecialChar = NBSP | OptionalHyphen | NBHyphen
                 deriving (Read,Show)

specialChar :: Parser SpecialChar
specialChar = rtfCmdChar '~' *>| NBSP <|>
              rtfCmdChar '-' *>| OptionalHyphen <|>
              rtfCmdChar '_' *>| NBHyphen

data Brief = Brief [Stroke] [Translation]
             deriving (Read, Show)

-- We could be more strict here, and only accept
-- chars in steno order
lhs :: Parser Text
lhs = takeWhile (inClass "STKPWHRAO*#0123456789")

rhs :: Parser Text
rhs = takeWhile (inClass "EUFRPBLGTSDZ*#0123456789")

stroke :: Parser Stroke
stroke = do
  l <- lhs
  optional $ char '-'
  r <- rhs
  if (l == "") && (r == "")
     then mzero
    else return $ Stroke l r

stenoGroup :: Parser [Stroke]
stenoGroup = (rtfGroup True "cxs" (sepBy1 (stroke <|> Junk <$> rtfLiteral) (char '/'))) <?> "Steno input group"
--stenoGroup = rtfGroup True "cxs" (sepBy1 (Junk <$> rtfLiteral) (char '/'))

par :: Parser Translation
par = (rtfCmd "par") *> (NewPar <$> optionMaybe newstyle) where
  newstyle = string "\\s" *> decimal

knownCharGroups :: Parser Translation
knownCharGroups =  CharGroup <$> (try $ betweenBraces $ satisfy $ inClass "'$|")

bareGroup :: Parser Text
bareGroup = try $ betweenBraces rtfLiteral

unknown :: Parser Translation
-- Calling at the end of translation ensures we don't take any known translation term
-- notFollowedBy ensures we don't take the stenoGroup
unknown = Unknown <$> (bareGroup <|>
                       anyGroup <|>
                       anyCmd )

translation :: Parser [Translation]
translation = many1 (
              PlainText <$> rtfLiteral <|>
              (Fingerspell <$> rtfGroup False "cxfing" rtfLiteral <?> "Fingerspell group") <|>
              Punctuation <$> rtfGroup False "cxp" rtfLiteral <|>
              Automatic <$> rtfGroup False "cxa" rtfLiteral <|>
              Stitch <$> rtfGroup False "cxstit" rtfLiteral <|>
              rtfCmd "cxds" *> pure DelSpace <|>
              (rtfCmd "cxdstroke" <|> rtfCmd "cxdstrokes") *>| DelStroke <|>
              par <|>
              knownCharGroups <|>
              SpecialChar <$> specialChar <|>
              rtfCmd "cxfc" *>| CapNext <|>
              ignored <|>
              unknown -- must come last
              ) <* skipSpace

stenoEntry :: Parser Brief
stenoEntry = liftA2 Brief stenoGroup translation

-- various extensions occur in digitalcat RTF files,
-- W accept them as input, but don't attempt to interpret them
-- this is different from the Unknown Translation
-- in that we know what these mean, and don't need to warn the user that we skip them
ignored :: Parser Translation
ignored = flags <|> date where
  flags = rtfGroup True "cxsvatdictflags" anyRTF *>| Ignored
  date  = rtfGroup True "cxsvatdictentrydate" (many anyRTF) *>| Ignored

rtfDictionary :: Parser [Brief]
rtfDictionary = many1 stenoEntry

-- scans header content, doesn't use it
dictionaryFile :: Parser [Brief]
dictionaryFile = (rtfGroup False "rtf1" $
                 manyTill anyChar stenoEntry *> rtfDictionary) <* skipSpace <* endOfInput

--I think using aeson would just make this more complex
toJSON :: [Brief] -> Text
toJSON xs = T.concat ["{\n", T.concat $ map brief xs, "}\n"] where
  brief (Brief stks trn) = T.concat [quoteS stks,  ": ",  T.concat $ map translation trn,  ",\n"]
  quoteS stks = T.concat ["\"", T.intercalate "/" $ map hyphenate stks,  "\""]
  hyphenate (Stroke l r) = T.concat [l,  "-",  r]  -- always inserts hyphen
  translation (PlainText s) = T.concat ["\"",  s,  "\""]
  translation (Fingerspell s) = T.concat ["\"{&",  s,  "\""]
  translation (Punctuation s) = T.concat ["\"",  s,  "\""]

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

-- file handling section
jsonExtension :: String -> String
jsonExtension fn = if isSuffixOf ".rtf" fn
                   then (reverse . (drop 4) . reverse) fn ++ ".json"
                   else fn ++ ".json"

main :: IO ()
main = do
  let filename = "ploverdicts/mk-test.rtf"
  bs <- BS.readFile filename
  let utf = decodeUtf8With lenientDecode bs
  case parse dictionaryFile utf of
       (Done _ r) -> T.writeFile (jsonExtension filename) (toJSON r)
       (Fail _ c e) -> putStrLn e
       (Partial _) -> putStrLn "Don't know how to get more data for parser"