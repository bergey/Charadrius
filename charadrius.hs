import Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import Control.Applicative
import Data.List (intersperse)
import Control.Monad

(*>|) a b  = a *> (pure b)

betweenBraces p = char '{' *> p <* char '}'

hiddenGroupPrefix :: Bool -> Parser ()
hiddenGroupPrefix True = string "\\*" *> return ()
hiddenGroupPrefix False = return ()

-- s should not have spaces, backslashes
rtfGroup :: Bool -> String -> Parser a -> Parser a
rtfGroup h s p = try $ betweenBraces (hiddenGroupPrefix h *> rtfCmd s *> p)

-- accept the payload of the group, assuming no nested groups or control words
rtfLiteral :: Parser String
rtfLiteral = many1 $ notInClass "{}\\\n\r"

-- strictly speaking, command words are different from command characters
-- this parser does both, and can also be used to parse things that
-- RTF disallows, like \:magic:
rtfCmd s = try $ char '\\' *> string s <* notFollowedBy alphaNum <* skipWhile isSpace

-- when the command begins with something other than alphaNum
-- it's one character long, and dosen't need space to delimit the end
rtfCmdChar c = try $ char '\\' *> char c

skipRTF :: Parser ()
-- skipRTF = many ( rtfLiteral *> empty <|>
--                  char '\\' *> many letter *> empty <|>
--                  between (char '{') (char '}') skipRTF
--                ) *> empty -- (), not [()]
skipRTF = anyRTF *> empty

-- even accepts baregroups, so that the nested anyRTF will do so
anyGroup :: Parser String
anyGroup = concat <$> sequence [string "{", option "" (string "\\"), option "" (string "*\\"), many1 alphaNum, concat <$> many anyRTF, string "}"]

anyCmd :: Parser String
anyCmd = liftA2 (++) (string "\\") (many1 alphaNum)

anyRTF :: Parser String
anyRTF = anyGroup <|> anyCmd <|> rtfLiteral <|> bareGroup <|> (many space)

-- Stroke left-hand-keys right-hand-keys
data Stroke = Stroke String String | Junk String
              deriving (Read, Show)

data Translation = PlainText String | Fingerspell String | Punctuation String | DelSpace | NewPar (Maybe Int) | Automatic String | SpecialChar SpecialChar | CapNext | CharGroup Char | Stitch String | DelStroke | Unknown String | Ignored
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
lhs :: Parser String
lhs = many (inClass "STKPWHRAO*#0123456789")

rhs :: Parser String
rhs = many (inClass "EUFRPBLGTSDZ*#0123456789")

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
  newstyle = string "\\s" *> (read <$> takeWhile isDigit_w8)

knownCharGroups =   CharGroup <$> (try $ betweenBraces $ inClass "'$|")

bareGroup :: Parser String
bareGroup = try $ betweenBraces rtfLiteral

unknown :: Parser Translation
-- Calling at the end of translation ensures we don't take any known translation term
-- notFollowedBy ensures we don't take the stenoGroup
unknown = (notFollowedBy stenoGroup) *>
          (Unknown <$> (bareGroup <|>
                       anyGroup <|>
                       anyCmd ))

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
              digitalcat <|>
              unknown -- must come last
              )

stenoEntry :: Parser Brief
stenoEntry = liftA2 Brief stenoGroup translation

-- various extensions occur in digitalcat RTF files,
-- W accept them as input, but don't attempt to interpret them
-- this is different from the Unknown Translation
-- in that we know what these mean, and don't need to warn the user that we skip them
digitalcat :: Parser Translation
digitalcat = flags <|> date where
  flags = rtfGroup True "cxsvatdictflags" anyRTF *>| Ignored
  date  = rtfGroup True "cxsvatdictentrydate" (many anyRTF) *>| Ignored

rtfDictionary :: Parser [Brief]
rtfDictionary = many1 stenoEntry

-- scans header content, doesn't use it
dictionaryFile :: Parser [Brief]
dictionaryFile = (rtfGroup False "rtf1" $
                 manyTill anyChar stenoEntry *> rtfDictionary) <* skipWhile isSpace <* endOfInput

-- I think using aeson would just make this more complex
toJSON :: [Brief] -> String
toJSON xs = '{':(briefs xs) where
  briefs (x:xs) = (br x) ++ (briefs xs)
  briefs [] = "}\n"
  br (Brief stks trn) = (quoteS stks) ++ ": " ++ concatMap translation trn ++ ",\n"
  quoteS stks = "\"" ++ (concat $ intersperse "/" $ map hyphenate stks) ++ "\""
  hyphenate (Stroke l r) = l ++ "-" ++ r  -- always inserts hyphen
  translation (PlainText s) = "\"" ++ s ++ "\""
  translation (Fingerspell s) = "\"{&" ++ s ++ "\""
  translation (Punctuation s) = "\"" ++ s ++ "\""

isRight (Left _) = False
isRight (Right _) = True