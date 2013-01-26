{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Prelude hiding (takeWhile)
import Data.Attoparsec.Text
import Control.Applicative
import Data.Monoid (mconcat, mappend)
import Control.Monad
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (isAlpha, isDigit, isSpace)
import Types

(*>|) :: Applicative f => f a -> b -> f b
(*>|) a b  = a *> (pure b)

betweenBraces :: Parser a -> Parser a
betweenBraces p = char '{' *> p <* char '}'

-- another function from Parsec not included in attoparsec
optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = Just <$> p <|> pure Nothing

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

specialChar :: Parser SpecialChar
specialChar = rtfCmdChar '~' *>| NBSP <|>
              rtfCmdChar '-' *>| OptionalHyphen <|>
              rtfCmdChar '_' *>| NBHyphen

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
