{-# LANGUAGE OverloadedStrings #-}

module Plover where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Either
import Types

--I think using aeson would just make this more complex
toJSON :: [Brief] -> Text
toJSON xs = T.concat ["{\n", T.concat $ map brief xs, "}\n"] where
  brief (Brief stks trn) = T.concat [quoteS stks,  ": ",  T.concat $ rights $ map plover trn,  ",\n"]
  quoteS stks = T.concat ["\"", T.intercalate "/" $ map hyphenate stks,  "\""]
  hyphenate (Stroke l r) = T.concat [l,  "-",  r]  -- always inserts hyphen
  hyphenate (Junk t)     = t

plover :: Translation -> Either Text Text
plover (PlainText s) = Right $ T.concat ["\"",  s,  "\""]
plover (Fingerspell s) = Right $ T.concat ["\"{&",  s,  "\""]
plover (Punctuation s) = Right $ T.concat ["\"",  s,  "\""]
plover DelSpace = Right "^" -- should check for adjacent word here?
plover (NewPar _) = Left "skipping unsupported paragraph formatting stroke"
plover (Automatic text) = Right text
plover (SpecialChar NBSP) = Right " " -- These are UTF-8 chars
plover (SpecialChar OptionalHyphen) = Right "-" --"­"
plover (SpecialChar NBHyphen) = Right "-" -- "‑"
plover CapNext = Right "{-|}"
plover (CharGroup cg) = Left $ T.concat ["skipping undocumented group: ", T.singleton cg]
plover (Stitch text) = Right text -- TODO check this with pro stenographers
plover DelStroke = Left "Delete Stroke not yet implemented" -- TODO
plover (Unknown text) = Left $ T.concat ["skipping: ", text]
plover Ignored = Left ""
