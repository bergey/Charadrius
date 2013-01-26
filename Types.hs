module Types where

import Data.Text (Text)

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

data Brief = Brief [Stroke] [Translation]
             deriving (Read, Show)
