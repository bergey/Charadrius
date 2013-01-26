{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as T
import Data.Text.Encoding
import qualified Data.ByteString as BS
import Data.Text.Encoding.Error (lenientDecode)
import System.Environment
import Data.List (isSuffixOf)
import Data.Attoparsec.Text (parseOnly)
import Parser
import Plover

-- file handling section
jsonExtension :: String -> String
jsonExtension fn = if isSuffixOf ".rtf" fn
                   then (reverse . (drop 4) . reverse) fn ++ ".json"
                   else fn ++ ".json"

main :: IO ()
main = do
       files <- getArgs
       mapM convertFile files
       return ()

convertFile :: String -> IO ()
convertFile filename = do
  bs <- BS.readFile filename
  let utf = decodeUtf8With lenientDecode bs
  case parseOnly dictionaryFile utf of
       (Right r) -> T.writeFile (jsonExtension filename) (toJSON r)
       (Left e) -> putStrLn e
