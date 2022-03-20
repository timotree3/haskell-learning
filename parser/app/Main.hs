module Main where

import Data.Char
import Lib

main :: IO ()
main = do
  print $ runParser (char 'a') "abcd" -- Just ('a', "bcd")
  print $ runParser (ord <$> char 'a') "abcd" -- Just (97, "bcd")
  print $
    runParser
      ( (:) <$> char 'a'
          <*> ((:) <$> char 'b' <*> pure [])
      )
      "abcd" -- Just ("ab", "cd")
  print $ runParser (string "ab") "abcd"
  print $ runParser jsonValue "false"
  print $ runParser jsonValue "true"
  print $ runParser jsonValue "null"
  print $ runParser jsonValue "\"foo\""
  print $ runParser jsonValue "[]"
  print $ runParser jsonValue "[,]"
  print $ runParser jsonValue "[\"foo\",false,]"
  print $ runParser jsonValue "[\"fo,o\",false,]"
  print $ runParser jsonValue "[null]"
  print $ runParser jsonValue "[null,]"
  print $ runParser jsonValue "[\"foo\",false]"
  print $ runParser jsonValue "[\"fo,o\",false]"
  print $ runParser jsonValue "{\"a\":{\"b\":4}}"
  print $ runParser jsonValue "-540"
  print $ runParser jsonValue "-540.456"
  print $ runParser jsonValue "-0.0"
  print $ runParser jsonValue "0.0"
  print $ runParser jsonValue "00.00"
  print $ runParser jsonValue "1.-1"
  print $
    runParser
      jsonValue
      "\n\
      \  \n\
      \  {\n\
      \    \"key\"         \n\
      \    \r\n\
      \    : \"v a  l u e\"}"
  print $
    runParser
      jsonValue
      "\"a\\\"b\\\\c\""