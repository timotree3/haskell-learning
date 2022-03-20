module Concrete where

-- parse :: String -> JsonValue
-- parse = snd . parseExpr

-- -- Takes a String beginning with a JSON expression, and returns the rest of the string and the parsed expression.
-- parseExpr :: String -> (String, JsonValue)
-- parseExpr ('{' : s) = let (tail, obj) = parseObject s in (tail, Object obj)
-- parseExpr ('[' : s) = let (tail, arr) = parseArray s in (tail, Array arr)
-- parseExpr ('"' : s) = let (tail, string) = parseString s in (tail, String string)
-- parseExpr ('t' : 'r' : 'u' : 'e' : s) = (s, Boolean True)
-- parseExpr ('f' : 'a' : 'l' : 's' : 'e' : s) = (s, Boolean False)
-- parseExpr ('n' : 'u' : 'l' : 'l' : s) = (s, Null)
-- -- Otherwise, assume it's a number
-- parseExpr s = let (tail, n) = parseNumber s in (tail, Number n)

-- parseObject :: String -> (String, Map String JsonValue)
-- parseObject ('}' : s) = (s, Map.empty)
-- parseObject s =
--   let (s, key) = parseString s
--    in let (s, value) = parseExpr (eatColon s)
--        in let (s, restOfObj) = parseObject (eatComma s)
--            in (s, Map.insert key value restOfObj)

-- parseArray :: String -> (String, [JsonValue])
-- parseArray (']' : s) = (s, [])
-- parseArray s =
--   let (s, elem) = parseExpr s
--    in let (s, restOfArr) = parseArray (eatComma s)
--        in (s, elem : restOfArr)

-- parseString :: String -> (String, String)
-- parseString ('"' : s) = (s, "")
-- parseString (c : s) =
--   let (s, restOfString) = parseString s
--    in (s, c : restOfString)

-- parseNumber :: String -> (String, Double)
-- parseNumber s =
--   let (number, s) = splitAt (longestPrefixWhere isNumber s) s
--    in (s, read number)

-- isNumber :: String -> Bool
-- isNumber s = case readEither s of
--   Left _ -> false
--   Right _ -> true

-- longestPrefixWhere :: (a -> Bool) -> [a] -> Int
-- longestPrefixWhere _ [] = 0
-- longestPrefixWhere f (x : xs) = if f x then 1 + (longestPrefixWhere f xs) else 0

-- -- splitAt :: Int -> String -> (String, String)
-- -- splitAt 0 s = ("", s)
-- -- splitAt n (c : s) = let (left, right) = splitAt (n - 1) s in (c : left, right)

-- eatColon :: String -> String
-- eatColon (':' : s) = s

-- eatComma :: String -> String
-- eatComma (',' : s) = s

-- parseBool = do
--   matched_string <- string "true" <|> string "false"
--   pure $ if matched_string == "true" then Boolean True else Boolean False

-- mytoken x =
--   token showTok posFromTok testTok
--   where
--     showTok (pos, t) = show t
--     posFromTok (pos, t) = pos
--     testTok (pos, t) = if x == t then Just t else Nothing

-- class Orderable Equal k => Map m where
--     empty :: m
--     insert :: k -> v -> m -> m
--     get :: (m -> k -> v)
--     remove :: k -> m -> m
