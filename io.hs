{-# LANGUAGE NamedFieldPuns #-}

import Text.Read (readEither, readMaybe)

printOrComplain :: (Show a) => Either String a -> IO ()
printOrComplain (Left "Prelude.read: no parse") = putStrLn "argh!! that's not a number"
printOrComplain (Left e) = putStrLn $ "argh!! " ++ e
printOrComplain (Right x) = print x

main :: IO ()
main = do
  print $ curry id "a" "b"
  print $ (,) "a" "b"

-- main :: IO ()
-- main = do
--   input <- getLine
--   let user_num = readEither input :: Either String Int
--   printOrComplain user_num
--   main

-- fn stephen_query<T: FromStr>() -> Result<T, StephenQueryError> {
--   let line = read_line().context(Read)?;
--   line.parse().context(Parse)
-- }

-- #[derive(Debug, Snafu)]
-- enum StephenQueryError {
--   Read(io::Error),
--   Parse(i32::FromStrError)
-- }

-- main :: IO ()
-- main = stephenMain
-- main = User <$> (putStrLn "What's your name?" *> (read <$> getLine)) <*> (putStrLn "What's your favorite number?" *> (read <$> getLine)) <*> (putStrLn "What's your favorite boolean?" *> (read <$> getLine)) >>= print
-- main = do
--   putStrLn "What's your name?"
--   let name = getLine
--    in do
--         putStrLn "What's your favorite number?"
--         let favoriteNumber = read <$> getLine
--          in do
--               putStrLn "What's your favorite boolean?"
--               let favoriteBoolean = read <$> getLine
--                in (User <$> name <*> favoriteNumber <*> favoriteBoolean) >>= print

-- print $ User <$> name <*> favoriteNumber <*> favoriteBoolean
-- (<*) :: Applicative m => m a -> m b -> m a
-- (*>) :: Applicative m => m a -> m b -> m b

-- throw "ERRORR"

-- ^ the above expression evaluates to type Bottom

data User = User {name :: String, favoriteNumber :: Int, favoriteBoolean :: Bool} deriving (Show)

stephenQuery :: Read a => String -> IO a
stephenQuery s = putStrLn s *> (read <$> getLine)

stephenMain :: IO ()
stephenMain =
  User
    <$> (putStrLn "What's your name?" *> getLine)
    <*> stephenQuery "What's your favorite number?"
    <*> stephenQuery "What's your favorite boolean?"
    >>= print

-- main =
--   getLine
--     >>= ( \input ->
--             printOrComplain (readEither input :: Either String Int) >> main
--         )

-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>) :: m a -> m b -> m b