{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage a = case head . words $ a of
  "I" -> LogMessage Info (read $ (words a)!!1 :: Int) $ unwords . drop 1 . words $ a
  "W" -> LogMessage Warning (read ((words a)!!1) :: Int) $ unwords . drop 1 . words $ a
  "E" -> LogMessage (Error (read ((words a)!!1) :: Int)) (read ((words a)!!1) :: Int) $ unwords . drop 2 . words $ a
  _   -> Unknown a
