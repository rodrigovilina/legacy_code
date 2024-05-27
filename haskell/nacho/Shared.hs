module Shared where

askFloat msg = do
  putStr msg
  readLn ::IO Float

askInt msg = do
  putStr msg
  readLn :: IO Int

askWord msg = do
  putStr msg
  getLine ::IO String

