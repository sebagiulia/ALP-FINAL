{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Error where

import Control.Exception

newtype Error = Error String deriving (Show)
instance Exception Error

error :: String -> IO ()
error s = putStrLn $ "Error: " ++ s

typeError :: String -> IO ()
typeError s = putStrLn $ "Error de tipos: " ++ s

exError :: SomeException -> IO ()
exError e = case fromException e of 
              Just (Error s) -> putStrLn $ "Error: " ++ s
              _ -> do {putStr "Error de excepcion: ";print e }

msg :: String -> IO ()
msg = putStrLn