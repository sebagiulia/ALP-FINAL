{-# LANGUAGE OverloadedStrings #-}
module PrettyPrinter
  ( printTable,
    printType,
    printTerm
       -- pretty printer para terminos
  )
where

import Data.List (transpose)
import           TableOperators
import           Common
import           Text.PrettyPrint.HughesPJ
import           Prelude                 hiding ( (<>) )

------------  



-- Function to convert list of strings to a pretty row
prettyRow :: [String] -> [Int] -> Doc
prettyRow values widths = hsep $ zipWith pad values widths

-- Function to pad each cell to a given width
pad :: String -> Int -> Doc
pad str n = text str <> text (replicate (n - length str) ' ')

-- Function to calculate the maximum width of each column
columnWidths :: [[String]] -> [Int]
columnWidths rows = map maximum (transpose (map (map length) rows))

-- Function to convert list of rows to a pretty table
prettyTable :: [[String]] -> Doc
prettyTable rows =
  let widths = columnWidths rows
  in vcat (map (`prettyRow` widths) rows)

------------
pp :: Table -> Doc
pp (rows, tname, cols) = let colsList =  concatMap fc cols :: [String]
                             rowsList = map (map tableValueToString) rows :: [[String]]
                         in prettyTable $ colsList : rowsList
                         where fc l = if length (fst l) > 1
                                      then map (\t -> t ++ "." ++ snd l) (fst l)
                                      else [snd l]
printTable :: Table -> Doc
printTable = pp

printType :: TableType -> Doc
printType (_, cs) = printType' cs
  where printType' [] = text ""
        printType' ((c,t):cols) = text (snd c) <> text ": " <> ptype t
                                  <> text "\n" <>
                                  printType' cols
        ptype StrT = text "String"
        ptype IntT = text "Integer"

printTerm :: Term -> Doc
printTerm t = text $ show t

