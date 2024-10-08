module Csv where

import TableOperators
import Common

import Data.List
import Data.Csv (encode, decode, HasHeader(NoHeader))
import qualified Data.Vector as V
import Data.ByteString.Lazy.Internal as B ( ByteString )
import  Data.Text (Text, pack, unpack, strip)
import qualified Data.Text as T

-- Funciones para manejar la importacion/exportacion de archivos y transferir los datos en el formato adecuado.

tableToCsv :: Table -> String
tableToCsv (rows, cols) = let tcols = intercalate "," (concatMap (\(ts, c) -> (map (\t -> t ++ "." ++ c) ts)) cols)
                              trows = map (intercalate "," . map tableValueToString) rows
                          in intercalate "\r\n" (tcols:trows)

csvToTable :: TableName -> B.ByteString -> Either String GlobalE
csvToTable name csvData = case decode NoHeader csvData of
                        Right rows -> if V.null rows ||V.null (V.tail rows)
                                      then Left "Tabla vacía."
                                      else let (cols, rows', typ) = processRows (V.toList rows)
                                           in if length cols /= length typ then Left "Tabla inconsistente."
                                              else case rows' of
                                                      Right rs -> let cols' = map (\c -> ([name], unpack c)) cols
                                                                  in Right [(name, ((rs, cols'), (name, zip cols' typ)))]
                                                      Left err ->Left err
                        Left err -> Left err


processRows :: [[String]] -> ([Text], Either String [TableRow], [Type])
processRows [] = ([], Right [], [])
processRows (header:rest) = let typ = getType (head rest)
                            in (map (strip . pack) header, foldr (processRow typ) (Right []) rest, typ)
  where
    processRow :: [Type] -> [String] -> Either String [TableRow] -> Either String [TableRow]
    processRow t r rs = case rs of
                          Right rows -> if length r /= length t
                                        then Left "Faltan datos en tabla."
                                        else case convertToRowValues t r of
                                              Right nrow -> Right (nrow:rows)
                                              Left err -> Left err
                          err -> err
    toType s = case reads s :: [(Int, String)]of
                [(val, "")] -> IntT
                _           -> StrT
    getType = map toType

convertToRowValues :: [Type] -> [String] -> Either String TableRow
convertToRowValues t r = foldr convertToTableValue (Right [])  $ zip t r

convertToTableValue :: (Type, String) ->  Either String TableRow  -> Either String TableRow
convertToTableValue _ (Left e) = Left e
convertToTableValue (t, s) (Right r) =
    case reads s of
        [(val, "")] -> case t of
                        StrT -> Left $ "Tabla inconsistente: \"" ++ s ++ "\" en columna de tipo StrT."
                        _ -> if length s > 9 
                             then Left $ "No se pueden almacenar numeros de mas de 9 digitos como IntT: " ++ s ++  "." -- Numero muy grande
                             else Right $ Numb val:r
        _           -> case t of
                        IntT -> Left $ "Tabla inconsistente: \"" ++ s  ++ "\" en casilla de tipo StrT ."
                        _ -> case reads (unpack (T.tail (strip (pack s)))) :: [(Int, String)] of
                              [(val, "")] -> if T.head (strip (pack s)) == '\\'
                                             then Right $ Str (unpack(T.tail (strip (pack s)))):r
                                             else Right $ Str (unpack(strip (pack s))):r
                              _           -> Right $ Str (unpack(strip (pack s))):r