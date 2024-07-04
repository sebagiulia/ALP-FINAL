{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.MySQL.Base
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified Data.Text as T
import Data.String

type Column = String
type Name = String
type Row = [MySQLValue]
type Table = ([Row], Name, [Column])


-- Devuelve los elementos de la fila correspondiente a las columnas
cutCol :: (Row, [Column]) -> [Column] -> Row
cutCol _ [] = []
cutCol (_, []) _  = []
cutCol ([], _) _ = []
cutCol ((v:vs),(c:cols)) (cc:ccols) = if c == cc then (v: (cutCol (vs, cols) ccols) )
                                                 else cutCol (vs, cols) ccols

-- Devuelve ls filas cortadas a partir de las columnas [cols]
cutCols :: ([Row], [Column]) -> [Column] -> [Row]
cutCols ([], _) _ = []
cutCols ((r:rows), tcols) cols = let r' = cutCol (r, tcols) cols
                                     rows' = cutCols (rows, tcols) cols
                                 in (r':rows')

                            
proyeccion :: [Column] -> Table -> Table
proyeccion [] (_, tname, _) = ([], tname, [])  
proyeccion cols (trows, _, tcols) = let trows' = cutCols (trows, tcols) cols -- CS TIENE QUE ESTAR ORDENADO
                                   in (trows', "+", cols)
---------------------------------------------------------

extractName :: MySQLValue -> IO String
extractName (MySQLText s) = return (T.unpack s) 
extractName (MySQLInt32 i) = return (show i)

printLine :: Row -> IO [String]
printLine [] = return []
printLine (l:ls) = do v <- extractName l
                      vs <- printLine ls
                      return (v:vs)          

printLines :: [Row] -> IO ()
printLines [] = putStrLn ""
printLines (l:ls) = do line <- printLine l
                       print line
                       printLines ls
                   
printTables :: [Table] -> IO ()
printTables [] = putStrLn ""
printTables ((t, n, cs):ts) = do putStrLn ("Tabla: " ++ n)
                                 print cs
                                 printLines t
                                 printTables ts


traduce :: InputStream a -> IO [a] 
traduce stream = Streams.toList stream



getColumns :: [ColumnDef] -> [String]
getColumns c = [show (columnName x) | x <- c ]

getTables :: MySQLConn -> [[MySQLValue]] -> IO [Table] -- > [(table, tname, cnames)]
getTables _ [] = return [] 
getTables conn (l:ls) = do tableName <- extractName (l!!0)
                           (c, is) <- query_ conn (fromString ("select * from " ++ tableName))
                           table <- traduce is
                           tables <- getTables conn ls
                           return ((table, tableName, getColumns c):tables) 

arSql :: IO ()
arSql = do
  conn <-
    connect
      defaultConnectInfo {ciUser = "ar-sql-user", ciPassword = "ar-sql-password", ciDatabase = "alp_final"}
  (_, is) <- query_ conn "show tables"
  rows <- traduce is -- Nombre de tablas :: [MySQLText nombreTabla]
  tables <- getTables conn rows -- tablas :: [([[MySQLValue]], String)]
  printTables tables



main :: IO Int
main = do
  putStrLn "Hello, Haskell!"
  arSql
  return 0