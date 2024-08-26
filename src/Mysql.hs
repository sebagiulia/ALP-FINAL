module Mysql where

import TableOperators
import Common
import Error

import Database.MySQL.Base
import Data.String
import qualified System.IO.Streams as Streams
import qualified Data.Word as W
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Control.Exception (SomeException, Exception, toException, catch, IOException, throw)


getDBData :: ConnWords -> Either String ConnectInfo
getDBData = getDBData' defaultConnectInfo

getDBData' :: ConnectInfo -> ConnWords -> Either String ConnectInfo
getDBData' c (w:ws) = case w of
                        LHost (LString s) -> getDBData' (c {ciHost = s}) ws
                        LPort (LNum n) -> getDBData' (c {ciPort = fromIntegral (read n :: W.Word16) }) ws
                        LDb (LString s) -> getDBData' (c {ciDatabase = (B.pack s)}) ws
                        LUser (LString s) -> getDBData' (c {ciUser = (B.pack s)}) ws
                        LPw (LString s) -> getDBData' (c {ciPassword = (B.pack s)}) ws
                        _ -> Left "Parametro de conexion desconocido\n"
getDBData' c [] = Right c

traduce :: Streams.InputStream a -> IO [a]
traduce = Streams.toList

convertToTableValues :: [[MySQLValue]] -> [TableRow]
convertToTableValues [] = []
convertToTableValues (r:rs) = let newRow = foldr makerow [] r
                                  restRows = convertToTableValues rs
                              in newRow : restRows
                              where makerow v ls = mySQLValueToTableValue v : ls  

mySQLValueToTableValue :: MySQLValue -> TableValue
mySQLValueToTableValue (MySQLText  t) = Str (T.unpack t)
mySQLValueToTableValue (MySQLInt32 n) = Numb (fromIntegral n)
mySQLValueToTableValue n = Str (show n) -- No deberia entrar


getColumns :: [ColumnDef] -> TableName -> [Column]
getColumns c  n = [show2 (columnName x) | x <- c ]
                where show2 w = ([n] , init (tail (show w)))

getTables :: MySQLConn -> [[MySQLValue]] -> IO [((TableName, Table), [ColumnDef])] -- > [(table, tname, cnames)]
getTables _ [] = return []
getTables conn (l:ls) = do let tableName = getName (head l)
                           (cdef, is) <- query_ conn (fromString ("select * from " ++ tableName))
                           table <- traduce is
                           let table' = convertToTableValues table
                           tables <- getTables conn ls
                           return (((tableName, (table', getColumns cdef tableName)), cdef):tables)
                        where getName (MySQLText t) = T.unpack t
                              getName _             = "invalid"


mysqlconn :: ConnectInfo -> GlobalE -> IO GlobalE
mysqlconn inf e = do conn <- connect inf
                     (_, is) <- query_ conn (fromString "show tables")
                     rows <- traduce is
                     tables <- getTables conn rows -- [ Table ]
                     case foldl repeated (Right []) tables of
                      Right ts -> do let st = convertToEnv e ts
                                     return st
                      Left err -> throw $ Error $ "Tabla existente: " ++ err
        where repeated (Left err) _ = Left err
              repeated (Right tbs) t@((n,_),_) = case lookup n e of
                                                    Nothing -> Right $ t:tbs
                                                    _       -> Left n

convertToEnv ::  GlobalE -> [((TableName, Table), [ColumnDef])] -> GlobalE
convertToEnv e [] = e
convertToEnv e (((name,t@(rows, cols)), cts):ts) = case lookup name e of
                                        Just _ -> convertToEnv e ts
                                        Nothing -> let typ = (name, getType cts cols)
                                                   in ((name, (t, typ)):convertToEnv e ts)
                                        where getType [] _ = []
                                              getType (c:cts) (col:cols) = case columnType c of
                                                                              t -> if t == mySQLTypeLong then (col, IntT):getType cts cols
                                                                                   else (col, StrT):getType cts cols

