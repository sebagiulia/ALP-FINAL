{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.Int
import Database.MySQL.Base
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified Data.Text as T
import Data.String
import Data.List

type Name = String
type Column = (Name, Name)
type Row = [MySQLValue]
type Table = ([Row], Name, [Column])
data Value = Col Column | Val MySQLValue

data Condition = And Condition Condition | Or Condition Condition
                 | Gt Value Value | Lr Value Value | Eq Value Value | Empty


------------------------------- DIVISION ------- Hay que chequear tipos --------------
removeCols :: [Column] -> [Column] -> [Column]
removeCols a b = a \\ b

-- r/s = ΠR−S (r) − ΠR−S {[ΠR−S (r) × s] − r}
--       ---p1---         ---p1---   
--                        ------p2-----
--                        --------p3--------                
--                  -----------p4------------                
division :: Table -> Table -> Table
division t1@(_,_,acols) t2@(_,_,bcols) = let rest = removeCols acols bcols
                                             p1 = proyeccion rest t1
                                             p2 = prodCartesiano p1 t2
                                             p3 = diferencia p2 t1
                                             p4 = proyeccion rest p3
                                         in diferencia p1 p4 

------------------------------- PRODUCTO NATURAL ------- Hay que chequear tipos --------------
combineCols :: [Column] -> [Column]
combineCols [] = []
combineCols (c: cs) = let (_, rest) = lookFor (snd c) cs
                      in (c: combineCols rest)
                      where lookFor _ [] = ([],[])
                            lookFor n (col:cols) = let (eq', rest') = lookFor n cols
                                                   in if snd col == n then ((col:eq'), rest')
                                                                      else (eq', col:rest')

-- Funcion que genera el arbol de condicion para producto natural.
prodNatCondition :: [Column] -> Condition
prodNatCondition [] = Empty 
prodNatCondition (c:cols) = let (sames, rest) = lookFor (snd c) cols -- Busco las columnas con el mismo nombre
                                cond = equals c sames -- Genero el arbol de condicion
                            in And cond (prodNatCondition rest)
                            where lookFor _ [] = ([],[])
                                  lookFor n (col:cs) = let (eq', rest') = lookFor n cs
                                                        in if snd col == n then ((col:eq'), rest')
                                                                           else (eq', col:rest')
                                  equals _ [] = Empty
                                  equals col (same:ss) = And (Eq (Col col) (Col same)) (equals col ss) 

prodNatural :: Table -> Table -> Table
prodNatural t1 t2 = let t'@(_, _, cols') = prodCartesiano t1 t2
                        cond = prodNatCondition cols'
                        t = seleccion t' cond
                        cols = combineCols cols'
                    in proyeccion cols t

------------------------------- INTERSECCION ----------- Hay que chequear tipos --------------
interseccion :: Table -> Table -> Table 
interseccion t1 t2 = diferencia t1 (diferencia t1 t2)

------------------------------- RENOMBRAMIENTO -------------------------------------------------
renombramiento :: Table -> Name -> Table
renombramiento (rs, _, cs) n = (rs, n, cs)

------------------------------- PRODUCTO CARTESIANO ----------- Hay que chequear tipos --------------
bindRows :: Row -> [Row] -> [Row]
bindRows r (r':rs) = ((r ++ r') : (bindRows r rs)) 
bindRows _ [] = [] 

combineRows :: [Row] -> [Row] -> [Row]
combineRows [] _ = []
combineRows (ar:ars) rs = let ars' = bindRows ar rs
                         in ars' ++ (combineRows ars rs)

prodCartesiano :: Table -> Table -> Table -- aname y bname tienen que ser distintos
prodCartesiano (arows, _, acols) (brows, _, bcols) = let cols = acols ++ bcols
                                                         rs = combineRows arows brows
                                                     in (rs, "X", cols)

------------------------------- DIFERENCIA  ----------- Hay que chequear tipos --------------
removeRows :: [Row] -> [Row] -> [Row]
removeRows a b = a \\ b 

diferencia :: Table -> Table -> Table
diferencia (ars, _, acols) (brs, _, _) = let rs = removeRows ars brs
                                         in (rs , "diferencia", acols)

------------------------------- UNION  ----------- Hay que chequear tipos --------------
compareRow :: Row -> Row -> Bool
compareRow (v:vs) (v':vs') = (extractVal v) == (extractVal v') && compareRow vs vs'
compareRow _ _ = True

removeRow :: Row -> [Row] -> [Row]
removeRow _ [] = []
removeRow r (r':rs) = if compareRow r r' then removeRow r rs
                                          else (r': removeRow r rs)

removeEqRows :: Table -> Table
removeEqRows ((r:rs), name, cols) = let rs' = removeRow r rs 
                                        (rs'', _, _) = removeEqRows (rs', name, cols)
                                    in (r:rs'', name, cols)
removeEqRows t = t 

union :: Table -> Table -> Table
union (arows, _, acols) (brows, _, _) = removeEqRows (arows ++ brows, "union", acols)

------------------------------- SELECCION  ----------- Hay que chequear tipos --------------

extractVal :: MySQLValue -> Either Int32 T.Text
extractVal (MySQLInt32 i) = Left i
extractVal (MySQLText t) = Right t
extractVal _ = undefined

getVal :: Value -> Row -> [Column] -> MySQLValue
getVal (Val s) _ _ = s
getVal _ [] _ = undefined
getVal _ _ [] = undefined
getVal (Col var) (r:rs) (c:cs) = if var == c then r
                                 else getVal (Col var) rs cs

getNumber :: Either Int32 T.Text -> Int32
getNumber (Left i) = i
getNumber _ = undefined


-- cond: = > < val or variable
condition :: Condition -> Row -> [Column] -> Bool 
condition Empty _ _ = True  
condition (And c1 c2) r cs = (condition c1 r cs) && (condition c2 r cs)  
condition (Or c1 c2) r cs = (condition c1 r cs) || (condition c2 r cs)  
condition (Gt v1 v2) r cs = getNumber (extractVal (getVal v1 r cs)) > getNumber (extractVal (getVal v2 r cs))  
condition (Lr v1 v2) r cs = getNumber (extractVal (getVal v1 r cs)) < getNumber (extractVal (getVal v2 r cs))  
condition (Eq v1 v2) r cs = extractVal (getVal v1 r cs) == extractVal (getVal v2 r cs)  

seleccion :: Table -> Condition -> Table
seleccion ([], name, cols) _ = ([], name, cols)
seleccion (r:rs, name, cols) cond = let (rs', _, _) = seleccion (rs, name, cols) cond
                                    in if (condition cond r cols) then (r:rs', "sleccion", cols)
                                                 else (rs', "seleccion", cols)    

------------------------------- PROYECCION ---------------------------------------------
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

sortCols :: [Column] -> [Column] -> [Column]
sortCols _ [] = []
sortCols cols (c: cs) = if exist c cols then c:(sortCols cols cs)
                                        else sortCols cols cs
                        where exist _ [] = False
                              exist co (col:columns) = if co == col then True
                                                                    else exist co columns   
                            
proyeccion :: [Column] -> Table -> Table
proyeccion [] (_, tname, _) = ([], tname, [])  
proyeccion cols (trows, _, tcols) = let trows' = cutCols (trows, tcols) (sortCols cols tcols)
                                   in (trows', "proyeccion", cols)
---------------------------------------------------------
extractName :: MySQLValue -> IO String
extractName (MySQLText s) = return (T.unpack s) 
extractName (MySQLInt32 i) = return (show i)
extractName _ = undefined

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



getColumns :: [ColumnDef] -> Name -> [Column]
getColumns c  n = [show2 (columnName x) | x <- c ]
                where show2 w = (n , init (tail (show w))) 

getTables :: MySQLConn -> [[MySQLValue]] -> IO [Table] -- > [(table, tname, cnames)]
getTables _ [] = return [] 
getTables conn (l:ls) = do tableName <- extractName (l!!0)
                           (c, is) <- query_ conn (fromString ("select * from " ++ tableName))
                           table <- traduce is
                           tables <- getTables conn ls
                           return ((table, tableName, getColumns c tableName):tables) 

getTableByName :: Name -> [Table] -> Table
getTableByName _ [] = ([], "undefined",[]) -- Undefined
getTableByName n ((a, tname, b): ts) | n == tname = (a, tname, b)
                                     | otherwise = getTableByName n ts

arSql :: IO ()
arSql = do
  conn <-
    connect
      defaultConnectInfo {ciUser = "ar-sql-user", ciPassword = "ar-sql-password", ciDatabase = "alp_final"}
  (_, is) <- query_ conn "show tables"
  rows <- traduce is -- Nombre de tablas :: [MySQLText nombreTabla]
  tables <- getTables conn rows -- tablas :: [([[MySQLValue]], String)]
  printTables tables
  printTables [(diferencia (getTableByName "Proyectos" tables) (seleccion (getTableByName "Proyectos" tables) (Eq (Col ("Proyectos", "proyecto_id")) (Val (MySQLInt32 105)))))]



main :: IO Int
main = do
  putStrLn "Hello, Haskell!"
  arSql
  return 0