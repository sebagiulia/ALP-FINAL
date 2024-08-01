{-# LANGUAGE OverloadedStrings #-}

module TableOperators where

import GHC.Int
import Database.MySQL.Base
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified Data.Text as T
import Data.String
import Data.List

type TableName = String
type Row = [MySQLValue]
type ColumnName = String
type Column = (TableName,ColumnName)
type Table = ([Row],TableName, [Column])

data Value = Col Column | Val MySQLValue
               deriving(Show)

data Condition = And Condition Condition 
               | Or Condition Condition
               | Gr Value Value 
               | Lr Value Value 
               | Greq Value Value 
               | Lreq Value Value 
               | Eq Value Value 
               | Empty
               deriving(Show)

------------------------------- DIVISION ------- Hay que chequear tipos --------------
removeCols :: [Column] -> [Column] -> [Column]
removeCols a b = a \\ b

-- r/s = ΠR−S (r) − ΠR−S {[ΠR−S (r) × s] − r}
--       ---p1---         ---p1---   
--                        ------p2-----
--                        --------p3--------                
--                  -----------p4------------                
divtables :: Table -> Table -> Table
divtables t1@(_,_,acols) t2@(_,_,bcols) = let rest = removeCols acols bcols
                                              p1 = proy rest t1
                                              p2 = pcart p1 t2
                                              p3 = difftables p2 t1
                                              p4 = proy rest p3
                                          in difftables p1 p4 

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

pnat :: Table -> Table -> Table
pnat t1 t2 = let t'@(_, _, cols') = pcart t1 t2
                 cond = prodNatCondition cols'
                 t = sel t' cond
                 cols = combineCols cols'
             in proy cols t

------------------------------- INTERSECCION ----------- Hay que chequear tipos --------------
int :: Table -> Table -> Table 
int t1 t2 = difftables t1 (difftables t1 t2)

------------------------------- RENOMBRAMIENTO -------------------------------------------------
ren :: Table -> TableName -> Table
ren (rs, _, cs) n = (rs, n, cs)

------------------------------- PRODUCTO CARTESIANO ----------- Hay que chequear tipos --------------
bindRows :: Row -> [Row] -> [Row]
bindRows r (r':rs) = ((r ++ r') : (bindRows r rs)) 
bindRows _ [] = [] 

combineRows :: [Row] -> [Row] -> [Row]
combineRows [] _ = []
combineRows (ar:ars) rs = let ars' = bindRows ar rs
                         in ars' ++ (combineRows ars rs)

pcart :: Table -> Table -> Table -- aname y bname tienen que ser distintos
pcart (arows, _, acols) (brows, _, bcols) = let cols = acols ++ bcols
                                                rs = combineRows arows brows
                                            in (rs, "X", cols)

------------------------------- DIFERENCIA  ----------- Hay que chequear tipos --------------
removeRows :: [Row] -> [Row] -> [Row]
removeRows a b = a \\ b 

difftables :: Table -> Table -> Table
difftables (ars, _, acols) (brs, _, _) = let rs = removeRows ars brs
                                         in (rs , "difftables", acols)

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

uni :: Table -> Table -> Table
uni (arows, _, acols) (brows, _, _) = removeEqRows (arows ++ brows, "uni", acols)

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
condition (Gr v1 v2) r cs = getNumber (extractVal (getVal v1 r cs)) > getNumber (extractVal (getVal v2 r cs))  
condition (Lr v1 v2) r cs = getNumber (extractVal (getVal v1 r cs)) < getNumber (extractVal (getVal v2 r cs))  
condition (Eq v1 v2) r cs = extractVal (getVal v1 r cs) == extractVal (getVal v2 r cs)  

sel :: Table -> Condition -> Table
sel ([], name, cols) _ = ([], name, cols)
sel (r:rs, name, cols) cond = let (rs', _, _) = sel (rs, name, cols) cond
                              in if (condition cond r cols) then (r:rs', "sleccion", cols)
                                           else (rs', "sel", cols)    

------------------------------- PROYECCION ---------------------------------------------
-- Devuelve los elementos de la fila correspondiente a las columnas
cutCol :: (Row, [Column]) -> [Column] -> Row
cutCol ((v:vs),(c:cols)) (cc:ccols) = if c == cc then (v: (cutCol (vs, cols) ccols) )
                                                 else cutCol (vs, cols) ccols
cutCol _ _ = []

-- Devuelve ls filas cortadas a partir de las columnas [cols]
cutCols :: ([Row], [Column]) -> [Column] -> [Row]
cutCols ([], _) _ = []
cutCols ((r:rows), tcols) cols = let r' = cutCol (r, tcols) cols
                                     rows' = cutCols (rows, tcols) cols
                                 in (r':rows')

sortCols :: [Column] -> [Column] -> [Column]
sortCols _ [] = []
sortCols cols (c:cs) = if exist c cols then c:(sortCols cols cs)
                                        else sortCols cols cs
                        where exist _ [] = False
                              exist co (col:columns) = if snd co == snd col && 
                                                          (fst co == fst col ||
                                                           fst col == "") then True
                                                                                else exist co columns   
                            
proy :: [Column] -> Table -> Table
proy [] (_, tname, _) = ([], tname, [])  
proy cols (trows, _, tcols) = let sortedCols = (sortCols cols tcols)
                                  trows' = cutCols (trows, tcols) sortedCols
                              in (trows', "proy", sortedCols)
---------------------------------------------------------
extractName' :: MySQLValue -> String
extractName' (MySQLText s) = (T.unpack s) 
extractName' (MySQLInt32 i) = (show i)
extractName' _ = undefined

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



getColumns :: [ColumnDef] -> TableName -> [Column]
getColumns c  n = [show2 (columnName x) | x <- c ]
                where show2 w = (n , init (tail (show w))) 

getTables :: MySQLConn -> [[MySQLValue]] -> IO [(Table, [ColumnDef])] -- > [(table, tname, cnames)]
getTables _ [] = return [] 
getTables conn (l:ls) = do tableName <- extractName (head l)
                           (cdef, is) <- query_ conn (fromString ("select * from " ++ tableName))
                           table <- traduce is
                           tables <- getTables conn ls
                           return (((table, tableName, getColumns cdef tableName), cdef):tables) 

getTableByName :: TableName -> [Table] -> Table
getTableByName _ [] = ([], "undefined",[]) -- Undefined
getTableByName n ((a, tname, b): ts) | n == tname = (a, tname, b)
                                     | otherwise = getTableByName n ts

