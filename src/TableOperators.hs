{-# LANGUAGE OverloadedStrings #-}

module TableOperators where

import Data.String
import Data.List

type TableName = String
data TableValue = Str String | Numb Int deriving (Show, Eq)
type TableRow = [TableValue]
type ColumnName = String
type Column = (TableName,ColumnName)
type Table = ([TableRow],TableName, [Column])

data Value = Col Column | Val TableValue
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

--------------------------------------------------------------------------------------
tableValueToString :: TableValue -> String
tableValueToString (Str s) = s
tableValueToString (Numb i) = show i

------------------------------- DIVISION ------- Hay que chequear tipos --------------
removeEquals :: Table -> Table
removeEquals (rs, n, cs) = (nub rs, n, cs) 

removeCols :: [Column] -> [Column] -> [Column]
removeCols [] b = []
removeCols (ac:acs) b = if exist ac b then removeCols acs b
                                       else ac:removeCols acs b
                      where exist _ [] = False
                            exist co (col:columns) = snd co == snd col || exist co columns

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
                                          in removeEquals $ difftables p1 p4

{-

104 1 -> 104 1 - 104 1 
104 2 -> 104 1

-}

------------------------------- PRODUCTO NATURAL ------- Hay que chequear tipos --------------
combineCols :: [Column] -> [Column]
combineCols [] = []
combineCols (c: cs) = let (_, rest) = lookFor (snd c) cs
                      in (c: combineCols rest)
                      where lookFor _ [] = ([],[])
                            lookFor n (col:cols) = let (eq', rest') = lookFor n cols
                                                   in if snd col == n then (col:eq', rest')
                                                                      else (eq', col:rest')

-- Funcion que genera el arbol de condicion para producto natural.
prodNatCondition :: [Column] -> Condition
prodNatCondition [] = Empty
prodNatCondition (c:cols) = let (sames, rest) = lookFor (snd c) cols -- Busco las columnas con el mismo nombre
                                cond = equals c sames -- Genero el arbol de condicion
                            in And cond (prodNatCondition rest)
                            where lookFor _ [] = ([],[])
                                  lookFor n (col:cs) = let (eq', rest') = lookFor n cs
                                                        in if snd col == n then (col:eq', rest')
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
ren (rs, _, cs) n = let ncs = renCols n cs
                    in (rs, n, ncs)
                    where renCols n [] = []
                          renCols n (c:cs') = (n, snd c):renCols n cs'

------------------------------- PRODUCTO CARTESIANO ----------- Hay que chequear tipos --------------
bindRows :: TableRow -> [TableRow] -> [TableRow]
bindRows r (r':rs) = ((r ++ r') : (bindRows r rs))
bindRows _ [] = []

combineRows :: [TableRow] -> [TableRow] -> [TableRow]
combineRows [] _ = []
combineRows (ar:ars) rs = let ars' = bindRows ar rs
                         in ars' ++ (combineRows ars rs)

pcart :: Table -> Table -> Table -- aname y bname tienen que ser distintos
pcart (arows, _, acols) (brows, _, bcols) = let cols = acols ++ bcols
                                                rs = combineRows arows brows
                                            in (rs, "X", cols)

------------------------------- DIFERENCIA  ----------- Hay que chequear tipos --------------
existRow :: [Column] -> TableRow -> [Column] -> [TableRow] -> Bool
existRow acols a bcols brs = let ar' = zip acols a
                                 brs' = map (zip bcols) brs
                             in searchIn ar' brs'
                             where searchIn _ [] = False
                                   searchIn arow (brow:bs') = compareRows arow brow || searchIn arow bs'
                                   compareRows [] _ = True
                                   compareRows (a': as') brow = case lookup (snd (fst a')) (map (\(c, v) -> (snd c, v)) brow) of
                                                                  Nothing -> False
                                                                  Just av -> av == snd a' && compareRows as' brow


removeRows :: [Column] -> [TableRow] -> [Column] -> [TableRow] -> [TableRow]
removeRows acols [] bcols b = []
removeRows acols (a:ars) bcols brs = if existRow acols a bcols brs
                                     then removeRows acols ars bcols brs
                                     else a:removeRows acols ars bcols brs


difftables :: Table -> Table -> Table
difftables (ars, _, acols) (brs, _, bcols) = let rs = removeRows acols ars bcols brs
                                         in (rs , "difftables", acols)

------------------------------- UNION  ----------- Hay que chequear tipos --------------
compareRow :: TableRow -> TableRow -> Bool
compareRow (v:vs) (v':vs') = (extractVal v) == (extractVal v') && compareRow vs vs'
compareRow _ _ = True

removeRow :: TableRow -> [TableRow] -> [TableRow]
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

extractVal :: TableValue -> Either Int String
extractVal (Numb i) = Left i
extractVal (Str t) = Right t

getVal :: Value -> TableRow -> [Column] -> Either Bool TableValue
getVal (Val s) _ _ = Right s
getVal (Col var) (r:rs) (c:cs) = if snd var == snd c && ( fst var == fst c || fst c == "") then Right r
                                 else getVal (Col var) rs cs
getVal _ [] _ = Left False
getVal _ _ [] = Left False

getNumber :: Either Int String -> Either Bool Int
getNumber (Left i) = Right i
getNumber _ = Left False


-- cond: = > < val or variable
condition :: Condition -> TableRow -> [Column] -> Bool
condition Empty _ _ = True
condition (And c1 c2) r cs = (condition c1 r cs) && (condition c2 r cs)
condition (Or c1 c2) r cs = (condition c1 r cs) || (condition c2 r cs)
condition (Gr v1 v2) r cs = case (getVal v1 r cs) of
                                Right a -> case (getVal v2 r cs) of
                                                Right b -> case getNumber (extractVal a) of
                                                                Right a' -> case getNumber (extractVal b) of
                                                                                Right b' -> a' > b'
                                                                                _ -> False
                                                                _ -> False
                                                _ -> False
                                _ -> False
condition (Greq v1 v2) r cs = case getVal v1 r cs of
                                Right a -> case getVal v2 r cs of
                                                Right b -> case getNumber (extractVal a) of
                                                                Right a' -> case getNumber (extractVal b) of
                                                                                Right b' -> a' >= b'
                                                                                _ -> False
                                                                _ -> False
                                                _ -> False
                                _ -> False
condition (Lr v1 v2) r cs = case getVal v1 r cs of
                                Right a -> case getVal v2 r cs of
                                                Right b -> case getNumber (extractVal a) of
                                                                Right a' -> case getNumber (extractVal b) of
                                                                                Right b' -> a' < b'
                                                                                _ -> False
                                                                _ -> False
                                                _ -> False
                                _ -> False
condition (Lreq v1 v2) r cs = case getVal v1 r cs of
                                Right a -> case getVal v2 r cs of
                                                Right b -> case getNumber (extractVal a) of
                                                                Right a' -> case getNumber (extractVal b) of
                                                                                Right b' -> a' <= b'
                                                                                _ -> False
                                                                _ -> False
                                                _ -> False
                                _ -> False
condition (Eq v1 v2) r cs = case getVal v1 r cs of
                                Right a -> case getVal v2 r cs of
                                                Right b -> extractVal a == extractVal b
                                                _ -> True
                                _ -> False

sel :: Table -> Condition -> Table
sel ([], name, cols) _ = ([], name, cols)
sel (r:rs, name, cols) cond = let (rs', _, _) = sel (rs, name, cols) cond
                              in if condition cond r cols then (r:rs', "sleccion", cols)
                                           else (rs', "sel", cols)

------------------------------- PROYECCION ---------------------------------------------
-- Devuelve los elementos de la fila correspondiente a las columnas
cutCol :: (TableRow, [Column]) -> [Column] -> TableRow
cutCol (v:vs,c:cols) (cc:ccols) = if  snd c == snd cc &&
                                          (fst c == fst cc ||
                                           fst cc == "") then v: cutCol (vs, cols) ccols
                                                         else cutCol (vs, cols) (cc:ccols)
cutCol _ _ = []

-- Devuelve ls filas cortadas a partir de las columnas [cols]
cutCols :: ([TableRow], [Column]) -> [Column] -> [TableRow]
cutCols ([], _) _ = []
cutCols (r:rows, tcols) cols = let r' = cutCol (r, tcols) cols
                                   rows' = cutCols (rows, tcols) cols
                               in (r':rows')

sortCols :: [Column] -> [Column] -> [Column]
sortCols _ [] = []
sortCols cols (c:cs) = if exist c cols then c:sortCols cols cs
                                        else sortCols cols cs
                        where exist _ [] = False
                              exist co (col:columns) = (snd co == snd col &&
                                                          (fst co == fst col ||
                                                           fst col == "")) || exist co columns

proy :: [Column] -> Table -> Table
proy [] (_, tname, _) = ([], tname, [])
proy cols (trows, _, tcols) = let sortedCols = sortCols cols tcols
                                  trows' = cutCols (trows, tcols) sortedCols
                              in (trows', "proy", sortedCols)
---------------------------------------------------------