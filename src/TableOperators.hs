{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}

module TableOperators where

import Data.String
import Data.List
import Data.Function
import Data.Maybe

type TableName = String
data TableValue = Str String | Numb Int deriving (Show, Eq)
type TableRow = [TableValue]
type ColumnName = String
type Column = ([TableName],ColumnName)
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

------------------------------- DIVISION -------------------------------------------------
dropCols :: [Column] -> [Column] -> [Column]
dropCols _ []  = []
dropCols [] ts  = ts
dropCols (c:cols) (t:tcols) = if snd c == snd t
                              then if null (fst c) && length (fst t) == 1
                                   then dropCols cols tcols
                                   else ((fst t \\ fst c), snd c):dropCols cols tcols
                              else t:dropCols (c:cols) tcols


removeCols :: [Column] -> [Column] -> [Column]
removeCols [] b = []
removeCols from rem = let torem = sortCols rem from
                      in filter (\(t,c) -> not (null t)) $ dropCols torem from

divtables :: Table -> Table -> Table
divtables t1@(_,_,acols) t2@(_,_,bcols) = let rest = removeCols acols bcols
                                              p1 = proy rest t1
                                              p2 = pcart p1 t2
                                              p3 = difftables p2 t1
                                              p4 = proy rest p3
                                          in difftables p1 p4


------------------------------- PRODUCTO NATURAL --------------------------------------------
-- Funcion que genera el arbol de condicion para producto natural.
prodNatCondition :: [Column] -> Condition
prodNatCondition [] = Empty
prodNatCondition (c:cols) = if length (fst c) == 1
                            then prodNatCondition cols
                            else And (f (fst c) (head (fst c))) (prodNatCondition cols)
                            where f [] _ = Empty
                                  f [c1'] v = Eq (Col ([c1'], snd c)) (Col ([v], snd c))
                                  f (c1':c2':cs') v = And (Eq (Col ([c1'], snd c)) (Col ([c2'], snd c))) (f cs' v)



    {- let (sames, rest) = lookFor (snd c) cols -- Busco las columnas con el mismo nombre
                                cond = equals c sames -- Genero el arbol de condicion
                            in And cond (prodNatCondition rest)
                            where lookFor _ [] = ([],[])
                                  lookFor n (col:cs) = let (eq', rest') = lookFor n cs
                                                        in if snd col == n then (col:eq', rest')
                                                                           else (eq', col:rest')
                                  equals _ [] = Empty
                                  equals col (same:ss) = And (Eq (Col col) (Col same)) (equals col ss) -}

pnat :: Table -> Table -> Table
pnat t1@(_,n1,_) t2@(_,n2,_) = let t'@(_, _, cols') = pcart t1 t2
                                   cond = prodNatCondition cols'
                                   t = sel t' cond
                                   cols = map (\(ts, cn) -> ([head ts], cn)) cols'
                               in ren (proy cols t) (n1 ++ "|*|" ++ n2)

------------------------------- INTERSECCION ----------- Hay que chequear tipos --------------
int :: Table -> Table -> Table
int t1 t2 = difftables t1 (difftables t1 t2)

------------------------------- RENOMBRAMIENTO -------------------------------------------------
ren :: Table -> TableName -> Table
ren (rs, _, cs) n = let ncs = renCols n cs
                    in (rs, n, ncs)
                    where renCols name [] = []
                          renCols name (c:cs') = if length (fst c) > 1
                                              then c:renCols name cs'
                                              else ([name], snd c):renCols name cs'

------------------------------- PRODUCTO CARTESIANO ----------- Hay que chequear tipos --------------
concatColsRows :: ([Column], [Column]) -> ([TableRow],[TableRow]) -> ([Column],[TableRow])
concatColsRows (xs, ys) (rs , rs') = let colrows = map combineGroup grouped
                                         cols = map fst colrows
                                         alignedRows = concatMap snd colrows
                                     in (cols, transpose alignedRows)
  where
    combined = zip (xs ++ ys) (transpose (concatMap (\r -> map (uncurry (++). (r,)) rs') rs))
    sorted = sortBy (compare `on` (snd. fst)) combined
    grouped = groupBy ((==) `on` (snd. fst)) sorted -- Agrupamos por nombre de columna
    -- Para columnas con el mismo nombre, agrupamos segun tabla de origen: ([t1,t2], cname)
    combineGroup grp = ((concatMap (fst. fst) grp, (snd. fst) (head grp)), map snd grp)

pcart :: Table -> Table -> Table -- aname y bname tienen que ser distintos
pcart (arows, _, acols) (brows, _, bcols) = let (cols, rs) = concatColsRows (acols,bcols) (arows,brows)
                                            in (rs, "X", cols)

------------------------------- DIFERENCIA  --------------------------------------------------------
existRow :: [Column] -> TableRow -> [Column] -> [TableRow] -> Bool
existRow [] _ _ _ = False
existRow _ [] _ _ = False
existRow _ _ [] _ = False
existRow _ _ _ [] = False
existRow acols a bcols (b:brs) = a == sortedb || existRow acols a bcols brs
                   where sortedb = concat sortedByTable
                         sortedByTable = map (\(t,((t', _),r)) -> 
                                              map snd (sortOn (\(z,_) -> 
                                                               fromMaybe (length (fst t)) (elemIndex z (fst t))) 
                                                  (zip t' r)))
                                             (zip acols sortedByCol)
                         sortedByCol = sortOn (\x -> fromMaybe (length acols) (elemIndex (snd (fst x)) (map snd acols))) grouped
                         grouped = f bcols b
                         f [] _ = []
                         f (bc@(l, _):bcs) b = (bc, take (length l) b): f bcs (drop (length l) b)

removeRows :: [Column] -> [TableRow] -> [Column] -> [TableRow] -> [TableRow]
removeRows acols [] bcols b = []
removeRows acols (a:ars) bcols brs = if existRow acols a bcols brs
                                     then removeRows acols ars bcols brs
                                     else a:removeRows acols ars bcols brs


difftables :: Table -> Table -> Table
difftables (ars, _, acols) (brs, _, bcols) = let rs = removeRows acols ars bcols brs
                                         in (rs , "difftables", acols)

------------------------------- UNION  ----------- Hay que chequear tipos --------------

uni :: Table -> Table -> Table
uni (arows, _, acols) (brows, _, _) = (nub (arows ++ brows), "uni", acols)

------------------------------- SELECCION  ----------- Hay que chequear tipos --------------

extractVal :: TableValue -> Either Int String
extractVal (Numb i) = Left i
extractVal (Str t) = Right t

getVal :: Value -> TableRow -> [Column] -> Either Bool TableValue
getVal (Val s) _ _ = Right s
getVal (Col var) (r:rs) (c:cs) = if snd var == snd c
                                 then (if (length (fst c) == 1) || (head (fst c) == head (fst var))
                                       then Right r
                                       else getVal (Col var) rs ((tail (fst c), snd c):cs))
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
cutCol :: TableRow -> [Column] -> [Column] -> TableRow
cutCol (v:vs) ((tcs, tc):tcols) ((ts,c):cols) | null tcs = cutCol (v:vs) tcols ((ts, c):cols)
                                              | null ts = cutCol (v:vs) ((tcs, tc):tcols) cols
                                              | tc == c = if head tcs == head ts
                                                          then v:cutCol vs ((tail tcs, tc):tcols) ((tail ts, c):cols)
                                                          else cutCol vs ((tail tcs, tc):tcols) ((ts, c):cols)
                                              | otherwise = cutCol (drop (length tcs) (v:vs)) tcols ((ts,c):cols)
cutCol _ _ _ = []

-- Devuelve ls filas cortadas a partir de las columnas [cols]
cutCols :: ([TableRow], [Column]) -> [Column] -> [TableRow]
cutCols ([], _) _ = []
cutCols (r:rows, tcols) cols = let r' = cutCol r tcols cols
                                   rows' = cutCols (rows, tcols) cols
                               in (r':rows')

sortCols :: [Column] -> [Column] -> [Column]
sortCols _ [] = []
sortCols [] _ = []
sortCols cs ts@(tc:tcols) = let scols = sortOn (\x -> fromMaybe (length ts) (elemIndex (snd x) (map snd ts))) cs
                            in sortCols' scols ts
                    where sortCols' _ [] = []
                          sortCols' [] _ = [] 
                          sortCols' (c:cols) (t':ts') = if snd c == snd t'
                                                        then if null (fst c)
                                                             then t':sortCols' cols ts'
                                                             else (fst t' \\ (fst t' \\ fst c) , snd c):sortCols' cols ts'
                                                        else sortCols' (c:cols) ts'

proy :: [Column] -> Table -> Table
proy [] (_, tname, _) = ([], tname, [])
proy cols (trows, _, tcols) = let sortedCols = sortCols cols tcols
                                  trows' = cutCols (trows, tcols) sortedCols
                              in (trows', "proy", sortedCols)
---------------------------------------------------------