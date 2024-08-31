{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TableOperators where

import Data.String
import Data.List
import Data.Function
import Data.Maybe

type TableName = String

-- Valores en las celdas.
data TableValue = Str String | Numb Int deriving (Show, Eq)
-- Fila
type TableRow = [TableValue]

-- Columna: (Tablas de origen, Nombre de columna)
-- Notar que pueden haber columnas con mismo nombre pero
-- representando tablas diferentes.
type ColumnName = String
type Column = ([TableName],ColumnName)

-- Tabla: (Filas, Columnas)
type Table = ([TableRow],[Column])

-- Representacion de valores ingresados por el usuario, 
-- pueden representar valores directos o indirectos 
-- mediante columnas.
data Value = Col Column | Val TableValue
               deriving(Show)

-- Arbol de condicion para para filtrar filas
data Condition = And Condition Condition
               | Or Condition Condition
               | Gr Value Value
               | Lr Value Value
               | Greq Value Value
               | Lreq Value Value
               | Eq Value Value
               | Empty
               deriving(Show)


-- tableValueToString x:
--  pasaje de valor en celda x a valor imprimible
tableValueToString :: TableValue -> String
tableValueToString (Str s) = s
tableValueToString (Numb i) = show i

-- extractVal x
--  extrae el valor de una celda en Int o String (Either)
extractVal :: TableValue -> Either Int String
extractVal (Numb i) = Left i
extractVal (Str t) = Right t


-- OPERACION NECESARIA PARA EL RENOMBRAMIENTO ALGEBRAICO --

-- ren x n
--  renombra la tabla x con nombre n
--   solo se renombran las columnas que tienen una sola
--   tabla de origen.
ren :: Table -> TableName -> Table
ren (rs, cs) n = let ncs = renCols n cs
                 in (rs, ncs)
                where renCols name [] = []
                      renCols name (c:cs') = if length (fst c) > 1
                                             then c:renCols name cs'
                                             else ([name], snd c):renCols name cs'



--- OPERACIONES NECESARIAS PARA LA RESTA ALGEBRAICA --

-- existRow
--  determina si una fila existe en un grupo de filas que pueden tener
--  un orden distinto de columnas.
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

-- removeRows _ a _ b
--  remueve un grupo de filas b de otro grupo de filas a que pueden tener un orden
--  de columnas diferentes
removeRows :: [Column] -> [TableRow] -> [Column] -> [TableRow] -> [TableRow]
removeRows acols [] bcols b = []
removeRows acols (a:ars) bcols brs = if existRow acols a bcols brs
                                     then removeRows acols ars bcols brs
                                     else a:removeRows acols ars bcols brs

-- difftables t1 t2
--  resta algebraica de las filas de t2 sobre las de t1
difftables :: Table -> Table -> Table
difftables (ars, acols) (brs, bcols) = let rs = removeRows acols ars bcols brs
                                       in (rs , acols)


--- OPERACIONES NECESARIAS PARA LA SELECCION ALGEBRAICA --

-- getVal 
--  obtiene el valor ingresado por el usuario, sea atomico o desde
--  una columna, de no poder devuelve false.
getVal :: Value -> TableRow -> [Column] -> Either Bool TableValue
getVal (Val s) _ _ = Right s
getVal (Col var) (r:rs) (c:cs) = if snd var == snd c
                                 then (if (length (fst c) == 1) || (head (fst c) == head (fst var))
                                       then Right r
                                       else getVal (Col var) rs ((tail (fst c), snd c):cs))
                                 else getVal (Col var) (drop (length (fst c)) (r:rs)) cs
getVal _ [] _ = Left False
getVal _ _ [] = Left False

-- getNumber x
--  extrae el numero dentro de x, de no poder devuelve False
getNumber :: Either Int String -> Either Bool Int
getNumber (Left i) = Right i
getNumber _ = Left False


-- condition c row cols
--  devuelve el resultado de aplicar la condicion c
--  sobre una fila row ordenada en base a cols 
condition :: Condition -> TableRow -> [Column] -> Bool
condition c r cs = case c of
  Empty      -> True
  And c1 c2  -> condition c1 r cs && condition c2 r cs
  Or c1 c2   -> condition c1 r cs || condition c2 r cs
  Eq c1 c2   -> case values c1 c2 of
                Right (v1, v2) -> v1 == v2
                _              -> False
  Gr v1 v2   -> case numbers v1 v2 of
                Right (n1, n2) -> n1 > n2
                _              -> False
  Greq v1 v2 -> case numbers v1 v2 of
                Right (n1, n2) -> n1 >= n2
                _              -> False
  Lr v1 v2   -> case numbers v1 v2 of
                 Right (n1, n2) -> n1 < n2
                 _              -> False
  Lreq v1 v2 -> case numbers v1 v2 of
                Right (n1, n2) -> n1 <= n2
                _              -> False
  where
    values va vb =
      case getVal va r cs of
        Right a -> case getVal vb r cs of
                    Right b -> Right (extractVal a, extractVal b)
                    _ -> Left False
        _ -> Left False
    numbers va vb =
      case values va vb of
        Right (a, b) -> case getNumber a of
                          Right a' -> case getNumber b of
                                        Right b' -> Right (a', b')
                                        _ -> Left False
                          _ -> Left False
        _ -> Left False

-- sel t c
--  seleccion algebraica de la tabla t a partir de la condicion c. 
sel :: Table -> Condition -> Table
sel ([], cols) _ = ([], cols)
sel (r:rs, cols) cond = let (rs', _) = sel (rs, cols) cond
                        in if condition cond r cols then (r:rs', cols)
                           else (rs', cols)

--- OPERACIONES NECESARIAS PARA LA PROYECCION ALGEBRAICA --

-- cutRow
--  devuelve los elementos de la fila correspondiente a las columnas
cutRow :: TableRow -> [Column] -> [Column] -> TableRow
cutRow (v:vs) ((tcs, tc):tcols) ((ts,c):cols) | null tcs = cutRow (v:vs) tcols ((ts, c):cols)
                                              | null ts = cutRow (v:vs) ((tcs, tc):tcols) cols
                                              | tc == c = if head tcs == head ts
                                                          then v:cutRow vs ((tail tcs, tc):tcols) ((tail ts, c):cols)
                                                          else cutRow vs ((tail tcs, tc):tcols) ((ts, c):cols)
                                              | otherwise = cutRow (drop (length tcs) (v:vs)) tcols ((ts,c):cols)
cutRow _ _ _ = []

-- cutRows (rows, tcols) cols
--  devuelve las filas rows cortadas a partir de las columnas cols
cutRows :: ([TableRow], [Column]) -> [Column] -> [TableRow]
cutRows ([], _) _ = []
cutRows (r:rows, tcols) cols = let r' = cutRow r tcols cols
                                   rows' = cutRows (rows, tcols) cols
                               in (r':rows')

-- sortCols c1 c2 
--  ordena las columnas c1 segun como aparecen en c2.
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

-- proy cs t
--  Proyeccion algebraica de la tabla t a partir de las columnas cs. 
proy :: [Column] -> Table -> Table
proy [] _ = ([], [])
proy cols (trows, tcols) = let sortedCols = sortCols cols tcols
                               trows' = cutRows (trows, tcols) sortedCols
                           in (trows', sortedCols)


--- OPERACIONES NECESARIAS PARA EL PRODUCTO CARTESIANO ALGEBRAICO --

-- concatColsRows (c1, c2) (r1, r2)
--  concatena todas las filas de r2 a cada fila de r1 y las ordena segun
--  la union de las columnas.
concatColsRows :: ([Column], [Column]) -> ([TableRow],[TableRow]) -> ([Column],[TableRow])
concatColsRows (xs, ys) (rs , rs') = let colrows = map combineGroup grouped
                                         cols = map fst colrows -- extramos columnas
                                         alignedRows = concatMap snd colrows -- extraemos filas transpuestas
                                     in (cols, transpose alignedRows)
  where
    -- aplanamos las columnas para alinearlas a las filas
    plain = concatMap (\c -> map (\e -> ([e], snd c)) (fst c))
    -- alineamos cada columna a una lista que representa cada valor de la fila en la columa
    combined = zip (plain xs ++ plain ys) (transpose (concatMap (\r -> map (uncurry (++). (r,)) rs') rs))
    -- ordenamos las columnas junto con las filas en base al nombre de la columna
    sorted = sortBy (compare `on` (snd. fst)) combined
    -- agrupamos las columnas con mismo nombre(diferentes tabla origen)
    grouped = groupBy ((==) `on` (snd. fst)) sorted -- Agrupamos por nombre de columna
    -- concatenamos tablas origen y concatenamos valores de fila.
    combineGroup grp = ((concatMap (fst. fst) grp, (snd. fst) (head grp)), map snd grp)


-- pcart t1 t2
--  producto cartesiano algebraico de las tablas(relaciones) t1 y t2
pcart :: Table -> Table -> Table
pcart (arows, acols) (brows, bcols) = let (cols, rs) = concatColsRows (acols,bcols) (arows,brows)
                                      in (rs, cols)


-- OPERACIONES NECESARIAS PARA EL PRODUCTO NATURAL ALGEBRAICO --

-- prodNatCondition
--  genera el arbol de condición para producto natural.
--   para cada columna, se crea una igualdad circular entre
--   todas sus tablas origen.
prodNatCondition :: [Column] -> Condition
prodNatCondition [] = Empty
prodNatCondition (c:cols) = if length (fst c) == 1
                            then prodNatCondition cols
                            else And (f (fst c) (head (fst c))) (prodNatCondition cols)
                            where f [] _ = Empty
                                  f [c1'] v = Eq (Col ([c1'], snd c)) (Col ([v], snd c))
                                  f (c1':c2':cs') v = And (Eq (Col ([c1'], snd c)) (Col ([c2'], snd c))) (f cs' v)

-- pnat t1 t2
--  producto natural algebraico de las tablas(relaciones) t1 y t2
pnat :: Table -> Table -> Table
pnat t1 t2 = let t'@(_, cols') = pcart t1 t2
                 -- condicion de pnat
                 cond = prodNatCondition cols'
                 t = sel t' cond
                 -- reducimos a una tabla origen por columna.
                 cols = map (\(ts, cn) -> ([head ts], cn)) cols'
             in proy cols t


-- OPERACIONES NECESARIAS PARA LA DIVISION ALGEBRAICA --

-- dropCols l1 l2:
--  elimina las columnas en l2 de l1 (se presupone columnas ordenadas)
dropCols :: [Column] -> [Column] -> [Column]
dropCols _ []  = []
dropCols [] ts  = ts
dropCols (c:cols) (t:tcols) = if snd c == snd t
                              then if null (fst c) && length (fst t) == 1
                                   then dropCols cols tcols
                                   else ((fst t \\ fst c), snd c):dropCols cols tcols
                              else t:dropCols (c:cols) tcols

-- removeCols l1 l2:
--  elimina las columnas en l2 de l1 
removeCols :: [Column] -> [Column] -> [Column]
removeCols [] b = []
removeCols from rem = let torem = sortCols rem from
                      in filter (\(t,c) -> not (null t)) $ dropCols torem from

-- divtables t1 t2
--  división algebraica de las tablas(relaciones) t1 y t2
divtables :: Table -> Table -> Table
divtables t1@(_,acols) t2@(_,bcols) = let rest = removeCols acols bcols
                                          p1 = proy rest t1
                                          p2 = pcart p1 t2
                                          p3 = difftables p2 t1
                                          p4 = proy rest p3
                                      in difftables p1 p4


--- OPERACION NECESARIA PARA LA INTERSECCION ALGEBRAICA --

-- int t1 t2
--  interseccion de las filas de t1 y t2 en una tabla
int :: Table -> Table -> Table
int t1 t2 = difftables t1 (difftables t1 t2)


--- OPERACION NECESARIA PARA LA UNION ALGEBRAICA --

-- union t1 t2
--  union de las filas de t1 y t2 en una tabla
uni :: Table -> Table -> Table
uni (arows, acols) (brows, _) = (nub (arows ++ brows), acols)