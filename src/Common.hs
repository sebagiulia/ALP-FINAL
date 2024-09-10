module Common where

  import TableOperators

  -- TableAtom
  -- Representacion de variables, strings y numeros
  data TableAtom = LVar String | LNum String | LString String deriving(Show)

  -- Representacion de la informacion de conexion con DB 
  data ConnData = LHost TableAtom 
                | LDb TableAtom
                | LUser TableAtom 
                | LPw TableAtom 
                | LPort TableAtom
                deriving(Show)
  type ConnInfo = [ConnData]

  -- Representacion de parametros de operadores
  type OperatorArgs = [String]
  type OperatorName = String

  -- Comandos interactivos o de archivos
  data Stmt i = Table String i           --  Declarar un nuevo identificador x, def x = e
              | Eval i                 --  Evaluar el término
              | Assign String i        --  Declarar un identificador temporal x, x -> t
              | ImportDB ConnInfo      --  Importar dataset desde una base de datos
              | ImportCSV String String -- Importar una tabla desde un archivo csv
              | ExportCSV String String -- Exportar tabla a archivo csv
              | DropTable String        -- Eliminar una tabla de las tablas globales
              | DropOp String           -- Eliminar un operador de los operadores
              | Operator String OperatorArgs i -- Declarar un nuevo operador f, operator f = (..) ... 
              | Text String -- Imprimir texto
    deriving (Show)

  instance Functor Stmt where
    fmap f (Table s i) = Table s (f i)
    fmap f (Assign s i) = Assign s (f i)
    fmap f (Eval i)  = Eval (f i)
    fmap f (ImportCSV fil v)  = ImportCSV fil v
    fmap f (ImportDB w)  = ImportDB w
    fmap f (ExportCSV v s)  = ExportCSV v s
    fmap f (DropTable v) = DropTable v
    fmap f (DropOp v) = DropOp v
    fmap f (Operator v a i)  = Operator v a (f i)
    fmap f (Text s) = Text s

  -- Tipo de las celdas
  data Type = StrT 
            | IntT
            deriving (Show, Eq)

  -- Tipo de las tablas
  type TableType = (TableName, [(Column, Type)])

  -- Entornos
  type GlobalE = [(TableName, (Table, TableType))] -- Tablas globales
  type LocalE = [(TableName, (Table, TableType))] -- Tablas locales
  type OperE = [(OperatorName, Term)] -- Operadores

  -- TableColumns
  -- Representacion de las columnas ingresadas por el usuario en proyeccion
  type TableCols = [TableAtom]


  -- Representacion de condicion.
  -- TableCond
  data TableCond = LEquals TableAtom TableAtom
                 | LAnd TableCond TableCond
                 | LOr TableCond TableCond
                 | LLr TableAtom TableAtom
                 | LGr TableAtom TableAtom
                 | LLrEq TableAtom TableAtom
                 | LGrEq TableAtom TableAtom
            deriving (Show)

  -- Representacion de asignacion de variable local
  data TableAssign = LAssign TableName TableTerm
            deriving (Show)

  -- TableTerm: AST de terminos con nombres
  data TableTerm = LSel TableCond TableTerm
                 | LProy TableCols TableTerm            
                 | LRen TableName TableTerm            
                 | LPNat TableTerm TableTerm            
                 | LPCart TableTerm TableTerm            
                 | LDiv TableTerm TableTerm            
                 | LDiff TableTerm TableTerm            
                 | LUni TableTerm TableTerm            
                 | LInt TableTerm TableTerm
                 | LTableVar TableName
                 | LApp String OperatorArgs
            deriving (Show)


  -- Term: AST de terminos con variables globales, locales y ligadas
  data Term = Sel Condition Term
            | Proy [Column] Term
            | Ren TableName Term
            | PNat Term Term
            | PCart Term Term
            | Div Term Term
            | Diff Term Term
            | Uni Term Term
            | Int Term Term
            | GlobalTableVar TableName
            | LocalTableVar TableName
            | Bound Int
            | App String OperatorArgs
            deriving (Show)