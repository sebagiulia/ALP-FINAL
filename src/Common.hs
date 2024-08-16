module Common where

import TableOperators

-- Tipo de los tipos
data Type = StrT 
          | IntT
          deriving (Show, Eq)
type TableType = (TableName, [(Column, Type)])
data ConnectionType = T { 
                          host :: String,
                          db :: String,
                          port :: String,
                          user :: String,
                          pw :: String  
                        } deriving(Show)

type Context = [TableType]




-- Entornos
type NameEnv table ty = [(TableName, (table, ty))]

data ConnWord = LHost TableAtom 
              | LDb TableAtom
              | LUser TableAtom 
              | LPw TableAtom 
              | LPort TableAtom
              deriving(Show) 

type ConnWords = [ConnWord] 

-- TableColumns
type TableCols = [TableAtom]

-- TableAtom
data TableAtom = LVar String | LNum String | LString String deriving(Show)

-- TableCond
data TableCond = LEquals TableAtom TableAtom
               | LAnd TableCond TableCond
               | LOr TableCond TableCond
               | LLr TableAtom TableAtom
               | LGr TableAtom TableAtom
               | LLrEq TableAtom TableAtom
               | LGrEq TableAtom TableAtom
          deriving (Show)


data TableAssign = LAssign TableName TableTerm
          deriving (Show)

-- TableTerm
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
          deriving (Show)


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
          deriving (Show)

-- Comandos interactivos o de archivos
data Stmt i = Def String i           --  Declarar un nuevo identificador x, def x = e
            | Eval i                 --  Evaluar el término
            | Assign String i        --  Declarar un identificador temporal x, x -> t
            | ImportDB ConnWords      --  Conectarse a base de datos
            | ImportCSV String String
            | ExportCSV String String
  deriving (Show)

instance Functor Stmt where
  fmap f (Def s i) = Def s (f i)
  fmap f (Assign s i) = Assign s (f i)
  fmap f (Eval i)  = Eval (f i)
  fmap f (ImportCSV fil v)  = ImportCSV fil v
  fmap f (ImportDB w)  = ImportDB w
  fmap f (ExportCSV v s)  = ExportCSV v s