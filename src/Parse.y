{
module Parse where
import Common
import Data.Maybe
import Data.Char

}

%monad { P } { thenP } { returnP }
%name parseStmt Comm
%name parseStmts Comms
%name term Exp

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '|*|'    { TPNat }
    '*'      { TPCart }
    'I'      { TInt } 
    '/'      { TDiv } 
    'U'      { TUni } 
    '-'      { TDiff } 
    '='      { TEquals }
    '>='     { TGreq }
    '<='     { TLreq }
    '>'      { TGr }
    '<'      { TLr }
    '&'      { TAnd }
    '|'      { TOr }
    '.'      { TDot }
    '('      { TOpen }
    ')'      { TClose }
    '['      { TOpenB }
    ']'      { TCloseB }
    '->'     { TArrow }
    '"'      { TQuote }
    CSV      { TCsv }
    AS       { TAs }
    DROP     { TDrop }
    IMPORT   { TImport }
    EXPORT   { TExport }
    DATABASE { TDatabase }
    TABLE    { TTable }
    STRING   { TString $$ }
    VAR      { TVar $$ }
    NUM      { TNum $$ }
    TEXT     { TText }
    SEL      { TSelect }
    PROY     { TProy }
    REN      { TRen }
    HOST     { THost }
    DB       { TDb }
    PORT     { TPort }
    USER     { TUser }
    PW       { TPw }
    OPERATOR { TOperator }
    '=>'     { TOp }

%right VAR NUM
%left '->'
%left '/'
%left 'U' 'I'
%left '-'
%left '=' '&' '|' '*' '|*|' 
%right PROY SELECT


%% 

Comm     :  Defexp                                { $1 }
         |  Assign	                         { $1 }
         | IMPORT CSV VAR AS VAR                  { ImportCSV $3 $5 }
         | IMPORT DATABASE '[' ConnInfo ']'      { ImportDB $4 }
         | EXPORT CSV VAR AS VAR                  { ExportCSV $3 $5 }
         | Drop                                   { $1 }
         | TEXT '['STRING ']'                            { Text $3 }
         | Exp	                                 { Eval $1 }
         | OPERATOR VAR '=' '(' Args ')' '=>' Exp { Operator $2 $5 $8 }
Defexp   :  TABLE VAR '=' Exp                       { Table $2 $4 }
Assign : VAR '->' Exp                            { Assign $1 $3 }
             

Drop : DROP TABLE VAR    { DropTable $3 }
     | DROP OPERATOR VAR { DropOp $3 }

Exp     :: { TableTerm }
        : SEL '[' Cond ']' '(' Exp ')' { LSel $3 $6}
        | PROY '[' Cols ']' '(' Exp ')'  { LProy $3 $6}
        | REN '[' VAR ']' '(' Exp ')'     { LRen $3 $6}
        | VAR                             { LTableVar $1 }
        | Exp '*' Exp                     { LPCart $1 $3 }
        | Exp '|*|' Exp                   { LPNat $1 $3 }
        | Exp '-' Exp                     { LDiff $1 $3 }
        | Exp 'U' Exp                     { LUni $1 $3 }
        | Exp 'I' Exp                     { LInt $1 $3 }
        | Exp '/' Exp                     { LDiv $1 $3 }
        | VAR '[' Args  ']'               { LApp $1 $3 }
        | '(' Exp ')'                     { $2 }

ConnInfo :: { ConnInfo }
          : ConnData                 { [$1] }  
          | ConnData '|' ConnInfo   { $1 : $3 }


ConnData :: { ConnData }
         : HOST Atom    { LHost $2 }
         | DB Atom      { LDb $2   }
         | PORT Atom    { LPort $2 }
         | USER Atom    { LUser $2 }
         | PW Atom      { LPw $2   }

Args :: { OperatorArgs }
     : VAR                { [$1] }
     | VAR '|' Args       { $1 : $3 }

Cols :: { TableCols }
      : Atom                           { [$1] }
      | Atom '|' Cols               { $1 : $3 }

Cond :: { TableCond }
        : TermCond                { $1 }
        | Cond '&' Cond     { LAnd $1 $3 }
        | Cond '|' Cond     { LOr $1 $3 }

TermCond :: { TableCond }
        : Atom '=' Atom           { LEquals $1 $3 }
        | Atom '<' Atom           { LLr $1 $3 }
        | Atom '>' Atom           { LGr $1 $3 }
        | Atom '<=' Atom          { LLrEq $1 $3 }
        | Atom '>=' Atom          { LGrEq $1 $3 }

Atom :: { TableAtom }
     : NUM      { LNum $1 }
     | STRING   { LString $1 }
     | VAR      { LVar $1 }

Comms    : Comm Comms                  { $1 : $2 }
         |                              { [] }

{

data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)


data Token = TVar String
               | TNum String
               | TString String
               | TPNat
               | TPCart
               | TInt
               | TUni
               | TImport
               | TExport
               | TDatabase
               | TCsv
               | TAs
               | TDrop
               | TDiff
               | TDiv
               | TSelect
               | TProy
               | TRen
               | TText
               | TDot
               | TQuote
               | TTable
               | TComma
               | TOpen
               | TOpenB
               | TClose 
               | TCloseB 
               | TColon
               | TArrow
               | TEquals
               | TGreq
               | TLreq
               | TGr
               | TLr
               | TAnd
               | TOr
               | TEOF
               | THost
               | TDb
               | TUser
               | TPw
               | TPort
               | TOperator
               | TOp
               deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isDigit c -> lexNum False (c:cs)
                          | isAlpha c -> lexVar (c:cs)
                    ('"':cs) -> lexString cs
                    ('-':'-':cs) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':'-':cs) -> consumirBK 0 0 cont cs	
                    ('-':'}':cs) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('-':'>':cs) -> cont TArrow cs
                    ('.':cs) -> cont TDot cs
                    ('(':cs) -> cont TOpen cs
                    ('[':cs) -> cont TOpenB cs
                    (')':cs) -> cont TClose cs
                    (']':cs) -> cont TCloseB cs
                    ('|':'*':'|':cs) -> cont TPNat cs
                    ('*':cs) -> cont TPCart cs
                    ('-':c:cs) -> if isDigit c then lexNum True (c:cs)
                                             else cont TDiff (c:cs)
                    ('-':cs) -> cont TDiff cs
                    ('/':cs) -> cont TDiv cs
                    ('&':cs) -> cont TAnd cs
                    ('|':cs) -> cont TOr cs
                    ('<':'=':cs) -> cont TLreq cs
                    ('>':'=':cs) -> cont TGreq cs
                    ('=':'>':cs) -> cont TOp cs
                    ('=':cs) -> cont TEquals cs
                    ('>':cs) -> cont TGr cs
                    ('<':cs) -> cont TLr cs
                    unknown -> \line -> Failed $ 
                     "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlpha' cs of
                              ("ht", (':':rest)) -> cont THost rest
                              ("db", (':':rest)) -> cont TDb rest
                              ("us", (':':rest)) -> cont TUser rest
                              ("pw", (':':rest)) -> cont TPw rest
                              ("pt", (':':rest)) -> cont TPort rest
                              ("drop",rest) -> cont TDrop rest
                              ("as",rest) -> cont TAs rest
                              ("csv",rest) -> cont TCsv rest
                              ("import",rest) -> cont TImport rest
                              ("export",rest) -> cont TExport rest
                              ("database",rest) -> cont TDatabase rest
                              ("table", rest) -> cont TTable rest
                              ("operator", rest) -> cont TOperator rest
                              ("S",('[':rest))   -> cont TSelect ('[':rest)
                              ("P", ('[':rest))   -> cont TProy ('[':rest)
                              ("R", ('[':rest))   -> cont TRen ('[':rest)
                              ("T", ('[':rest))   -> cont TText ('[':rest)
                              ("U", rest)   -> cont TUni rest
                              ("I", rest)   -> cont TInt rest
                              (var,rest)     -> cont (TVar var) rest
                          consumirBK anidado cl cont s = case s of
                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
                              ('-':('}':cs)) -> case anidado of
                                                  0 -> \line -> lexer cont cs (line+cl)
                                                  _ -> consumirBK (anidado-1) cl cont cs
                              ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                              (_:cs) -> consumirBK anidado cl cont cs
                          lexNum neg cs = case span isDigit cs of
                                        (num,rest)     -> if neg then cont (TNum ('-':num)) rest
                                                                 else  cont (TNum num) rest
                          lexString cs = case span isNotQuote cs of
                                (str, ('"':rest)) -> cont (TString str) rest
                                (str, rest) -> cont (TString str) rest 
                                where isNotQuote c = c /= '"' 
                          isAlpha' c = isAlpha c || c == '.' || c == '_' || c == '-' || c == '/' || isDigit c

                           
stmts_parse s = parseStmts s 1
stmt_parse s = parseStmt s 1
term_parse s = term s 1
}
