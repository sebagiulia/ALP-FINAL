# TP FINAL - ALP - 2024

## Introducción
Hola! Este README es un documento complementario al informe PDF del proyecto.

### Stack
En este proyecto se utilizó Stack (https://docs.haskellstack.org/), una herramienta sencilla
para desarrollar proyectos en Haskell.

Antes que nada, puede que tengas que instalarlo. En https://docs.haskellstack.org/en/stable/
README/#how-to-install hay guías de instalación para distintas plataformas.

Para instalar la versión correcta de GHC e instalar los paquetes necesarios basta con abrir una
terminal en el directorio AR-SQL y ejecutar:
```
stack setup
```

Luego de esto (que puede demorar un rato), está todo listo para compilar el proyecto, haciendo:
```
stack build
```

Una vez compilado el proyecto, se puede correr el ejecutable definido en app/Main.hs haciendo:
```
stack exec AR−SQL
```
Esto lanzará el evaluador interactivo de AR-SQL. Con el comando :? pueden leer sobre el resto de
los comandos disponibles.

### Estructura del código
La estructura del proyecto es la siguiente:
```
.
├── app
│   └── Main.hs
├── src
│   ├── Common.hs
│   ├── Csv.hs
│   ├── Error.hs
│   ├── Eval.hs
│   ├── Mysql.hs
│   ├── Parse.y
│   ├── PrettyPrinter.hs
│   ├── TableOperators.hs
│   └── TypeChecker.hs
├── Ejemplos
│   ├── Prelude.arsql
│   └── spj.arsql
├── imports
│   ├── S.csv
│   ├── P.csv
│   ├── J.csv
│   └── SPJ.csv
├── exports
├── README.md
├── Setup.hs
├── AR-SQL.cabal
├── package.yaml
├── stack.yaml
└── stack.yaml.lock
```
* En el directorio `app` se define el módulo `Main`, que implementa el ejecutable final. 

* En el directorio `src` se encuentran:
- `Common` define los tipos de expresiones junto a sus ASTs y comandos presentados en la gramática junto a alguno tipos auxiliares.
- `Eval` define los evaluadores de las expresiones y de algunos comandos.
- `TypeChecker` define el chequeador de tipos de las expresiones.
- `PrettyPrinter` define el printer del interprete.
- `Error` define los mensajes de error del interprete.
- `Parse` define el parser del lenguaje generado automáticamente por Happy.
- `Mysql` define las funciones para importar un dataset de una base de datos MySQL.
- `Csv` define las funciones para manejar archivos csv.
- `TableOperators` define las operaciones sobre las relaciones basadas en el álgebra relacional.

* En el directorio `Ejemplos` está `Prelude.arsql`, con la importación de tablas csv.
También se encuentra `spj.arsql` que utiliza las tablas importadas en el preludio para hacer ope-
raciones sobre estas.
(Las tablas son las que se utilizan en la materia TBD para aprender álgebra relacional, y en
`spj.arsql` se muestran algunos ejercicios característicos resueltos.)
En el directorio imports se encuentran las tablas importadas desde el preludio.
(aclaración: no es necesario para importar tablas que se encuentren en la carpeta imports, basta
con pasar el path adecuado al comando)
En el directorio `exports` se encuentran las tablas exportadas desde el interprete a csv.
* El resto de los archivos son de configuración del proyecto. 

## Referencias
[1] - Fueron utilizados como guías estructurales del proyecto los trabajos prácticos 3 y 4 realizados
durante el cursado de la materia.

[2] - https://docs.haskellstack.org/en/stable/README/#how-to-install

[3] - https://hackage.haskell.org/package/mysql-haskell

[4] - https://hackage.haskell.org/package/cassava-0.5.3.2/docs/Data-Csv.html

[5] - https://haskell-happy.readthedocs.io/en/latest/using.html

