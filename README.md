# SLox - Scala Implementation of the Lox Language

This is a WIP implementation of the jlox Language described in the book
[Crafting Interpreters](https://craftinginterpreters.com/).

## Running the code

The code is run using the Scala Built Tool [sbt](https://www.scala-sbt.org/).

The REPL can be started by running with no command line arguments:

```shell
sbt run
```

To run the interpreter with a script:

```shell
sbt "run /path/to/my/script" 
```

See the help:

```shell
sbt "run --help"
```
