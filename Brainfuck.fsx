#load "Syntax.fs"

open System
open Brainfuck
open AbstractMachine

//Loader.tokenize "[->+<]"
//|> Result.map Parser.translate

"++++++++[-]"
|> Loader.tryLoadProgram
|> Result.map Interpreter.start
