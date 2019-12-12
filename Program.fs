open System
open Brainfuck.AbstractMachine

[<EntryPoint>]
let main argv =
    
  let showLoaded loaded =
    loaded
    |> Loader.show
    |> printfn "%s"

    loaded

  argv
  |> Array.tryHead
  |> Option.map (
      Loader.tryLoadProgram
//      >> Result.map showLoaded
      >> Result.map Interpreter.start
      >> ignore
//      >> printfn "%A"
  )
  |> Option.defaultWith (fun _ ->
      printfn "Needs argument"
  )

  0 // return an integer exit code