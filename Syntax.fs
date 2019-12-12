namespace Brainfuck

open System


module Address =
  type T = int

  let make x : T = x

  let after = (+) 1

module Error =
  type T =
    | UnrecognizedToken of Address.T * char

module Direction =
  type T = Right | Left

module Change =
  type T = Increment | Decrement

module Interaction =
  type T = Input | Output

module Demarcation =
  type T = Enter | Exit

module TokenType =
  type T =
    | Move     of Direction.T
    | Update   of Change.T
    | Interact of Interaction.T
    | Branch   of Demarcation.T

  let (|Recognized|_|) = function
  | '>' -> Move Direction.Right        |> Some
  | '<' -> Move Direction.Left         |> Some
  | '+' -> Update Change.Increment     |> Some
  | '-' -> Update Change.Decrement     |> Some
  | '.' -> Interact Interaction.Output |> Some
  | ',' -> Interact Interaction.Input  |> Some
  | '[' -> Branch Demarcation.Enter    |> Some
  | ']' -> Branch Demarcation.Exit     |> Some
  | any -> None

type Token = Token of Address.T * TokenType.T

module Condition =
  type T = Zero | NonZero

module Instruction =
  type T =
    | Move     of Direction.T
    | Update   of Change.T
    | Interact of Interaction.T
    | Jump     of Address.T * Condition.T
    | Halt

  let show instruction = 
    instruction.ToString ()

module Function =
  let konst x = fun _ -> x

module List =
  let cons x xs = x::xs

  let sequence (xs: 'a option list) : 'a list option =
    let prepend =
      Option.map << cons

    let folder = function
    | Some x -> prepend x
    | None   -> Function.konst None

    List.foldBack folder xs <| Some []

  let traverse f =
    List.map f >> sequence

  let traversei f =
    List.mapi f >> sequence

module Result =
  let cons x xs = x::xs

  let sequence (xs: Result<'a, 'e> list) : Result<'a list, 'e> =
    let prepend =
      Result.map << cons

    let folder = function
    | Ok x    -> prepend x
    | Error e -> Function.konst (Error e)

    List.foldBack folder xs <| Ok []

  let traverse f =
    List.map f >> sequence

  let traversei f =
    List.mapi f >> sequence

module Parser =
  type StreamEntry = Address.T * Instruction.T

  module StreamEntry =
    let make address instruction = 
      address, instruction

    let address = function
    | (address, _) -> address

    let show = function
    | address, instruction ->
      instruction
      |> Instruction.show
      |> sprintf "[%d] %s" address

  type InstructionStream = StreamEntry list

  type Block = 
    | Open of int * InstructionStream
    | Closed of int * int * InstructionStream

  module Block =
    (* This whole implementation needs a once-or-twice over. *)
    let enter address = Open (address, [])

    let exit exitAddress = function
    | Open (enterAddress, stream) -> 
      Closed (enterAddress, exitAddress, stream)
    | Closed _ -> 
      failwith "wtf"

    let add x = function
    | Open (enterAddress, stream) -> 
      Open (enterAddress, x :: stream)
    | Closed _ -> 
      failwith "wtf"

    let stream = function
    | Closed (_, _, stream) -> stream
    | Open (_, stream)      -> stream

  type BlockStack = Blocks of Block * Block list * InstructionStream

  (* Is this the right name for this? *)
  module BlockStack =
    let empty = Blocks (Block.enter 0, [], [])

    let add instruction = function
    | Blocks (head, tail, committed) ->
      let head' = Block.add instruction head
      Blocks (head', tail, committed)

    let enter at = function
    | Blocks (head, tail, committed) ->
      let head' = Block.enter at
      Blocks (head', head :: tail, committed)

    let commit = function
    | Closed (enter, exit, stream) ->
      let jumpEnd =
        Instruction.Jump (Address.after exit,  Condition.Zero)
        |> StreamEntry.make enter
      let jumpBeginning = 
        Instruction.Jump (Address.after enter, Condition.NonZero)
        |> StreamEntry.make exit

      jumpEnd :: stream @ [jumpBeginning]
    | Open _ ->
      failwith "wtf"

    let leave at = function
    | Blocks (head, parent :: tail, committed) ->
      let head'  = Block.exit at head
      let stream = commit head'

      Blocks (parent, tail, stream @ committed)

    | _ -> failwith "I know."

    let joinStreams = function
    | Blocks (root, _, stream) ->
      Block.stream root @ stream


  let insertHalt stream =
    let haltEntryAt at =
      Instruction.Halt
      |> StreamEntry.make at

    let instruction =
      stream
      |> List.maxBy (fun (address, _) -> address)
      |> StreamEntry.address
      |> Address.after
      |> haltEntryAt

    instruction :: stream

  let translate =

    let translateTrivial = function
    | TokenType.T.Move     x -> Instruction.Move x
    | TokenType.T.Update   x -> Instruction.Update x
    | TokenType.T.Interact x -> Instruction.Interact x
    | _ -> failwith "I know."

    let translateOne stack = function
    | Token (address, TokenType.T.Branch Demarcation.Enter) -> 
      BlockStack.enter address stack

    | Token (address, TokenType.T.Branch Demarcation.Exit) -> 
      BlockStack.leave address stack

    | Token (address, trivial) -> 
      let instruction = 
        translateTrivial trivial
        |> StreamEntry.make address
      BlockStack.add instruction stack

    List.fold translateOne BlockStack.empty
    >> BlockStack.joinStreams
    >> insertHalt

module Tokenizer =

  let tokenize : string -> Result<Token list, Error.T>  =

    let parseToken address = function
    | TokenType.Recognized token -> 
      Ok <| Token (address, token)
    | unrecognized -> 
      (address, unrecognized) 
      |> (Error.UnrecognizedToken >> Error)

    Result.traversei parseToken
    << List.ofSeq

module Memory = 
  type 'a T = Mem of 'a * Map<Address.T, 'a>

  let make zero = Mem (zero, Map.empty)

  let ofList zero list =
    Mem (zero, Map.ofList list)

  let read at = function
  | Mem (zero, data) ->
      Map.tryFind at data
      |> Option.defaultValue zero

  let write at value = function
  | Mem (zero, data) ->
    Mem (zero, Map.add at value data)

  let update at (f: 'a -> 'a) memory = 
    let x = read at memory |> f
    write at x memory

module AbstractMachine =
  let todo<'a> : 'a = failwith "Doh!"

  type State =
    { instructionPointer: Address.T
      dataPointer:        Address.T
      instructions:       Instruction.T Memory.T
      data:               int Memory.T
      continuation:       Continuation
    }
  and Continuation =
    | Goto of Address.T
    | Next
    | Halt

  module Continuation =
    let continueWith continuation state =
      { state with
          continuation = continuation
      }

    let next = continueWith Next

    let goto = continueWith << Goto

    let halt = continueWith Halt

  module Lift =
    let ifM p t f s =
      if p s then t s else f s

  module State =
    type Data = int Memory.T

    let mapDataPointer (f: Address.T -> Address.T) state =
      { state with 
          dataPointer = f state.dataPointer
      }

    let mapData (f: int -> int) state =
      { state with
          data = Memory.update state.dataPointer f state.data
      }

    let mapInstructionPointer (f: Address.T -> Address.T) state =
      { state with 
          instructionPointer = f state.instructionPointer
      }

    let unfoldContinuation = function 
    | { continuation = Goto target } as state ->
      mapInstructionPointer (Function.konst target) state
      |> Continuation.next
      |> Some

    | { continuation = Next } as state ->
      mapInstructionPointer Address.after state
      |> Some

    | { continuation = Halt } ->
      None

    let runAutomat (automat: State -> State) state =
      match unfoldContinuation state with
      | Some state' -> automat state'
      | None        -> state

    let decodeInstruction { instructionPointer = ip
                            instructions       = code } =
      Memory.read ip code

    let data { dataPointer = dp
               data        = data } =
      Memory.read dp data


  module Loader =

    let bootstrap program =
      let codeMap =
        Memory.ofList Instruction.Halt program

      { instructionPointer = Address.make 0
        dataPointer        = Address.make 0
        instructions       = codeMap
        data               = Memory.make 0
        continuation       = Next
      }

    let show stream =
      stream
      |> List.sortBy (fun (address, _) -> address)
      |> List.map Parser.StreamEntry.show
      |> String.concat "\n"

    let tryLoadProgram =
      Tokenizer.tokenize
      >> Result.map Parser.translate

  module Bios =
    let input = 
      Console.Read 
      >> Function.konst
      >> State.mapData 

    let output state =
      State.data state
      |> (char >> printf "%c")

      state

  module Intrinsic =
    let direction = function
    | Direction.Left  -> fun x -> x - 1
    | Direction.Right -> (+) 1      

    let change = function
    | Change.Decrement -> fun x -> x - 1
    | Change.Increment -> (+) 1

    let evaluate = function
    | Condition.NonZero -> (<>) 0
    | Condition.Zero    -> (=)  0

    let runIo = function
    | Interaction.Input  -> Bios.input ()
    | Interaction.Output -> Bios.output


  module Interpreter =

    let applyInstruction = function
    | Instruction.Halt ->
      Continuation.halt

    | Instruction.Move move ->
      State.mapDataPointer <| Intrinsic.direction move

    | Instruction.Update update ->
      State.mapData <| Intrinsic.change update

    | Instruction.Interact io ->
      Intrinsic.runIo io

    | Instruction.Jump (target, givenThat) ->
      let predicate = 
        State.data >> Intrinsic.evaluate givenThat

      Lift.ifM predicate
      <| Continuation.goto target 
      <| Continuation.next

    (* This thing is not tail recursive so overflows the stack. *)
    let rec runLoop state =
//      printfn "IP: %d; DP: %d; D: %d" 
//        state.instructionPointer
//        state.dataPointer
//        (State.data state)

      State.decodeInstruction state
      |> fun ins -> applyInstruction ins state
      |> State.runAutomat runLoop

    let start =
      Loader.bootstrap
      >> runLoop