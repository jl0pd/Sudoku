namespace Sudoku

open Sudoku.Extensions

[<AutoOpen>]
module Functools =
    let flip f x y = f y x

    let rec repeat n f x =
        match n with
        | 0 -> x
        | _ -> repeat (n - 1) f (f x)

    let rec applyAll x =
        function
        | f :: fs -> applyAll (f x) fs
        | [] -> x

module Sudoku =
    open System.Text

    type Cell =
        { X: int
          Y: int
          Digit: int
          IsHidden: bool
          IsUnchangeable: bool }

    type Field =
        { Cells: Cell [,] }
        override f.ToString() =
            let c = f.Cells
            let w, h = c.GetLength(0), c.GetLength(1)
            let builder = StringBuilder()
            for y = 0 to h - 1 do
                for x = 0 to w - 1 do
                    let { IsHidden = isHidden; Digit = digit } = c.[y, x]
                    builder.Append(if isHidden then " " else string digit)
                    |> ignore
                builder.AppendLine() |> ignore
            builder.ToString()

    let openAll { Cells = cells } =
        { Cells = Array2D.map (fun c -> { c with IsHidden = false }) cells }

    let loadField (hiddenMap: bool [,]) (digitsMap: int [,]): Field =
        let cells =
            Array2D.zip hiddenMap digitsMap
            |> Array2D.mapi (fun x y (h, d) ->
                { X = x
                  Y = y
                  Digit = d
                  IsHidden = h
                  IsUnchangeable = not h })

        { Cells = cells }

    let rank { Cells = cells } =
        cells.GetLength(0) |> float |> sqrt |> int

    let inline private square x = x * x

    let size = square << rank

module SudokuGenerator =
    type FieldBuilder = { Cells: int [,] }

    type RandRange = int -> int -> int

    let size { Cells = c } = c.GetLength(0)
    let rank { Cells = c } = c.GetLength(0) |> float |> sqrt |> int

    let createBaseField (rank: int): FieldBuilder =
        let size = rank * rank

        let cells =
            Array2D.init size size (fun y x -> (y * rank + y / rank + x) % size)

        { Cells = cells }

    let transpose (rand: RandRange) (field: FieldBuilder): FieldBuilder =
        let cells =
            field.Cells
            |> Array2D.toArrayOfArrays
            |> Array.transpose
            |> Array2D.fromArrayOfArrays

        { Cells = cells }

    let private applyTwice f x = f x, f x
    let private applyTriple f x = f x, f x, f x

    let swapRows (rand: RandRange) (field: FieldBuilder): FieldBuilder =
        let r = rank field
        let groupNumber, first, second = applyTriple (rand 0) r

        let newCells =
            Array2D.copy field.Cells
            |> Array2D.swapRows (r * groupNumber + first) (r * groupNumber + second)

        { Cells = newCells }

    let swapCols (rand: RandRange) (field: FieldBuilder): FieldBuilder =
        let r = rank field
        let groupNumber, first, second = applyTriple (rand 0) r

        let newCells =
            Array2D.copy field.Cells
            |> Array2D.swapCols (r * groupNumber + first) (r * groupNumber + second)

        { Cells = newCells }

    let swapRegionCols (rand: RandRange) (field: FieldBuilder): FieldBuilder =
        let upperBound = rank field
        let first, second = applyTwice (rand 0) upperBound

        let newCells = Array2D.copy field.Cells

        for x = 0 to upperBound - 1 do
            newCells
            |> Array2D.swapCols (first * upperBound + x) (second * upperBound + x)
            |> ignore

        { Cells = newCells }

    let swapRegionRows (rand: RandRange) (field: FieldBuilder): FieldBuilder =
        let upperBound = rank field
        let first, second = applyTwice (rand 0) upperBound

        let newCells = Array2D.copy field.Cells

        for y = 0 to upperBound - 1 do
            newCells
            |> Array2D.swapRows (first * upperBound + y) (second * upperBound + y)
            |> ignore

        { Cells = newCells }

    module Default =
        let rand = System.Random()
        let randRange start stop = rand.Next(start, stop)

        let transpose = transpose randRange
        let swapRows = swapRows randRange
        let swapCols = swapCols randRange
        let swapRegionCols = swapRegionCols randRange
        let swapRegionRows = swapRegionRows randRange

module SudokuGame =
    open Sudoku

    open SudokuGenerator
    open SudokuGenerator.Default

    type State =
        { IsSolved: bool
          Rank: int
          OriginalField: Field
          PlayerField: Field
          HighlightEnabled: bool
          SelectedCell: int * int }

    let createRandomField rank: Field =
        let f = createBaseField rank

        let field =
            { Cells = if rank <= 3 then f.Cells |> Array2D.map ((+) 1) else f.Cells }

        let operations =
            [ swapRegionCols
              swapRows
              swapRegionRows
              transpose
              swapCols ]

        let size = size field

        let rand = System.Random()

        let hiddenMap =
            Array2D.init size size (fun x y -> rand.Next(0, 100) > 30)

        let f =
            repeat 50 (fun f -> applyAll f operations) field

        let m =
            repeat 10 (fun f -> applyAll f operations) ({ Cells = Array2D.map (fun b -> if b then 1 else 0) hiddenMap })

        Sudoku.loadField (Array2D.map ((<>) 0) m.Cells) f.Cells

    let init =
        let f = createRandomField 3
        { IsSolved = false
          Rank = 3
          OriginalField = openAll f
          PlayerField = f
          HighlightEnabled = true
          SelectedCell = 0, 0 }

    let newGame ({ Rank = rank } as state) =
        let f = createRandomField rank
        { state with
              IsSolved = false
              OriginalField = openAll f
              PlayerField = f
              SelectedCell = 0, 0 }

    let isSolved { Field.Cells = c1 } { Field.Cells = c2 } =
        Array2D.zip c1 c2
        |> Array2D.toSeq
        |> Seq.forall (fun (x, y) -> x.Digit = y.Digit && x.IsHidden = y.IsHidden)

    type Message =
        | NewGame
        | RankChanged of int
        | CellSelected of int * int
        | HighlightChanged of bool
        | Digit of int

    let update (msg: Message) (state: State): State =
        match msg with
        | NewGame -> newGame state
        | HighlightChanged ->
            { state with
                  HighlightEnabled = not state.HighlightEnabled }
        | RankChanged d -> { state with Rank = d }
        | CellSelected (x, y) -> { state with SelectedCell = (x, y) }
        | Digit d ->
            if state.IsSolved
               || state.PlayerField.Cells.[fst state.SelectedCell, snd state.SelectedCell].IsUnchangeable then
                state
            else
                let newCells = Array2D.copy state.PlayerField.Cells
                let x, y = state.SelectedCell
                let oldCell = newCells.[x, y]

                newCells.[x, y] <- if d = oldCell.Digit then
                                       { oldCell with
                                             IsHidden = not oldCell.IsHidden }
                                   else
                                       { oldCell with
                                             Digit = d
                                             IsHidden = false }

                let newField = { Field.Cells = newCells }
                { state with
                      PlayerField = newField
                      IsSolved = isSolved state.OriginalField newField }
