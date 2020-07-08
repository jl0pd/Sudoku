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
          IsHidden: bool }

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
                  IsHidden = h })

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
          SelectedCell: int * int }

    let createRandomField rank: Field =
        let f = createBaseField rank
        let field = { Cells = if rank <= 3 then
                                f.Cells
                                |> Array2D.map ((+) 1)
                               else
                                f.Cells }

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
            repeat 50 (fun f -> applyAll f operations) ({ Cells = Array2D.map (fun b -> if b then 1 else 0) hiddenMap })

        Sudoku.loadField (Array2D.map ((<>) 0) m.Cells) f.Cells

    let init =
        let f = createRandomField 3
        { IsSolved = false
          Rank = 3
          OriginalField = openAll f
          PlayerField = f
          SelectedCell = 0, 0 }

    let newGame rank =
        let f = createRandomField rank
        { IsSolved = false
          Rank = rank
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
        | Digit of int

    let update (msg: Message) (state: State): State =
        match msg with
        | NewGame -> newGame state.Rank
        | RankChanged d -> { state with Rank = d }
        | CellSelected (x, y) -> { state with SelectedCell = (x, y) }
        | Digit d ->
            if state.IsSolved then
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

module View =
    open Avalonia
    open Avalonia.Media
    open Avalonia.Layout
    open Avalonia.Controls
    open Avalonia.Controls.Primitives
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Helpers

    open Sudoku
    open SudokuGame

    let private intToString = sprintf "%X"

    let private split ({ Cells = cells } as field): Cell seq seq =
        let r = rank field

        seq {
            for gy = 0 to r - 1 do
                for gx = 0 to r - 1 do
                    seq {
                        for iy = 0 to r - 1 do
                            for ix = 0 to r - 1 do
                                let x, y = (gx * r + ix), (gy * r + iy)
                                cells.[y, x]
                    }
        }

    let private fieldView (state: State) (dispatch: Message -> unit) =
        UniformGrid.create
            [ UniformGrid.columns (Sudoku.rank state.OriginalField)
              UniformGrid.children
                  (state.PlayerField
                   |> split
                   |> Seq.map (fun group ->
                       Border.create
                           [ Border.borderThickness 1.
                             Border.borderBrush Brushes.Black

                             Border.child
                                 (UniformGrid.create
                                     [ UniformGrid.columns (Sudoku.rank state.OriginalField)
                                       UniformGrid.children
                                           (group
                                            |> Seq.map (fun cell ->
                                                let { X = x; Y = y; Digit = digit; IsHidden = isHidden } = cell
                                                Button.create
                                                    [ Button.minWidth 30.
                                                      Button.minHeight 30.
                                                      Button.borderThickness 1.
                                                      Button.borderBrush Brushes.Black
                                                      Button.foreground Brushes.Black
                                                      Button.fontSize 20.
                                                      Button.padding 0.
                                                      Button.fontWeight FontWeight.DemiBold

                                                      Button.background
                                                          (if state.IsSolved then Brushes.LightGreen
                                                           elif (x, y) = state.SelectedCell then Brushes.LightCyan
                                                           else Brushes.LightGray)
                                                      Button.content (if isHidden then "" else intToString digit)
                                                      Button.onClick (fun _ -> dispatch <| CellSelected(x, y)) ]
                                                |> generalize)
                                            |> List.ofSeq) ]) ]
                       |> generalize)
                   |> List.ofSeq) ]

    let private digitsPanelView (state: State) (dispatch: Message -> unit) =
        let size = Sudoku.size state.OriginalField
        UniformGrid.create
            [ UniformGrid.rows 1
              UniformGrid.margin (Thickness.Parse "0,10")
              UniformGrid.dock Dock.Bottom
              UniformGrid.minHeight 40.
              UniformGrid.children
                  (size
                   |> flip Seq.init (fun i ->
                          let n = if size <= 9 then i + 1 else i
                          Button.create
                              [ Button.borderThickness 1.
                                Button.borderBrush Brushes.Black
                                Button.foreground Brushes.Black
                                Button.fontSize 20.
                                Button.padding 0.
                                Button.fontWeight FontWeight.DemiBold
                                Button.background Brushes.WhiteSmoke

                                Button.content (intToString n)
                                Button.onClick (fun _ -> if not state.IsSolved then dispatch <| Digit n) ]
                          |> generalize)
                   |> List.ofSeq) ]

    let private menuView (state: State) (dispatch: Message -> unit) =
        StackPanel.create
            [ StackPanel.orientation Orientation.Horizontal
              StackPanel.dock Dock.Top

              StackPanel.children
                  [ NumericUpDown.create
                      [ NumericUpDown.increment 1.
                        NumericUpDown.minimum 1.
                        NumericUpDown.fontSize 16.
                        NumericUpDown.width 80.
                        NumericUpDown.height 50.

                        NumericUpDown.value (float state.Rank)
                        NumericUpDown.onValueChanged (dispatch << RankChanged << int) ]
                    Button.create
                        [ Button.margin (Thickness.Parse "0,10")
                          Button.fontSize 16.
                          Button.horizontalAlignment HorizontalAlignment.Center

                          Button.content "New game"
                          Button.onClick (fun _ -> dispatch NewGame) ] ] ]

    let view (state: State) (dispatch: Message -> unit) =
        DockPanel.create
            [ DockPanel.minHeight 200.
              DockPanel.minWidth 200.
              //   DockPanel.maxHeight 600.
              //   DockPanel.maxWidth 600.

              DockPanel.children
                  [ menuView state dispatch
                    digitsPanelView state dispatch
                    fieldView state dispatch ] ]
