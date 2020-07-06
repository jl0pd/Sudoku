namespace Sudoku

[<AutoOpen>]
module Functools =
    let flip f x y = f y x

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array2D =
    let flat (arr: 'a [,]): 'a [] =
        [| for x = 0 to (Array2D.length1 arr) - 1 do
            for y = 0 to (Array2D.length2 arr) - 1 do
                yield arr.[x, y] |]

    let fromArrayOfArrays (ar: 'a [] []): 'a [,] =
        let h, w = ar.Length, ar.[0].Length

        let newAr = Array2D.zeroCreate h w

        Array.iteri (fun y row -> Array.iteri (fun x el -> Array2D.set newAr y x el) row) ar
        newAr

    let zip (ar1: 'a [,]) (ar2: 'b [,]): ('a * 'b) [,] =
        let w1, h1 = ar1.GetLength(0), ar1.GetLength(1)
        let w2, h2 = ar2.GetLength(0), ar2.GetLength(1)

        if (w1, h1) <> (w2, h2) then failwith "Arrays have different size"

        [| for y = 0 to h1 - 1 do
            [| for x = 0 to w1 - 1 do
                ar1.[y, x], ar2.[y, x] |] |]
        |> fromArrayOfArrays

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
                    let cc = c.[y, x]
                    builder.Append(if cc.IsHidden then " " else string cc.Digit)
                    |> ignore
                builder.AppendLine() |> ignore
            builder.ToString()

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

    let isSolved { Cells = cells } =
        let isAllOpen =
            cells
            |> Array2D.flat
            |> Array.forall (fun c -> not c.IsHidden)

        isAllOpen

module SudokuGame =
    open Sudoku

    type State =
        { IsSolved: bool
          OriginalField: Field
          PlayerField: Field
          SelectedCell: int * int }

    let defaultDigits = Array2D.init 9 9 (fun y x -> y * 10 + x)
    let defaultHidden = Array2D.zeroCreate 9 9

    let defaultField =
        Sudoku.loadField defaultHidden defaultDigits

    let init =
        { IsSolved = false
          OriginalField = defaultField
          PlayerField = defaultField
          SelectedCell = 0, 0 }

    type Message =
        | NewGame
        | CellSelected of int * int
        | Digit of int

    let update (msg: Message) (state: State): State =
        match msg with
        | NewGame -> init
        | CellSelected (x, y) -> { state with SelectedCell = (x, y) }
        | Digit d ->
            let newCells = Array2D.copy state.PlayerField.Cells
            let y, x = state.SelectedCell
            newCells.[y, x] <- { newCells.[y, x] with Digit = d }
            let newField = { Cells = newCells }
            { state with
                  PlayerField = newField
                  IsSolved = Sudoku.isSolved newField }

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

    let private fieldView (state: State) (dispatch: Message -> unit) =
        UniformGrid.create
            [ UniformGrid.columns (Sudoku.size state.OriginalField)
              UniformGrid.children
                  (state.PlayerField.Cells
                   |> Array2D.flat
                   |> Array.map (fun c ->
                       let { X = x; Y = y } = c
                       Button.create
                           [ Button.borderThickness 1.
                             Button.borderBrush Brushes.Black
                             Button.background
                                 (if (x, y) = state.SelectedCell then Brushes.LightCyan else Brushes.LightGray)
                             Button.content c.Digit
                             Button.foreground Brushes.Black
                             Button.onClick (fun _ -> dispatch <| CellSelected(x, y)) ]
                       |> generalize)
                   |> Array.toList) ]

    let private intToString =
        function
        | n when n >= 0 && n <= 9 -> string n
        | n -> sprintf "??%d" n

    let private digitsPanelView (state: State) (dispatch: Message -> unit) =
        UniformGrid.create
            [ UniformGrid.rows 1
              UniformGrid.margin (Thickness.Parse "0,10")
              UniformGrid.children
                  (Sudoku.size state.OriginalField
                   |> flip Seq.init (fun i ->
                          Button.create [
                              Button.content (intToString i)
                              Button.onClick (fun _ ->
                                    dispatch <| Digit i) ]
                          |> generalize)
                   |> List.ofSeq) ]

    let view (state: State) (dispatch: Message -> unit) =
        StackPanel.create
            [ StackPanel.horizontalAlignment HorizontalAlignment.Center
              StackPanel.verticalAlignment VerticalAlignment.Center

              StackPanel.children
                  [ fieldView state dispatch
                    digitsPanelView state dispatch ] ]
