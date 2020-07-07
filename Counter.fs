﻿namespace Sudoku

[<AutoOpen>]
module Functools =
    let flip f x y = f y x

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array2D =
    let flat (arr: 'a [,]): 'a [] =
        [| for x = 0 to (Array2D.length1 arr) - 1 do
            for y = 0 to (Array2D.length2 arr) - 1 do
                yield arr.[x, y] |]

    let toSeq ar =
        seq {
            for x = 0 to (Array2D.length1 ar) - 1 do
                for y = 0 to (Array2D.length2 ar) - 1 do
                    yield ar.[x, y]
        }

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

module SudokuGame =
    open Sudoku

    type State =
        { IsSolved: bool
          OriginalField: Field
          PlayerField: Field
          SelectedCell: int * int }

    let defaultDigits =
        Array2D.fromArrayOfArrays
            [| [| 0; 1; 2; 3 |]
               [| 3; 0; 1; 2 |]
               [| 2; 3; 0; 1 |]
               [| 1; 2; 3; 0 |] |]

    let defaultHidden =
        Array2D.fromArrayOfArrays
            [| [| false; false; false; true |]
               [| false; false; false; true |]
               [| false; false; false; true |]
               [| false; false; false; true |] |]

    let defaultField =
        Sudoku.loadField defaultHidden defaultDigits

    let init =
        { IsSolved = false
          OriginalField = Sudoku.loadField (Array2D.map (fun _ -> false) defaultDigits) defaultDigits
          PlayerField = defaultField
          SelectedCell = 0, 0 }

    let isSolved { Cells = c1 } { Cells = c2 } =
        Array2D.zip c1 c2
        |> Array2D.toSeq
        |> Seq.forall (fun (x, y) -> x.Digit = y.Digit && x.IsHidden = y.IsHidden)

    type Message =
        | NewGame
        | CellSelected of int * int
        | Digit of int

    let update (msg: Message) (state: State): State =
        match msg with
        | NewGame -> init
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

                let newField = { Cells = newCells }
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

    let private intToString =
        function
        | n when n >= 0 && n <= 9 -> string n
        | n -> sprintf "??%d" n

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
                   |> Seq.map (fun g ->
                       Border.create
                           [ Border.borderThickness 1.
                             Border.borderBrush Brushes.Red

                             Border.child
                                 (UniformGrid.create
                                     [ UniformGrid.columns (Sudoku.rank state.OriginalField)
                                       UniformGrid.children
                                           (g
                                            |> Seq.map (fun c ->
                                                let { X = x; Y = y } = c
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
                                                      Button.content (if c.IsHidden then "" else intToString c.Digit)
                                                      Button.onClick (fun _ -> dispatch <| CellSelected(x, y)) ]
                                                |> generalize)
                                            |> List.ofSeq) ]) ]
                       |> generalize)
                   |> List.ofSeq) ]

    let private digitsPanelView (state: State) (dispatch: Message -> unit) =
        UniformGrid.create
            [ UniformGrid.rows 1
              UniformGrid.margin (Thickness.Parse "0,10")
              UniformGrid.dock Dock.Bottom
              UniformGrid.minHeight 40.
              UniformGrid.children
                  (Sudoku.size state.OriginalField
                   |> flip Seq.init (fun i ->
                          Button.create
                              [ Button.borderThickness 1.
                                Button.borderBrush Brushes.Black
                                Button.foreground Brushes.Black
                                Button.fontSize 20.
                                Button.padding 0.
                                Button.fontWeight FontWeight.DemiBold
                                Button.background Brushes.WhiteSmoke

                                Button.content (intToString i)
                                Button.onClick (fun _ -> if not state.IsSolved then dispatch <| Digit i) ]
                          |> generalize)
                   |> List.ofSeq) ]

    let view (state: State) (dispatch: Message -> unit) =
        DockPanel.create
            [ DockPanel.minHeight 200.
              DockPanel.minWidth 200.
              DockPanel.maxHeight 600.
              DockPanel.maxWidth 600.

              DockPanel.children
                  [ Button.create
                      [ Button.margin (Thickness.Parse "0,10")
                        Button.dock Dock.Top
                        Button.fontSize 16.
                        Button.horizontalAlignment HorizontalAlignment.Center

                        Button.content "New game"
                        Button.onClick (fun _ -> dispatch NewGame) ]
                    digitsPanelView state dispatch
                    fieldView state dispatch ] ]
