namespace Sudoku

open System.Text

module Counter =

    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout

    type State = { count: int }
    let init = { count = 0 }

    type Msg =
        | Increment
        | Decrement
        | Reset

    let update (msg: Msg) (state: State): State =
        match msg with
        | Increment -> { state with count = state.count + 1 }
        | Decrement -> { state with count = state.count - 1 }
        | Reset -> init

    let view (state: State) (dispatch) =
        DockPanel.create
            [ DockPanel.children
                [ Button.create
                    [ Button.dock Dock.Bottom
                      Button.onClick (fun _ -> dispatch Reset)
                      Button.content "reset" ]
                  Button.create
                      [ Button.dock Dock.Bottom
                        Button.onClick (fun _ -> dispatch Decrement)
                        Button.content "-" ]
                  Button.create
                      [ Button.dock Dock.Bottom
                        Button.onClick (fun _ -> dispatch Increment)
                        Button.content "+" ]
                  TextBlock.create
                      [ TextBlock.dock Dock.Top
                        TextBlock.fontSize 48.0
                        TextBlock.verticalAlignment VerticalAlignment.Center
                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                        TextBlock.text (string state.count) ] ] ]


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array2D =
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

module SudokuGame =
    open Sudoku

    type State = { IsSolved: bool; Field: Field }
