namespace Sudoku.Avalonia


module View =
    open Avalonia
    open Avalonia.Media
    open Avalonia.Layout
    open Avalonia.Controls
    open Avalonia.Controls.Primitives
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Helpers

    open Sudoku.Functools
    open Sudoku.Sudoku
    open Sudoku.SudokuGame

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
        let selectedCell =
            state.PlayerField.Cells.[fst state.SelectedCell, snd state.SelectedCell]

        UniformGrid.create
            [ UniformGrid.columns (rank state.OriginalField)
              UniformGrid.children
                  (state.PlayerField
                   |> split
                   |> Seq.map (fun group ->
                       Border.create
                           [ Border.borderThickness 1.
                             Border.borderBrush Brushes.Orange

                             Border.child
                                 (UniformGrid.create
                                     [ UniformGrid.columns (rank state.OriginalField)
                                       UniformGrid.children
                                           (group
                                            |> Seq.map (fun cell ->
                                                let { X = x; Y = y; Digit = digit; IsHidden = isHidden } = cell
                                                Button.create
                                                    [ Button.minWidth 30.
                                                      Button.minHeight 30.
                                                      Button.borderThickness 1.
                                                      Button.borderBrush Brushes.Black
                                                      Button.foreground
                                                        (if cell.IsUnchangable then Brushes.Black
                                                         else Brushes.DarkSlateGray)
                                                      Button.fontSize 20.
                                                      Button.padding 0.
                                                      Button.fontWeight
                                                          (if cell.IsUnchangable then
                                                               FontWeight.Bold
                                                           else
                                                               FontWeight.DemiBold)

                                                      Button.background
                                                          (if state.IsSolved then
                                                              Brushes.LightGreen
                                                           elif (x, y) = state.SelectedCell then
                                                               Brushes.LightCyan
                                                           elif state.HighlightEnabled
                                                                && not selectedCell.IsHidden
                                                                && cell.Digit = selectedCell.Digit
                                                                && not cell.IsHidden then
                                                               Brushes.LightYellow
                                                           else
                                                               Brushes.LightGray)
                                                      Button.content (if isHidden then "" else intToString digit)
                                                      Button.onClick (fun _ -> dispatch <| CellSelected(x, y)) ]
                                                |> generalize)
                                            |> List.ofSeq) ]) ]
                       |> generalize)
                   |> List.ofSeq) ]

    let private digitsPanelView (state: State) (dispatch: Message -> unit) =
        let fieldSize = size state.OriginalField
        UniformGrid.create
            [ UniformGrid.rows 1
              UniformGrid.margin (Thickness.Parse "0,10")
              UniformGrid.dock Dock.Bottom
              UniformGrid.minHeight 40.
              UniformGrid.children
                  (fieldSize
                   |> flip Seq.init (fun i ->
                          let n = if fieldSize <= 9 then i + 1 else i
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
              StackPanel.height 36.

              StackPanel.children
                  [ NumericUpDown.create
                      [ NumericUpDown.increment 1.
                        NumericUpDown.minimum 1.
                        NumericUpDown.fontSize 16.
                        NumericUpDown.width 80.
                        NumericUpDown.margin (Thickness.Parse "10,2")
                        NumericUpDown.padding 0.

                        NumericUpDown.value (float state.Rank)
                        NumericUpDown.onValueChanged (dispatch << RankChanged << int) ]

                    Button.create
                        [ Button.margin (Thickness.Parse "10,2")
                          Button.fontSize 16.
                          Button.horizontalAlignment HorizontalAlignment.Center

                          Button.content "New game"
                          Button.onClick (fun _ -> dispatch NewGame) ]

                    CheckBox.create
                        [ CheckBox.margin (Thickness.Parse "10,2")
                          CheckBox.content "Highlight"
                          CheckBox.isThreeState false
                          CheckBox.fontSize 16.

                          CheckBox.isChecked state.HighlightEnabled
                          CheckBox.onChecked (fun _ -> dispatch <| HighlightChanged true)
                          CheckBox.onUnchecked (fun _ -> dispatch <| HighlightChanged false) ] ] ]

    let view (state: State) (dispatch: Message -> unit) =
        DockPanel.create
            [ DockPanel.minHeight 200.
              DockPanel.minWidth 200.

              DockPanel.children
                  [ menuView state dispatch
                    digitsPanelView state dispatch
                    fieldView state dispatch ] ]
