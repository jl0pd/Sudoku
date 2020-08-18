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

    let private fieldView (state: State) (dispatch: Message -> unit) =
        let createButton state ({ Digit = digit; IsHidden = isHidden } as cell) =
            let bg =
                (if state.IsSolved then
                    Brushes.LightGreen
                 elif state.SelectedCell.IsSome
                      && cell.Coords = state.SelectedCell.Value.Coords then
                     Brushes.LightCyan
                 elif state.HighlightEnabled
                      && state.SelectedCell.IsSome
                      && not state.SelectedCell.Value.IsHidden
                      && digit = state.SelectedCell.Value.Digit
                      && not isHidden then
                     Brushes.LightYellow
                 else
                     Brushes.LightGray)

            generalize
                (Button.create
                    [ Button.minWidth 30.
                      Button.minHeight 30.
                      Button.borderThickness 1.
                      Button.borderBrush Brushes.Black
                      Button.foreground (if cell.IsUnchangable then Brushes.Black else Brushes.DarkSlateGray)
                      Button.fontSize 20.
                      Button.padding 0.
                      Button.fontWeight (if cell.IsUnchangable then FontWeight.Bold else FontWeight.DemiBold)

                      Button.background bg
                      Button.content (if isHidden then "" else intToString digit)
                      Button.onClick ((fun _ -> dispatch <| CellSelected cell), Always) ])

        let groups =
            (state.PlayerField
             |> getGroups
             |> Seq.map (fun group ->
                 generalize
                     (Border.create
                         [ Border.borderThickness 1.
                           Border.borderBrush Brushes.Orange

                           Border.child
                               (UniformGrid.create
                                   [ UniformGrid.columns (rank state.OriginalField)
                                     UniformGrid.children (group |> Seq.map (createButton state) |> List.ofSeq) ]) ]))
             |> List.ofSeq)

        UniformGrid.create
            [ UniformGrid.columns (rank state.OriginalField)
              UniformGrid.children groups ]

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
