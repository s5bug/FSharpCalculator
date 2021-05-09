namespace FSharpCalculator

open System.Text.RegularExpressions
open FSharpCalculator.Core

/// This is the main module of your application
/// here you handle all of your child pages as well as their
/// messages and their updates, useful to update multiple parts
/// of your application, Please refer to the `view` function
/// to see how to handle different kinds of "*child*" controls
module Shell =
    open Elmish
    open Avalonia
    open Avalonia.Controls
    open Avalonia.Input
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Builder
    open Avalonia.FuncUI.Components.Hosts
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Elmish

    type State =
        { inputString: option<string> }

    type Msg =
        | ButtonPress of string

    let init =
        { inputString = None }, Cmd.none

    let update (msg: Msg) (state: State): State * Cmd<_> =
        match msg with
        | ButtonPress button ->
            if button = "=" then
                match state.inputString with
                | Some x ->
                    let tokenize = Calculator.tokenize x
                    let parse = Calculator.parse tokenize
                    let eval = Calculator.evaluate parse
                    { inputString = Option.map string eval }, Cmd.none
                | None -> state, Cmd.none
            elif Regex.IsMatch(button, "\\d|\\.") then
                match state.inputString with
                | None | Some "0" ->
                    { inputString = Some button }, Cmd.none
                | Some x ->
                    if Regex.IsMatch(x.Substring(x.Length - 1, 1), "\\d|\\.") then
                        if "." = x.Substring(x.Length - 1, 1) && "." = button then
                            state, Cmd.none
                        else
                            { inputString = Some (x + button) }, Cmd.none
                    else
                        { inputString = Some (x + " " + button) }, Cmd.none
            else
                match state.inputString with
                | Some x ->
                    if Regex.IsMatch(x.Substring(x.Length - 1, 1), "\\d|\\.") then
                        { inputString = Some (x + " " + button) }, Cmd.none
                    else
                        state, Cmd.none
                | None ->
                    { inputString = Some ("0 " + button) }, Cmd.none
    
    
    let makeButton (dispatch) (row: int) (column: int) (text: string) : Types.IView =
        upcast Button.create
            [ Grid.row row
              Grid.column column
              Button.content text
              Button.onClick (fun _ -> dispatch (ButtonPress text))
              Button.classes ["calcButton"] ]
    
    let calculatorOneThroughNine (dispatch) : seq<Types.IView> =
        seq {
            for dig in 0..8 do
                let text = string (dig + 1)
                let column = dig % 3
                let row = dig / 3
                yield makeButton dispatch row column text
        }
    
    let zeroButton (dispatch) : Types.IView =
        makeButton dispatch 3 0 "0"
    
    let decimalButton (dispatch) : Types.IView =
        makeButton dispatch 3 1 "."
    
    let equalsButton (dispatch) : Types.IView =
        makeButton dispatch 3 2 "="
    
    let makeOperator (dispatch) (row : int) (text: string) : Types.IView =
        makeButton dispatch row 3 text
    
    let operatorButtons (dispatch) : seq<Types.IView> =
        Seq.ofList ["+"; "-"; "*"; "/"]
        |> Seq.mapi (makeOperator dispatch)
        
    let calculatorButtons (dispatch) : list<Types.IView> =
        let buttonsSeq = seq {
            yield! calculatorOneThroughNine dispatch
            yield zeroButton dispatch
            yield decimalButton dispatch
            yield equalsButton dispatch
            yield! operatorButtons dispatch
        }
        List.ofSeq buttonsSeq
    
    let view (state: State) (dispatch) =
        DockPanel.create
            [ DockPanel.children
                [ TextBlock.create
                    [ TextBlock.dock Dock.Top
                      TextBlock.text (Option.defaultValue "0" state.inputString)
                      TextBlock.classes ["resultText"] ]
                  Grid.create
                    [ Grid.rowDefinitions "1*,1*,1*,1*"
                      Grid.columnDefinitions "1*,1*,1*,1*"
                      Grid.children (calculatorButtons dispatch) ] ] ]

    /// This is the main window of your application
    /// you can do all sort of useful things here like setting heights and widths
    /// as well as attaching your dev tools that can be super useful when developing with
    /// Avalonia
    type MainWindow() as this =
        inherit HostWindow()
        do
            base.Title <- "Calculator"
            base.Width <- 400.0
            base.Height <- 600.0
            base.MinWidth <- 400.0
            base.MinHeight <- 600.0

            //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
            //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

            Elmish.Program.mkProgram (fun () -> init) update view
            |> Program.withHost this
            |> Program.run