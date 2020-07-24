module Client

open Elmish
open Elmish.Debug
open Elmish.React
open Elmish.HMR
open Fable.React
open Fable.React.Props
open Fable.DateFunctions
open System

/// A type that stores a raw value and a "parsed" version as 'T. In this demo, we use the "raw"
/// value to bind back again to the input control, whilst storing the "parsed" value as an option.
/// The alternative is to simply store the DateTime as an option and in the view "regenerate" the
/// raw string based on the DateTime value.
type RawOption<'T> =
    { Raw : string
      Parsed : 'T option }
module RawOption =
    let empty = { Raw = ""; Parsed = None }
    let tryParse parser v =
        { Raw = v
          Parsed = try v |> parser with _ -> None }

type Model =
    { IndirectDate : DateTime RawOption
      DirectDate : DateTime option }

type Msg =
    | IndirectDateChanged of DateTime RawOption
    | DirectDateChanged of DateTime option

let init() =
    { IndirectDate = RawOption.empty; DirectDate = None }, Cmd.none

/// Safely parses a string into a DateTime
let safeParse = function "" | null -> None | s -> try DateTime.Parse s |> Some with | _ -> None
/// Converts a DateTime into a string suitable for the date input control. Uses the date-fns package.
let toDateFormat (d:DateTime) = d.Format "yyyy-MM-dd"

let update msg model =
    match msg with
    | IndirectDateChanged v -> { model with IndirectDate = v }, Cmd.none
    | DirectDateChanged v -> { model with DirectDate = v }, Cmd.none

let view model dispatch =
    div [ Style [ TextAlign TextAlignOptions.Center; Padding 40 ] ] [
        h2 [] [ str "INDIRECT (VIA STRING)" ]
        div [] [ str ("MODEL STRING: " + model.IndirectDate.Raw) ]
        div [] [ str ("MODEL DATETIME: " + (model.IndirectDate.Parsed |> Option.map string |> Option.defaultValue "(NONE)")) ]
        div [] [
            input [
                HTMLAttr.Type "date"
                Value model.IndirectDate.Raw
                OnChange (fun v -> v.Value |> RawOption.tryParse safeParse |> IndirectDateChanged |> dispatch) ]
        ]

        h2 [] [ str "DIRECT (DATETIME)" ]
        div [] [ str ("MODEL DATETIME (SERIALIZED): " + (model.DirectDate |> Option.map toDateFormat |> Option.defaultValue "(NONE)")) ]
        div [] [ str ("MODEL DATETIME: " + (model.DirectDate |> Option.map string |> Option.defaultValue "(NONE)")) ]
        div [] [
            input [
                HTMLAttr.Type "date"
                Value (model.DirectDate |> Option.map toDateFormat |> Option.defaultValue "")
                OnChange (fun v -> v.Value |> safeParse |> DirectDateChanged |> dispatch)
            ]
        ]
    ]

Program.mkProgram init update view
|> Program.withConsoleTrace
|> Program.withReactSynchronous "elmish-app"
|> Program.withDebugger
|> Program.run
