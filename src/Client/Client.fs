module Client

open Elmish
open Elmish.Debug
open Elmish.React
open Elmish.HMR
open Fable.React
open Fable.React.Props
open System

type Model =
    { IndirectDateRaw : string
      DirectDate : DateTime option }
    member this.IndirectDate =
        match this.IndirectDateRaw with
        | "" | null -> None
        | _ ->
            try this.IndirectDateRaw |> System.DateTime.Parse |> Some
            with _ -> None

type Msg =
    | IndirectDateChanged of string
    | DirectDateChanged of DateTime option

module CrazySerialization =
    type System.Int32 with
        member this.Pad digits =
            let output = this.ToString()
            (String.replicate (digits - output.Length) "0") + output

    type System.DateTime with
        member this.ToStringCorrect() =
            sprintf "%s-%s-%s" (this.Year.Pad 4) (this.Month.Pad 2) (this.Day.Pad 2)

open CrazySerialization

let init() =
    { IndirectDateRaw = ""; DirectDate = None }, Cmd.none

let update msg model =
    match msg with
    | IndirectDateChanged v -> { model with IndirectDateRaw = v }, Cmd.none
    | DirectDateChanged v -> { model with DirectDate = v }, Cmd.none

let view model dispatch =
    div [ Style [ TextAlign TextAlignOptions.Center; Padding 40 ] ] [
        h2 [] [ str "INDIRECT (VIA STRING)" ]
        div [] [ str ("MODEL STRING: " + model.IndirectDateRaw) ]
        div [] [ str ("MODEL DATETIME: " + (model.IndirectDate |> Option.map string |> Option.defaultValue "(NONE)")) ]
        div [] [ input [ HTMLAttr.Type "date"; Value model.IndirectDateRaw; OnChange (fun v -> dispatch (IndirectDateChanged v.Value)) ] ]

        h2 [] [ str "DIRECT (DATETIME)" ]
        div [] [ str ("MODEL DATETIME (SERIALIZED): " + (model.DirectDate |> Option.map(fun r -> r.ToStringCorrect()) |> Option.defaultValue "(NONE)")) ]
        div [] [ str ("MODEL DATETIME: " + (model.DirectDate |> Option.map(fun r -> r.ToString()) |> Option.defaultValue "(NONE)")) ]
        div [] [ input [ HTMLAttr.Type "date"; Value (model.DirectDate |> Option.map(fun d -> d.ToStringCorrect()) |> Option.defaultValue ""); OnChange (fun v -> if v.Value = "" then dispatch (DirectDateChanged None) else dispatch (DirectDateChanged (DateTime.Parse v.Value |> Some))) ] ]
    ]


Program.mkProgram init update view
|> Program.withConsoleTrace
|> Program.withReactSynchronous "elmish-app"
|> Program.withDebugger
|> Program.run
