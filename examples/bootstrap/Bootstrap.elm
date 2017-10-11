module Bootstrap exposing (main)

import Date exposing (Date, Day(..), day, dayOfWeek, month, year)
import DatePicker exposing (defaultSettings, DateEvent(..))
import Html exposing (Html, div, form, h1, input, label, text)
import Html.Attributes exposing (class, type_, value)


type Msg
    = ToDatePickerStart DatePicker.Msg
    | ToDatePickerEnd DatePicker.Msg


type alias Model =
    { startDate : Maybe Date
    , endDate : Maybe Date
    , datePicker : DatePicker.DatePicker
    }


startSettings : DatePicker.Settings
startSettings =
    let
        isDisabled date =
            dayOfWeek date
                |> flip List.member [ Sat, Sun ]
    in
        { defaultSettings
            | isDisabled = isDisabled
            , inputClassList = [ ( "form-control", True ) ]
            , inputName = Just "date"
            , inputId = Just "date-field"
        }


endSettings : DatePicker.Settings
endSettings =
    let
        isDisabled date =
            dayOfWeek date
                |> flip List.member [ Sat, Sun ]
    in
        { defaultSettings
            | isDisabled = isDisabled
            , inputClassList = [ ( "form-control", True ) ]
            , inputName = Just "end-date"
            , inputId = Just "end-date-field"
        }


init : ( Model, Cmd Msg )
init =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
        { startDate = Nothing
        , endDate = Nothing
        , datePicker = datePicker
        }
            ! [ Cmd.map ToDatePickerStart datePickerFx, Cmd.map ToDatePickerEnd datePickerFx ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ datePicker } as model) =
    case msg of
        ToDatePickerStart msg ->
            let
                ( newDatePicker, datePickerFx, event ) =
                    DatePicker.update startSettings msg datePicker
            in
                { model
                    | startDate =
                        case event of
                            Changed date ->
                                date

                            NoChange ->
                                model.startDate
                    , datePicker = newDatePicker
                }
                    ! [ Cmd.map ToDatePickerStart datePickerFx ]

        ToDatePickerEnd msg ->
            let
                ( newDatePicker, datePickerFx, event ) =
                    DatePicker.update endSettings msg datePicker
            in
                { model
                    | endDate =
                        case event of
                            Changed date ->
                                date

                            NoChange ->
                                model.endDate
                    , datePicker = newDatePicker
                }
                    ! [ Cmd.map ToDatePickerStart datePickerFx ]


view : Model -> Html Msg
view ({ startDate, endDate, datePicker } as model) =
    div [ class "col-md-3" ]
        [ form []
            [ div [ class "form-group" ]
                [ label [] [ text "Start date" ]
                , DatePicker.view startDate startSettings datePicker
                    |> Html.map ToDatePickerStart
                ]
            , div [ class "form-group" ]
                [ label [] [ text "End date" ]
                , DatePicker.view endDate endSettings datePicker
                    |> Html.map ToDatePickerEnd
                ]
            , input
                [ type_ "submit"
                , class "btn btn-primary"
                , value "Submit"
                ]
                []
            ]
        ]


formatDate : Date -> String
formatDate d =
    toString (month d) ++ " " ++ toString (day d) ++ ", " ++ toString (year d)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
