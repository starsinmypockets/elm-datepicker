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
    , startDatePicker : DatePicker.DatePicker
    , endDatePicker : DatePicker.DatePicker
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
            , inputName = Just "start-date"
            , inputId = Just "start-date-field"
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
        ( startDatePicker, startDatePickerFx ) =
            DatePicker.init

        ( endDatePicker, endDatePickerFx ) =
            DatePicker.init
    in
        { startDate = Nothing
        , endDate = Nothing
        , startDatePicker = startDatePicker
        , endDatePicker = endDatePicker
        }
            ! [ Cmd.map ToDatePickerStart startDatePickerFx, Cmd.map ToDatePickerEnd endDatePickerFx ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ startDatePicker, endDatePicker } as model) =
    case msg of
        ToDatePickerStart msg ->
            let
                ( newDatePicker, datePickerFx, event ) =
                    DatePicker.update startSettings msg startDatePicker
            in
                { model
                    | startDate =
                        case event of
                            Changed date ->
                                date

                            NoChange ->
                                model.startDate
                    , startDatePicker = newDatePicker
                }
                    ! [ Cmd.map ToDatePickerStart datePickerFx ]

        ToDatePickerEnd msg ->
            let
                ( newDatePicker, datePickerFx, event ) =
                    DatePicker.update endSettings msg endDatePicker
            in
                { model
                    | endDate =
                        case event of
                            Changed date ->
                                date

                            NoChange ->
                                model.endDate
                    , endDatePicker = newDatePicker
                }
                    ! [ Cmd.map ToDatePickerStart datePickerFx ]


view : Model -> Html Msg
view ({ startDate, endDate, startDatePicker, endDatePicker } as model) =
    div [ class "col-md-3" ]
        [ form []
            [ div [ class "form-group" ]
                [ label [] [ text "Start date" ]
                , DatePicker.view startDate startSettings startDatePicker
                    |> Html.map ToDatePickerStart
                ]
            , div [ class "form-group" ]
                [ label [] [ text "End date" ]
                , DatePicker.view endDate endSettings endDatePicker
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
