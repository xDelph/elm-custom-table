module CustomTablePagination exposing (render)

import CustomTableType as CustomTableType exposing (..)
import Html exposing (Attribute, Html, div, i, input, label, option, select, span, td, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (on, onClick, targetValue)
import Json.Decode as Decode


targetIntValue : Decode.Decoder Int
targetIntValue =
    Decode.at [ "target", "value" ] Decode.string
        |> Decode.andThen
            (\value ->
                case String.toInt value of
                    Just number ->
                        Decode.succeed number

                    Nothing ->
                        Decode.fail "not a number"
            )


alwaysStop : a -> ( a, Bool )
alwaysStop x =
    ( x, True )


onChange : (Int -> msg) -> Html.Attribute msg
onChange tagger =
    Html.Events.stopPropagationOn "change" <|
        Decode.map alwaysStop (Decode.map tagger targetIntValue)


render : State -> Int -> Int -> Int -> Html (Msg b)
render state page nbPage nbItems =
    let
        ( firstItem, lastItem ) =
            if state.itemPerPage == 0 then
                ( 1, nbItems )

            else
                ( state.itemPerPage * (page - 1) + 1, min (state.itemPerPage * page) nbItems )
    in
    div [ class "customTable_pagination" ]
        [ div []
            [ text "Rows per page : "
            , select [ onChange ItemPerPage ]
                [ option [ value "20", selected <| 20 == state.itemPerPage ] [ text "20" ]
                , option [ value "50", selected <| 50 == state.itemPerPage ] [ text "50" ]
                , option [ value "100", selected <| 100 == state.itemPerPage ] [ text "100" ]
                , option [ value "500", selected <| 200 == state.itemPerPage ] [ text "500" ]
                , option [ value "0", selected <| 0 == state.itemPerPage ] [ text "all" ]
                ]
            ]
        , div [] [ text <| String.fromInt firstItem ++ "-" ++ String.fromInt lastItem ++ " of " ++ String.fromInt nbItems ]
        , div
            [ class <|
                "customTable_arrow "
                    ++ (if page == 1 then
                            "customTable_disabled"

                        else
                            ""
                       )
            , onClick <| Go Next
            ]
            [ i [ class "fas fa-chevron-left" ] [] ]
        , div
            [ class <|
                "customTable_arrow "
                    ++ (if page == nbPage then
                            "customTable_disabled"

                        else
                            ""
                       )
            , onClick <| Go Previous
            ]
            [ i [ class "fas fa-chevron-right" ] [] ]
        ]
