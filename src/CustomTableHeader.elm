module CustomTableHeader exposing (render, renderCell)

{-|


# Render thead

@docs render, renderCell

-}

import CustomTableHelper exposing (..)
import CustomTableType exposing (..)
import Dict
import Html exposing (Attribute, Html, button, div, input, label, li, span, td, text, th, tr, ul)
import Html.Attributes exposing (checked, class, disabled, for, id, placeholder, style, type_, value)
import Html.Events exposing (custom, onClick, onInput)
import Html.OnClickOutside
import Json.Decode as Json
import List.Extra
import Views.Svg as Svg


renderSelectAllCell : State -> List Int -> Bool -> Html (Msg b)
renderSelectAllCell state filteredIndex allSelected =
    label [ class "customTable_checkbox" ]
        [ input [ type_ "checkbox", checked allSelected, onClick <| SelectAll (not allSelected) filteredIndex ]
            []
        , span [ class "customTable_checkmark" ]
            []
        ]


renderSortAndFilterIcon : Bool -> String -> String -> Bool -> Html (Msg b)
renderSortAndFilterIcon filtered sortedClass columnId opened =
    div
        [ class <|
            "customTable_filterIcon "
                ++ (if filtered then
                        " customTable_filtered"

                    else
                        ""
                   )
                ++ sortedClass
                ++ (if opened then
                        " customTable_popupOpened"

                    else
                        ""
                   )
        , custom "click" (Json.succeed { message = ShowFilterPopup columnId, stopPropagation = True, preventDefault = True })
        ]
        [ Svg.filter ]


renderActionsIcon : State -> Bool -> Html (Msg b)
renderActionsIcon state customized =
    span
        [ class <|
            "customTable_actionIcon"
                ++ (if state.actionsPopup then
                        " customTable_popupOpened"

                    else
                        ""
                   )
                ++ (if customized then
                        " customTable_actionsCustomized"

                    else
                        ""
                   )
        , custom "click" (Json.succeed { message = ShowActionsPopup, stopPropagation = True, preventDefault = True })
        ]
        [ Svg.setting ]


renderActionsPopup : State -> List (Column a b) -> Column a b -> Html (Msg b)
renderActionsPopup state columns actionsColumn =
    div
        ([ id <| actionsColumn.properties.id ++ "Id", class "customTable_popupView customTable_right" ]
            ++ Html.OnClickOutside.withId (actionsColumn.properties.id ++ "Id")
                ShowActionsPopup
        )
    <|
        [ input [ class "customTable_hiddenInput", id <| actionsColumn.properties.id ++ "IdInput" ] []
        , div [ class "customTable_popupTitle" ] [ text "Hide Fields" ]
        , div [ class "customTable_buttonsList" ]
            [ button [ class "customTable_button", onClick <| ShowAllColumns True ] [ text "Show All" ]
            , button [ class "customTable_button", onClick <| ShowAllColumns False ] [ text "Hide All" ]
            ]
        ]
            ++ List.map
                (\c ->
                    let
                        columnSetting =
                            Maybe.withDefault { visible = state.showAllColumns, pinned = False } <| Dict.get c.properties.id state.columnsSetting
                    in
                    case c.properties.id of
                        "customTable_selectRow" ->
                            text ""

                        "customTable_actionsRow" ->
                            text ""

                        _ ->
                            label [ class "customTable_checkbox" ]
                                [ input [ type_ "checkbox", checked columnSetting.visible, onClick <| ShowColumn c.properties.id ]
                                    []
                                , span [ class "customTable_checkmark" ]
                                    []
                                , text c.properties.title
                                ]
                )
                columns


renderSort : Sorting -> String -> Html (Msg b)
renderSort sorting columnId =
    div [ class "customTable_filterPopupSort" ]
        [ div
            [ class <|
                if sorting == Descending columnId then
                    " customTable_selected"

                else
                    ""
            , onClick <| Sort <| Descending columnId
            ]
            [ span [ class "customTable_icon" ] [ Svg.sortDescending ]
            , span [] [ text "Sort A -> Z" ]
            ]
        , div
            [ class <|
                if sorting == Ascending columnId then
                    " customTable_selected"

                else
                    ""
            , onClick <| Sort <| Ascending columnId
            ]
            [ span [ class "customTable_icon" ] [ Svg.sortAscending ]
            , span [] [ text "Sort Z -> A" ]
            ]
        ]


renderValueFilterList : State -> List String -> String -> Bool -> List (Html (Msg b))
renderValueFilterList state values columnId isDisabled =
    List.map
        (\a ->
            div [ class "customTable_pointer" ]
                [ input
                    [ type_ "checkbox"
                    , id <| a ++ columnId
                    , value a
                    , checked <|
                        not <|
                            List.member a <|
                                Maybe.withDefault [] <|
                                    Dict.get columnId state.valueFilter
                    , if isDisabled then
                        disabled isDisabled

                      else
                        onClick <| DataFilter columnId a
                    ]
                    []
                , label [ for <| a ++ columnId ]
                    [ text <|
                        if a == "" then
                            "—"

                        else
                            a
                    ]
                ]
        )
        values


renderFilter : State -> Column a v -> String -> Html (Msg b)
renderFilter state column filterValue =
    let
        filterPanelSubTitle selectAll values =
            div [ class "customTable_popupSubTitle" ] <|
                [ text "Filter by value" ]
                    ++ (if selectAll then
                            [ text " ( "
                            , span [ class "customTable_link", onClick <| SelectAllDataFilter column.properties.id [] ] [ text "all" ]
                            , text " / "
                            , span [ class "customTable_link", onClick <| SelectAllDataFilter column.properties.id values ] [ text "none" ]
                            , text " ) "
                            ]

                        else
                            [ text "" ]
                       )

        hiddenInput =
            input [ class "customTable_hiddenInput", id <| "customTable_" ++ column.properties.id ++ "IdInput" ] []
    in
    case column.filter of
        ValueFilter ->
            let
                values =
                    List.sort <| List.Extra.unique <| getStringDataValueFilter column

                ifDisabled =
                    if List.length values > 1 then
                        False

                    else
                        True
            in
            div []
                [ filterPanelSubTitle True values
                , div []
                    [ hiddenInput
                    , div
                        [ class <|
                            "customTable_valueForFilter"
                                ++ (if ifDisabled then
                                        " customTable_noScroll"

                                    else
                                        ""
                                   )
                        ]
                      <|
                        renderValueFilterList state
                            values
                            column.properties.id
                            ifDisabled
                    ]
                ]

        SimpleFilter ->
            div []
                [ filterPanelSubTitle False []
                , input
                    [ type_ "text"
                    , id <| "customTable_" ++ column.properties.id ++ "IdInput"
                    , onInput <| UpdateFilter column.properties.id
                    , value filterValue
                    , placeholder "Filter..."
                    ]
                    []
                ]

        Unfilterable ->
            hiddenInput


renderSortAndFilterPopup : Sorting -> String -> Column a v -> String -> State -> Html (Msg b)
renderSortAndFilterPopup sorting filterValue column position state =
    div
        ([ id <| "customTable_" ++ column.properties.id ++ "Id", class <| "customTable_popupView" ++ position ]
            ++ Html.OnClickOutside.withId ("customTable_" ++ column.properties.id ++ "Id")
                (ShowFilterPopup column.properties.id)
        )
    <|
        [ div [ class "customTable_popupTitle" ] [ text column.properties.title ]
        , if column.properties.sortable then
            renderSort sorting column.properties.id

          else
            text ""
        , renderFilter state column filterValue
        ]


renderCell : State -> List (Column a b) -> Column a b -> Bool -> List Int -> Bool -> Html (Msg b)
renderCell state columns column isOnRight filteredIndex allSelected =
    let
        columnSetting =
            Maybe.withDefault { visible = state.showAllColumns, pinned = False } <| Dict.get column.properties.id state.columnsSetting

        (FilterData filterId dict) =
            state.filter

        inputValue =
            Maybe.withDefault "" <| Dict.get column.properties.id dict

        sortedClass =
            let
                columnId =
                    case state.order of
                        Descending sortingId ->
                            sortingId

                        Ascending sortingId ->
                            sortingId

                        _ ->
                            ""

                hidden =
                    Maybe.withDefault [] <|
                        Dict.get column.properties.id
                            state.valueFilter
            in
            if column.properties.id == columnId || List.length hidden /= 0 || inputValue /= "" then
                " customTable_sortedOrFiltered"

            else
                ""

        actionsCutomized =
            not <| List.isEmpty (List.filter (\i -> not <| (Tuple.second i).visible) <| Dict.toList state.columnsSetting)

        content =
            case column.properties.id of
                "customTable_actionsRow" ->
                    renderActionsIcon state actionsCutomized

                "customTable_selectRow" ->
                    renderSelectAllCell state filteredIndex allSelected

                _ ->
                    text column.properties.title

        sortAndFilterIcon =
            if column.properties.sortable || column.filter /= Unfilterable then
                renderSortAndFilterIcon (inputValue /= "") sortedClass column.properties.id (filterId == column.properties.id)

            else
                text ""

        sortAndFilterPopup =
            if filterId == column.properties.id then
                renderSortAndFilterPopup state.order
                    inputValue
                    column
                    (if isOnRight then
                        " customTable_right"

                     else
                        " customTable_left"
                    )
                    state

            else
                text ""

        actionsPopup =
            if column.properties.id == "customTable_actionsRow" && state.actionsPopup then
                renderActionsPopup state columns column

            else
                text ""
    in
    case column.properties.id of
        "customTable_actionsRow" ->
            th [ class <| column.properties.id ] [ div [] [ content, sortAndFilterIcon, sortAndFilterPopup, actionsPopup ] ]

        "customTable_selectRow" ->
            th [ class <| column.properties.id ] [ div [] [ content, sortAndFilterIcon, sortAndFilterPopup, actionsPopup ] ]

        _ ->
            if columnSetting.visible then
                th [ class <| column.properties.id ] [ div [] [ content, sortAndFilterIcon, sortAndFilterPopup, actionsPopup ] ]

            else
                text ""


render : State -> List (Column a b) -> List Int -> Bool -> Html (Msg b)
render state allColumns filteredIndex allSelected =
    let
        nbColumns =
            List.length allColumns
    in
    tr [] <|
        List.indexedMap
            (\index c ->
                renderCell state allColumns c (nbColumns > 1 && (index - 1) * 2 // nbColumns > 0) filteredIndex allSelected
            )
            allColumns
