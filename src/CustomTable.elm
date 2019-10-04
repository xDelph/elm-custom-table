module CustomTable exposing
    ( init, initState, getActionsColumnConfig
    , getSortAsString, getSortingFromString, getFiltersAsString, getFiltersFromString, getValueFiltersAsString, getValueFiltersFromString
    , sort, selectAll, showFilterPopup, updateFilter, updateDataFilter, updateScrollPosition, paginationGo, updateItemPerPage
    , renderString, renderInt, renderFloat, renderBool
    , update
    , view
    )

{-|


# Initialize model and state

@docs init, initState, getActionsColumnConfig


# State properties as string

@docs getSortAsString, getSortingFromString, getFiltersAsString, getFiltersFromString, getValueFiltersAsString, getValueFiltersFromString


# Events binding

@docs sort, selectAll, showFilterPopup, updateFilter, updateDataFilter, updateScrollPosition, paginationGo, updateItemPerPage


# Column rendering

@docs renderString, renderInt, renderFloat, renderBool, renderSelect


# Update

@docs update


# Principal rendering function

@docs view

-}

import Array exposing (Array)
import Browser.Dom
import CustomTableHeader as CustomTableHeader
import CustomTableHelper exposing (..)
import CustomTablePagination as CustomTablePagination
import CustomTableType exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, div, input, label, section, span, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (attribute, checked, class, height, id, style, type_)
import Html.Events exposing (custom, onClick)
import Html.Keyed as Keyed
import Html.OnClickOutside
import List.Extra
import Task


{-|


# Initialize model and state

-}
init : State -> List (Item a) -> Config a b -> Bool -> Int -> Int -> Model a b
init state items config pagination rowHeight nbToDisplay =
    { config =
        { config
            | columns =
                if config.canSelectRows then
                    sortColumnConfig :: config.columns

                else
                    config.columns
        }
    , content = items
    , pagination = pagination
    , rowHeight = rowHeight
    , nbToDisplay = nbToDisplay
    }


initState : Int -> State
initState lastSelected =
    State Unsorted (FilterData "" (Dict.fromList [])) (Dict.fromList []) False lastSelected (ScrollEvent 50 0) 50 1 False (Dict.fromList []) True


getActionsColumnConfig : (Item a -> Html (Msg b)) -> Column a b
getActionsColumnConfig renderFn =
    { properties =
        { id = "customTable_actionsRow"
        , sortable = False
        , title = ""
        , visible = True
        }
    , filter = Unfilterable
    , dataForFilter = NoValueFilter
    , accessor = NoAccess
    , render = renderFn
    }


getSelectedItems : State -> List (Item a) -> List (Item a)
getSelectedItems state items =
    items


{-|


# State properties as string

-}
getSortAsString : Sorting -> String
getSortAsString sorting =
    case sorting of
        Ascending columnId ->
            "ascending " ++ columnId

        Descending columnId ->
            "descending " ++ columnId

        _ ->
            ""


getSortingFromString : State -> String -> State
getSortingFromString state str =
    let
        splitted =
            String.split " " str

        sorting =
            Maybe.withDefault "" <| List.head splitted

        columnId =
            Maybe.withDefault "" <| List.head <| List.reverse splitted
    in
    case sorting of
        "ascending" ->
            { state | order = Ascending columnId }

        "descending" ->
            { state | order = Descending columnId }

        _ ->
            state


getFiltersAsString : FilterData -> String
getFiltersAsString (FilterData _ dict) =
    let
        mapped =
            List.filter (\i -> i /= "") <|
                List.map
                    (\( k, v ) ->
                        if v /= "" then
                            k ++ ":" ++ v

                        else
                            ""
                    )
                <|
                    Dict.toList dict
    in
    if List.length mapped > 1 then
        List.foldr
            (\a b -> a ++ "," ++ b)
            ""
            mapped

    else
        Maybe.withDefault "" <| List.head mapped


getFiltersFromString : State -> String -> State
getFiltersFromString state str =
    if str == "" then
        state

    else
        let
            list =
                List.map
                    (\s ->
                        let
                            splitted =
                                String.split ":" s
                        in
                        ( Maybe.withDefault "" <| List.head splitted, Maybe.withDefault "" <| List.head <| List.reverse splitted )
                    )
                <|
                    String.split "," str
        in
        { state | filter = FilterData "" <| Dict.fromList list }


getValueFiltersAsString : Dict String (List String) -> String
getValueFiltersAsString valueFilter =
    let
        mapped =
            List.filter (\i -> i /= "") <|
                List.map
                    (\( k, v ) ->
                        if List.length v > 0 then
                            k ++ ":" ++ List.foldr (\a b -> a ++ ":" ++ b) "" v

                        else
                            ""
                    )
                <|
                    Dict.toList valueFilter
    in
    if List.length mapped > 1 then
        List.foldr
            (\a b -> a ++ "," ++ b)
            ""
            mapped

    else
        Maybe.withDefault "" <| List.head mapped


getValueFiltersFromString : State -> String -> State
getValueFiltersFromString state str =
    if str == "" then
        state

    else
        let
            list =
                List.map
                    (\s ->
                        let
                            splitted =
                                String.split ":" s
                        in
                        ( Maybe.withDefault "" <| List.head splitted, List.drop 1 splitted )
                    )
                <|
                    String.split "," str
        in
        { state | valueFilter = Dict.fromList list }


{-|


# Events binding

-}
sortColumnConfig : Column a b
sortColumnConfig =
    { properties =
        { id = "customTable_selectRow"
        , sortable = False
        , title = ""
        , visible = True
        }
    , filter = Unfilterable
    , dataForFilter = NoValueFilter
    , accessor = NoAccess
    , render = renderSelect
    }


sort : State -> Sorting -> State
sort state newSorting =
    { state
        | order =
            if state.order == newSorting then
                Unsorted

            else
                newSorting
    }


selectAll : State -> Bool -> State
selectAll state value =
    { state | allSelected = value }


showFilterPopup : State -> String -> State
showFilterPopup state str =
    let
        (FilterData columnId dict) =
            state.filter
    in
    { state
        | filter =
            if columnId /= str then
                FilterData str dict

            else
                FilterData "" dict
    }


updateFilter : State -> String -> String -> State
updateFilter state columnId value =
    let
        (FilterData filterColumnId dict) =
            state.filter
    in
    { state | filter = FilterData filterColumnId (Dict.insert columnId value dict) }


updateDataFilter : State -> String -> String -> State
updateDataFilter state columnId value =
    let
        valueFilter =
            case Dict.get columnId state.valueFilter of
                Just hidden ->
                    if List.member value hidden then
                        List.Extra.remove value hidden

                    else
                        value :: hidden

                Nothing ->
                    [ value ]
    in
    { state | valueFilter = Dict.insert columnId valueFilter state.valueFilter }


selectAllDataFilter : State -> String -> List String -> State
selectAllDataFilter state columnId values =
    { state | valueFilter = Dict.insert columnId values state.valueFilter }


updateScrollPosition : State -> ScrollEvent -> State
updateScrollPosition state scrollEvent =
    { state | scrollEvent = scrollEvent }


paginationGo : State -> Pagination -> State
paginationGo state pagination =
    case pagination of
        Next ->
            { state | page = state.page - 1 }

        Previous ->
            { state | page = state.page + 1 }


updateItemPerPage : State -> Int -> State
updateItemPerPage state itemPerPage =
    { state | itemPerPage = itemPerPage, page = 1 }


{-|


# Column rendering

-}
renderString : (Item a -> String) -> Item a -> Html (Msg b)
renderString field item =
    let
        a =
            field item
    in
    text <|
        if a == "" then
            "—"

        else
            a


renderInt : (Item a -> Int) -> Item a -> Html (Msg b)
renderInt field item =
    text <| String.fromInt <| field item


renderFloat : (Item a -> Float) -> Item a -> Html (Msg b)
renderFloat field item =
    text <| String.fromFloat <| field item


renderBool : (Item a -> Bool) -> Item a -> Html (Msg b)
renderBool field item =
    let
        a =
            field item
    in
    text <|
        if a then
            "True"

        else
            "False"


renderSelect : Item a -> Html (Msg b)
renderSelect item =
    if item.selected then
        label [ class "customTable_checkbox" ]
            [ input [ type_ "checkbox", checked item.selected, onClick <| Select item.index ]
                []
            , span [ class "customTable_checkmark" ]
                []
            ]

    else
        div [ style "width" "100%", style "height" "100%", onClick <| Select item.index ] [ span [ class "customTable_selectItem" ] [ text <| String.fromInt item.index ] ]


{-|


# Update

-}
update : Msg a -> State -> ( State, Cmd (Msg a) )
update msg state =
    case msg of
        Sort columnId ->
            ( sort state columnId, Cmd.none )

        ShowFilterPopup columnId ->
            ( showFilterPopup state columnId, Task.attempt Focus (Browser.Dom.focus <| "customTable_" ++ columnId ++ "IdInput") )

        UpdateFilter columnId value ->
            ( updateFilter state columnId value, Cmd.none )

        ScrolledTo scrollEvent ->
            ( updateScrollPosition state scrollEvent, Cmd.none )

        Go pagination ->
            ( paginationGo state pagination, Cmd.none )

        ItemPerPage value ->
            ( updateItemPerPage state value, Cmd.none )

        DataFilter columnId value ->
            ( updateDataFilter state columnId value, Cmd.none )

        SelectAllDataFilter columnId values ->
            ( selectAllDataFilter state columnId values, Cmd.none )

        SelectAll value _ ->
            ( selectAll state value, Cmd.none )

        ShowActionsPopup ->
            ( { state | actionsPopup = not state.actionsPopup }, Task.attempt Focus (Browser.Dom.focus <| "customTable_actionsRowIdInput") )

        ShowAllColumns value ->
            let
                keys =
                    Dict.keys state.columnsSetting

                list =
                    List.map
                        (\k ->
                            let
                                columnSetting =
                                    Maybe.withDefault { visible = value, pinned = False } <| Dict.get k state.columnsSetting
                            in
                            ( k, { columnSetting | visible = value } )
                        )
                        keys
            in
            ( { state | showAllColumns = value, columnsSetting = Dict.fromList list }, Cmd.none )

        ShowColumn columnId ->
            let
                columnSetting =
                    Maybe.withDefault { visible = state.showAllColumns, pinned = False } <| Dict.get columnId state.columnsSetting
            in
            ( { state | columnsSetting = Dict.insert columnId { columnSetting | visible = not columnSetting.visible } state.columnsSetting }, Cmd.none )

        _ ->
            ( state, Cmd.none )


{-|


# Principal rendering function

-}
view : State -> Model a b -> Html (Msg b)
view state model =
    let
        columns =
            List.filter
                (\c ->
                    let
                        columnSetting =
                            Maybe.withDefault { visible = state.showAllColumns, pinned = False } <| Dict.get c.properties.id state.columnsSetting
                    in
                    if c.properties.id == "customTable_selectRow" || c.properties.id == "customTable_actionsRow" then
                        True

                    else
                        columnSetting.visible
                )
                model.config.columns

        sortedContent =
            applySorting state columns model.content

        filteredContent =
            filterData (Array.fromList columns) state 0 sortedContent

        filteredIndex =
            List.map .index filteredContent

        allSelected =
            List.length filteredContent == (List.length <| List.filter (\item -> item.selected == True) filteredContent)

        tHeadRows =
            thead [] [ CustomTableHeader.render state model.config.columns filteredIndex allSelected ]

        paginatedContent =
            if model.pagination && state.itemPerPage /= 0 then
                List.take state.itemPerPage <| List.drop (state.itemPerPage * (state.page - 1)) filteredContent

            else
                filteredContent

        nbPage =
            getNbPage paginatedContent state.itemPerPage

        nbToHide =
            max (state.scrollEvent.scrollTop - (20 * model.rowHeight)) 0 // model.rowHeight

        itemsToDisplay =
            List.take model.nbToDisplay <| List.drop nbToHide paginatedContent

        itemSelected =
            case List.head <| List.filter (\item -> item.index == state.lastSelected) <| paginatedContent of
                Just item ->
                    let
                        rowSelectedClass =
                            if item.selected then
                                " customTable_rowSelected"

                            else
                                ""
                    in
                    [ ( String.fromInt item.index, tr [ class <| "row customTable_lastSelected" ++ rowSelectedClass ] <| List.map (\c -> td [ class <| c.properties.id ] [ c.render item ]) columns ) ]

                Nothing ->
                    [ ( "selected", text "" ) ]

        rowSelectedOnTop =
            case List.head <| List.filter (\item -> item.index == state.lastSelected) <| List.take nbToHide paginatedContent of
                Just item ->
                    True

                Nothing ->
                    False

        rowSelectedOnBottom =
            case List.head <| List.filter (\item -> item.index == state.lastSelected) <| List.drop (model.nbToDisplay + nbToHide) paginatedContent of
                Just item ->
                    True

                Nothing ->
                    False

        heightToHideTop =
            ((if rowSelectedOnTop then
                1

              else
                0
             )
                + nbToHide
            )
                * model.rowHeight

        heightToHideBottom =
            ((List.length paginatedContent
                - (if rowSelectedOnTop then
                    1

                   else
                    0
                  )
             )
                - (2 * model.rowHeight)
                - nbToHide
            )
                * model.rowHeight

        tBodyRows =
            Keyed.node "tbody" [] <|
                [ ( "top"
                  , if heightToHideTop > 0 then
                        tr [ class <| "row" ] [ td [] [ div [ style "height" (String.fromInt heightToHideTop ++ "px") ] [] ] ]

                    else
                        text ""
                  )
                ]
                    ++ (if rowSelectedOnTop then
                            itemSelected

                        else
                            [ ( "selected", text "" ) ]
                       )
                    ++ List.map
                        (\item ->
                            let
                                lastSelected =
                                    if item.index == state.lastSelected then
                                        " customTable_lastSelected"

                                    else
                                        ""

                                rowSelectedClass =
                                    if item.selected then
                                        " customTable_rowSelected"

                                    else
                                        ""
                            in
                            ( String.fromInt item.index, tr [ height model.rowHeight, class <| "row" ++ lastSelected ++ rowSelectedClass ] <| List.map (\c -> td [ class <| c.properties.id ] [ c.render item ]) columns )
                        )
                        itemsToDisplay
                    ++ (if rowSelectedOnBottom then
                            itemSelected

                        else
                            [ ( "selected", text "" ) ]
                       )
                    ++ [ ( "bottom"
                         , if heightToHideBottom > 0 then
                            tr [ class <| "row" ] [ td [] [ div [ style "height" (String.fromInt heightToHideBottom ++ "px") ] [] ] ]

                           else
                            text ""
                         )
                       ]
    in
    section [ class "customTable" ] <|
        [ div
            [ id "customTable" ]
            [ table []
                [ tHeadRows
                , if not <| List.isEmpty <| List.filter (\c -> c.properties.id /= "customTable_selectRow" && c.properties.id /= "customTable_actionsRow") columns then
                    tBodyRows

                  else
                    text ""
                ]
            ]
        ]
            ++ (if model.pagination then
                    [ div [ class "customTable_nbResults" ] [ text <| (String.fromInt <| List.length <| List.filter (\item -> item.selected) model.content) ++ " selected" ], CustomTablePagination.render state state.page nbPage (List.length filteredContent) ]

                else
                    [ div [ class "customTable_nbResults" ] [ text <| (String.fromInt <| List.length filteredContent) ++ " items - " ++ (String.fromInt <| List.length <| List.filter (\item -> item.selected) model.content) ++ " selected" ] ]
               )
