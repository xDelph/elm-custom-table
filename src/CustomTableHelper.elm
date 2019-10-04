module CustomTableHelper exposing
    ( applySorting
    , boolListToString
    , filterData
    , floatListToString
    , getNbPage
    , getStringDataValueFilter
    , intListToString
    , stringListToBool
    , stringListToFloat
    , stringListToInt
    )

import Array exposing (Array)
import CustomTableType exposing (..)
import Dict exposing (Dict)


sortData : List (Column a msg) -> List (Item a) -> String -> List (Item a)
sortData columns data sortingId =
    case List.head <| List.filter (\c -> c.properties.id == sortingId) columns of
        Just c ->
            case c.accessor of
                GetString accessor ->
                    List.sortBy accessor data

                GetInt accessor ->
                    List.sortBy accessor data

                GetFloat accessor ->
                    List.sortBy accessor data

                GetBool accessor ->
                    List.sortBy
                        (\item ->
                            if accessor item == True then
                                "True"

                            else
                                "False"
                        )
                        data

                _ ->
                    data

        Nothing ->
            data


applySorting : State -> List (Column a msg) -> List (Item a) -> List (Item a)
applySorting { order } columns data =
    case order of
        Ascending columnId ->
            List.reverse <| sortData columns data columnId

        Descending columnId ->
            sortData columns data columnId

        _ ->
            data


fieldToString : Accessor a -> Item a -> String
fieldToString accessor item =
    case accessor of
        GetString field ->
            field item

        GetInt field ->
            String.fromInt <| field item

        GetFloat field ->
            String.fromFloat <| field item

        GetBool field ->
            if field item then
                "True"

            else
                "False"

        _ ->
            ""


filterData : Array (Column a msg) -> State -> Int -> List (Item a) -> List (Item a)
filterData columns state index items =
    let
        nbColumns =
            Array.length columns
    in
    if index == nbColumns then
        items

    else
        case Array.get index columns of
            Just c ->
                case c.filter of
                    ValueFilter ->
                        let
                            dataValueFilter =
                                getStringDataValueFilter c

                            hidden =
                                case Dict.get c.properties.id state.valueFilter of
                                    Just list ->
                                        list

                                    Nothing ->
                                        []
                        in
                        filterData columns state (index + 1) <| List.filter (\item -> not <| List.member (fieldToString c.accessor item) hidden) items

                    _ ->
                        let
                            (FilterData _ dict) =
                                state.filter
                        in
                        case Dict.get c.properties.id dict of
                            Just v ->
                                filterData columns state (index + 1) <| List.filter (\item -> String.contains (String.toLower v) (String.toLower (fieldToString c.accessor item))) items

                            Nothing ->
                                filterData columns state (index + 1) items

            Nothing ->
                filterData columns state (index + 1) items


getStringDataValueFilter : Column a msg -> List String
getStringDataValueFilter column =
    case column.dataForFilter of
        BoolValueFilter valueFilter ->
            boolListToString valueFilter

        StringValueFilter valueFilter ->
            valueFilter

        IntValueFilter valueFilter ->
            intListToString valueFilter

        FloatValueFilter valueFilter ->
            floatListToString valueFilter

        _ ->
            []


intListToString : List Int -> List String
intListToString list =
    List.map String.fromInt list


stringListToInt : List String -> List Int
stringListToInt list =
    List.map (\a -> Maybe.withDefault 0 (String.toInt a)) list


floatListToString : List Float -> List String
floatListToString list =
    List.map String.fromFloat list


stringListToFloat : List String -> List Float
stringListToFloat list =
    List.map (\a -> Maybe.withDefault 0 (String.toFloat a)) list


boolListToString : List Bool -> List String
boolListToString list =
    List.map
        (\a ->
            if a then
                "True"

            else
                "False"
        )
        list


stringListToBool : List String -> List Bool
stringListToBool list =
    List.map
        (\a ->
            if a == "True" then
                True

            else
                False
        )
        list


getNbPage : List (Item a) -> Int -> Int
getNbPage list itemPerPage =
    round ((toFloat <| List.length list) / toFloat itemPerPage)
