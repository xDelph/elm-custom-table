port module Main exposing (Msg(..), MyModel, columns, config, customTableModel, getListItems, init, main, subscriptions, update, view)

import Browser
import CustomTable exposing (init, initState, renderInt, renderString, update, view)
import CustomTableType as CustomTableType exposing (Accessor(..), Column, Config, Filter(..), Item, Model, Msg(..), State, ValueFilter(..))
import Data exposing (RandomData, randomData)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Ports exposing (scrolledTo, updateCustomTable)


subscriptions : MyModel -> Sub Msg
subscriptions _ =
    Sub.batch
        [ scrolledTo ScrolledTo
        ]


main =
    Browser.element
        { init = \() -> init randomData
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias MyModel =
    { randomData : List RandomData
    , state : CustomTableType.State
    }


type Msg
    = ScrolledTo CustomTableType.ScrollEvent
    | CustomTableMsg (CustomTableType.Msg Msg)


init : List RandomData -> ( MyModel, Cmd Msg )
init data =
    let
        model =
            { randomData = data, state = CustomTable.initState -1 }
    in
    ( model, Cmd.none )


update : Msg -> MyModel -> ( MyModel, Cmd Msg )
update msg model =
    case msg of
        ScrolledTo scrollEvent ->
            update (CustomTableMsg <| CustomTableType.ScrolledTo scrollEvent) model

        CustomTableMsg customTableMsg ->
            let
                ( newState, newCmd ) =
                    CustomTable.update customTableMsg model.state
            in
            case customTableMsg of
                Select index ->
                    ( { model
                        | randomData =
                            List.map
                                (\item ->
                                    if item.index == index then
                                        { item | selected = not item.selected }

                                    else
                                        item
                                )
                                model.randomData
                      }
                    , Cmd.map CustomTableMsg newCmd
                    )

                SelectAll value filteredIndex ->
                    ( { model
                        | randomData =
                            List.map
                                (\item ->
                                    if List.member item.index filteredIndex then
                                        { item | selected = value }

                                    else
                                        item
                                )
                                model.randomData
                        , state = newState
                      }
                    , Cmd.map CustomTableMsg newCmd
                    )

                _ ->
                    ( { model | state = newState }, Cmd.batch [ updateCustomTable False, Cmd.map CustomTableMsg newCmd ] )


view : MyModel -> Html Msg
view model =
    div [ style "height" "90vh" ]
        [ Html.map CustomTableMsg <| CustomTable.view model.state (customTableModel model)
        ]


columns : List RandomData -> List (Column RandomData Msg)
columns data =
    [ { properties =
            { id = "status"
            , sortable = True
            , title = String.toUpper "status."
            , visible = True
            }
      , filter = ValueFilter
      , dataForFilter = StringValueFilter <| List.map .status data
      , accessor = GetString .status
      , render = renderString .status
      }
    , { properties =
            { id = "reference"
            , sortable = True
            , title = String.toUpper "reference."
            , visible = True
            }
      , filter = SimpleFilter
      , dataForFilter = NoValueFilter
      , accessor = GetString .reference
      , render = renderString .reference
      }
    , { properties =
            { id = "familyId"
            , sortable = False
            , title = String.toUpper "familyId."
            , visible = True
            }
      , filter = Unfilterable
      , dataForFilter = NoValueFilter
      , accessor = GetInt .familyId
      , render = renderInt .familyId
      }
    , { properties =
            { id = "itemId"
            , sortable = False
            , title = String.toUpper "itemId."
            , visible = True
            }
      , filter = Unfilterable
      , dataForFilter = NoValueFilter
      , accessor = GetString .itemId
      , render = renderString .itemId
      }
    , { properties =
            { id = "description"
            , sortable = False
            , title = String.toUpper "description."
            , visible = True
            }
      , filter = Unfilterable
      , dataForFilter = NoValueFilter
      , accessor = GetString .description
      , render = renderString .description
      }
    , { properties =
            { id = "column0"
            , sortable = True
            , title = String.toUpper "column0."
            , visible = True
            }
      , filter = ValueFilter
      , dataForFilter = StringValueFilter <| List.map .column0 data
      , accessor = GetString .column0
      , render = renderString .column0
      }
    , { properties =
            { id = "column1"
            , sortable = True
            , title = String.toUpper "column1."
            , visible = True
            }
      , filter = ValueFilter
      , dataForFilter = StringValueFilter <| List.map .column1 data
      , accessor = GetString .column1
      , render = renderString .column1
      }
    , { properties =
            { id = "column2"
            , sortable = True
            , title = String.toUpper "column2."
            , visible = True
            }
      , filter = ValueFilter
      , dataForFilter = StringValueFilter <| List.map .column2 data
      , accessor = GetString .column2
      , render = renderString .column2
      }
    , { properties =
            { id = "column3"
            , sortable = True
            , title = String.toUpper "column3."
            , visible = True
            }
      , filter = ValueFilter
      , dataForFilter = StringValueFilter <| List.map .column3 data
      , accessor = GetString .column3
      , render = renderString .column3
      }
    , { properties =
            { id = "column4"
            , sortable = True
            , title = String.toUpper "column4."
            , visible = True
            }
      , filter = ValueFilter
      , dataForFilter = StringValueFilter <| List.map .column4 data
      , accessor = GetString .column4
      , render = renderString .column4
      }
    ]


config : List RandomData -> Config RandomData Msg
config data =
    { canSelectRows = True
    , columns = columns data
    }


getListItems : List RandomData -> List (Item RandomData)
getListItems data =
    data


customTableModel : MyModel -> CustomTableType.Model RandomData Msg
customTableModel model =
    CustomTable.init model.state
        (getListItems model.randomData)
        (config model.randomData)
        False
        50
        100
