module CustomTableType exposing
    ( Msg(..), Model, State
    , Config, Column, ColumnProperties, Item
    , ScrollEvent
    , Accessor(..)
    , Sorting(..), Filter(..), FilterData(..), ValueDataFilter, ValueFilter(..)
    , Pagination(..)
    )

{-|


# Principal type

@docs Msg, Model, State


# Configuration

@docs Config, Column, ColumnProperties, Item


# Scroll

@docs ScrollEvent


# Accessor

@docs Accessor


# Sort and Filter

@docs Sorting, Filter, FilterData, ValueDataFilter, ValueFilter


# Pagination

@docs Pagination

-}

import Browser.Dom
import Dict exposing (Dict)
import Html exposing (Html)


{-|


# Principal type

-}
type Msg a
    = Sort Sorting
    | ShowFilterPopup String
    | UpdateFilter String String
    | Focus (Result Browser.Dom.Error ())
    | Go Pagination
    | ItemPerPage Int
    | DataFilter String String
    | SelectAllDataFilter String (List String)
    | ScrolledTo ScrollEvent
    | Select Int
    | SelectAll Bool (List Int)
    | ShowActionsPopup
    | ShowColumn String
    | ShowAllColumns Bool
    | External a


type alias Model a b =
    { config : Config a b
    , content : List (Item a)
    , pagination : Bool
    , rowHeight : Int
    , nbToDisplay : Int
    }


type alias State =
    { order : Sorting
    , filter : FilterData
    , valueFilter : Dict String (List String)
    , allSelected : Bool
    , lastSelected : Int
    , scrollEvent : ScrollEvent
    , itemPerPage : Int
    , page : Int
    , actionsPopup : Bool
    , columnsSetting :
        Dict String
            { visible : Bool
            , pinned : Bool
            }
    , showAllColumns : Bool
    }


{-|


# Configuration

-}
type alias Config a b =
    { canSelectRows : Bool
    , columns : List (Column a b)
    }


type alias Column a b =
    { properties : ColumnProperties
    , filter : Filter
    , dataForFilter : ValueFilter
    , accessor : Accessor a
    , render : Item a -> Html (Msg b)
    }


type alias ColumnProperties =
    { id : String
    , sortable : Bool
    , title : String
    , visible : Bool
    }


type alias Item a =
    { a
        | selected : Bool
        , index : Int
    }


{-|


# Scroll

-}
type alias ScrollEvent =
    { height : Int
    , scrollTop : Int
    }


{-|


# Accessor

-}
type Accessor a
    = GetString (Item a -> String)
    | GetInt (Item a -> Int)
    | GetFloat (Item a -> Float)
    | GetBool (Item a -> Bool)
    | NoAccess


{-|


# Sort and Filter

-}
type Sorting
    = Unsorted
    | Ascending String
    | Descending String


type Filter
    = SimpleFilter
    | ValueFilter
    | Unfilterable


type FilterData
    = FilterData String (Dict String String)


type ValueFilter
    = BoolValueFilter (List Bool)
    | StringValueFilter (List String)
    | IntValueFilter (List Int)
    | FloatValueFilter (List Float)
    | NoValueFilter


type alias ValueDataFilter =
    Dict String (List String)


{-|


# Pagination

-}
type Pagination
    = Next
    | Previous
