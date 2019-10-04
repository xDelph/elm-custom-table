port module Ports exposing (scrolledTo, updateCustomTable)

import CustomTableType


port updateCustomTable : Bool -> Cmd msg


port scrolledTo : (CustomTableType.ScrollEvent -> a) -> Sub a
