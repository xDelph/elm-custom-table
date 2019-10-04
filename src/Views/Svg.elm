module Views.Svg exposing (cross, filter, setting, sortAscending, sortDescending)

import Svg exposing (..)
import Svg.Attributes exposing (..)



-- svg converted to ELM with https://levelteams.com/svg-to-elm


filter =
    svg [ class "svgFilter", width "10", height "7", viewBox "0 0 10 7", fill "none" ] [ Svg.mask [ id "path-1-inside-1", fill "white" ] [ Svg.path [ d "M0 0H10V1H0V0Z" ] [], Svg.path [ d "M2 3H8V4H2V3Z" ] [], Svg.path [ d "M4 6H6V7H4V6Z" ] [] ], Svg.path [ d "M0 0H10V1H0V0Z", fill "#AAAAAA" ] [], Svg.path [ d "M2 3H8V4H2V3Z", fill "#AAAAAA" ] [], Svg.path [ d "M4 6H6V7H4V6Z", fill "#AAAAAA" ] [], Svg.path [ d "M0 0V-1H-1V0H0ZM10 0H11V-1H10V0ZM10 1V2H11V1H10ZM0 1H-1V2H0V1ZM2 3V2H1V3H2ZM8 3H9V2H8V3ZM8 4V5H9V4H8ZM2 4H1V5H2V4ZM4 6V5H3V6H4ZM6 6H7V5H6V6ZM6 7V8H7V7H6ZM4 7H3V8H4V7ZM0 1H10V-1H0V1ZM9 0V1H11V0H9ZM10 0H0V2H10V0ZM1 1V0H-1V1H1ZM2 4H8V2H2V4ZM7 3V4H9V3H7ZM8 3H2V5H8V3ZM3 4V3H1V4H3ZM4 7H6V5H4V7ZM5 6V7H7V6H5ZM6 6H4V8H6V6ZM5 7V6H3V7H5Z", fill "#AAAAAA", Svg.Attributes.mask "url(#path-1-inside-1)" ] [] ]


cross =
    svg [ class "svgCross", width "32", height "48", viewBox "0 0 32 48", fill "none" ] [ Svg.path [ d "M11 19L21 29M21 19L11 29", stroke "#AAAAAA", strokeLinecap "round" ] [] ]


setting =
    svg [ class "svgSetting", width "24", height "24", viewBox "0 0 24 24", fill "none" ] [ Svg.path [ d "M7 6V10M7 12V18M7 12H5M7 12H9M12 6V9M12 9H14M12 9H10M12 11V18M17 18V15M17 15H15M17 15H19M17 13V6", stroke "#AAAAAA", strokeLinecap "round" ] [] ]


sortDescending =
    svg [ class "svgSortDescending", width "12", height "10", viewBox "0 0 12 10", fill "none" ] [ Svg.path [ d "M0.5 9H7.5M6 7H0.5M0.5 5H4.5M3 3H0.5M10 2.5V8M10 8L8.5 6.5M10 8L11.5 6.5M1.5 1H0.5", stroke "#AAAAAA", strokeLinecap "round" ] [] ]


sortAscending =
    svg [ class "svgSortAscending", width "12", height "10", viewBox "0 0 12 10", fill "none" ] [ Svg.path [ d "M0.5 9H1.5M3 7H0.5M0.5 5H4.5M6 3H0.5M10 2.5V8M10 8L8.5 6.5M10 8L11.5 6.5M7.5 1H0.5", stroke "#AAAAAA", strokeLinecap "round" ] [] ]
