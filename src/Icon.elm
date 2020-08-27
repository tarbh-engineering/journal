module Icon exposing (checkSquare, pencil, square)

import Html exposing (Html)
import Svg exposing (Svg, defs, svg)
import Svg.Attributes exposing (class, d, fill, height, points, rx, ry, stroke, strokeLinecap, strokeLinejoin, strokeWidth, version, viewBox, width, y)


pencil : String
pencil =
    "url(data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiIHN0YW5kYWxvbmU9Im5vIj8+CjxzdmcKICAgeG1sbnM6ZGM9Imh0dHA6Ly9wdXJsLm9yZy9kYy9lbGVtZW50cy8xLjEvIgogICB4bWxuczpjYz0iaHR0cDovL2NyZWF0aXZlY29tbW9ucy5vcmcvbnMjIgogICB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiCiAgIHhtbG5zOnN2Zz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciCiAgIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIKICAgaGVpZ2h0PSIyOC40IgogICB3aWR0aD0iMjguNSIKICAgaWQ9InN2ZzEyIgogICB2ZXJzaW9uPSIxLjEiCiAgIGVuYWJsZS1iYWNrZ3JvdW5kPSJuZXcgMCAwIDUwIDUwIgogICB2aWV3Qm94PSIwIDAgMjguNSAyOC40Ij4KICA8bWV0YWRhdGEKICAgICBpZD0ibWV0YWRhdGExOCI+CiAgICA8cmRmOlJERj4KICAgICAgPGNjOldvcmsKICAgICAgICAgcmRmOmFib3V0PSIiPgogICAgICAgIDxkYzpmb3JtYXQ+aW1hZ2Uvc3ZnK3htbDwvZGM6Zm9ybWF0PgogICAgICAgIDxkYzp0eXBlCiAgICAgICAgICAgcmRmOnJlc291cmNlPSJodHRwOi8vcHVybC5vcmcvZGMvZGNtaXR5cGUvU3RpbGxJbWFnZSIgLz4KICAgICAgICA8ZGM6dGl0bGU+PC9kYzp0aXRsZT4KICAgICAgPC9jYzpXb3JrPgogICAgPC9yZGY6UkRGPgogIDwvbWV0YWRhdGE+CiAgPGRlZnMKICAgICBpZD0iZGVmczE2IiAvPgogIDxwYXRoCiAgICAgaWQ9InBhdGgyIgogICAgIGQ9Ik0gMCwyOC40IDIuNSwxOC41IDE3LjQsMy42IDI0LjgsMTEgOS45LDI1LjkgWiBNIDQuMywxOS41IDIuOCwyNS42IDguOSwyNC4xIDIyLDExIDE3LjQsNi40IFoiIC8+CiAgPHBhdGgKICAgICBpZD0icGF0aDQiCiAgICAgZD0iTSA4LjIsMjUuMyBDIDcuNiwyMi44IDUuNiwyMC44IDMuMSwyMC4yIGwgMC41LC0xLjkgYyAzLjIsMC44IDUuNywzLjMgNi41LDYuNSB6IiAvPgogIDxwYXRoCiAgICAgaWQ9InBhdGg2IgogICAgIGQ9Ik0gMTkuNjk4LDcuMjg3IDIxLjExMiw4LjcwMSA4LjEwMiwyMS43MjEgNi42ODgsMjAuMzA5IFoiIC8+CiAgPHBhdGgKICAgICBpZD0icGF0aDgiCiAgICAgZD0iTSAxLjQsMjcgNC4zLDI2LjMgQyA0LDI1LjIgMy4yLDI0LjQgMi4xLDI0LjEgWiIgLz4KICA8cGF0aAogICAgIGlkPSJwYXRoMTAiCiAgICAgZD0iTSAyNS40LDEwLjQgMTgsMyAyMSwwIDIxLjUsMC4xIGMgMy42LDAuNSA2LjQsMy4zIDYuOSw2LjkgbCAwLjEsMC41IHogTSAyMC44LDMgMjUuNCw3LjYgMjYuMyw2LjcgQyAyNS44LDQuNCAyNCwyLjYgMjEuNywyLjEgWiIgLz4KPC9zdmc+Cg==), auto"


svgFeatherIcon : String -> List (Svg msg) -> Html msg
svgFeatherIcon className =
    svg
        [ class <| "feather feather-" ++ className
        , fill "none"
        , height "24"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "2"
        , viewBox "0 0 24 24"
        , width "24"
        ]


checkSquare : Html msg
checkSquare =
    svgFeatherIcon "check-square"
        [ Svg.polyline [ points "9 11 12 14 22 4" ] []
        , Svg.path [ d "M21 12v7a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11" ] []
        ]


square : Html msg
square =
    svgFeatherIcon "square"
        [ Svg.rect [ Svg.Attributes.x "3", y "3", width "18", height "18", rx "2", ry "2" ] []
        ]
