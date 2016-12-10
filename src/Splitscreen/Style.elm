module Splitscreen.Style exposing (..)

import Css exposing (..)
import Css.Namespace exposing (namespace)
import Html.CssHelpers

type CssClasses
    = UrlContent
    | ShowOnHover
    | Round
    | UrlBar
    | ColumnGrid
    | RowGrid
    | Row

css =
    (stylesheet << namespace "splitscreen")
        [ (.) UrlContent
            [ border (pt 0)
            , width (pct 100)
            , height (pct 100)
            , display block
            , position absolute
            ]
        , (.) ShowOnHover
            [ property "transition" "1s"
            , backgroundColor transparent
            , color transparent
            , hover
                [ backgroundColor (hex "ccc"), color (hex "111") ]
            ]
        , (.) Round
            [ borderRadius (pct 100)
            , property "transition" "background-color .3s"
            , margin (px 1)
            , borderColor (hex "111")
            , height (px 25)
            , width (px 25)
            , backgroundColor (hex "ccc")
            , focus [ textDecoration none ]
            , cursor pointer
            , display inlineBlock
            , textAlign center
            , fontSize (px 20)
            , hover
                [ backgroundColor (hex "ddd") ]
            ]
        , (.) UrlBar
            [ top (pt 0)
            , left (pt 0)
            , textAlign center
            , fontSize (pt 24)
            , position absolute
            , width (pct 100)
            , border (pt 0)
            , padding (pt 0)
            ]
        , (.) ColumnGrid
            [ displayFlex
            , borderRight3 (px 1) solid (hex "eee")
            , property "width" "calc(100vw - 10px)"
            ]
        , (.) RowGrid
            [ flexGrow (int 1)
            , displayFlex
            , flexDirection column
            , property "height" "calc(100vh - 10px)"
            ]
        , (.) Row
            [ position relative
            , flexGrow (int 1)
            ]
        ]


{ id, class, classList } =
    Html.CssHelpers.withNamespace "splitscreen"

compiled = (.css (Css.compile [ css ]))