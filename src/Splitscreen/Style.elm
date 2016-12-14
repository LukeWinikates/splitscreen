module Splitscreen.Style exposing (..)

import Css exposing (..)
import Css.Elements exposing (body)
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
    | Prose
    | ButtonColumn
    | Disabled
    | Enabled


css =
    (stylesheet << namespace "splitscreen")
        [ body
            [ fontFamily sansSerif
            , color (hex "aaa")
            ]
        , (.) UrlContent
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
            , margin (px 4)
            , borderColor (hex "111")
            , height (px 25)
            , width (px 25)
            , backgroundColor (hex "ccc")
            , focus [ textDecoration none ]
            , cursor pointer
            , display inlineBlock
            , textAlign center
            , fontSize (px 20)
            ]
        , (.) Enabled
            [ hover
                [ backgroundColor (hex "f4427a")
                , color (hex "111")
                , fontWeight bold
                ]
            ]
        , (.) Disabled
            [ cursor default
            , backgroundColor (hex "eee")
            , color (hex "ddd")
            ]
        , (.) UrlBar
            [ top (pt 0)
            , left (pt 0)
            , textAlign center
            , fontSize (pt 24)
            , position absolute
            , width (pct 100)
            , border (pt 0)
            , padding2 (pt 4) (pt 0)
            , focus
                [ outline none
                ]
            ]
        , (.) ColumnGrid
            [ displayFlex
            , borderRight3 (px 1) solid (hex "eee")
            ]
        , (.) RowGrid
            [ flexGrow (int 1)
            , displayFlex
            , flexDirection column
            , height (vh 100)
            ]
        , (.) Row
            [ position relative
            , flexGrow (int 1)
            ]
        , (.) ButtonColumn
            [ children
                [ everything
                    [ display block ]
                ]
            ]
        , (.) Prose
            [ fontSize (pt 20)
            , maxWidth (pct 100)
            , textAlign center
            ]
        ]


{ id, class, classList } =
    Html.CssHelpers.withNamespace "splitscreen"


compiled =
    (.css (Css.compile [ css ]))
