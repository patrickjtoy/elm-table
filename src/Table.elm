module Table
    exposing
        ( Column
        , Fixed(Right, Left, None)
        , State
        , Msg
        , initialState
        , defaultColumn
        , withHeaderCell
        , withFooterCell
        , withHeaderAndFooterCells
        , withSorting
        , withFiltering
        , withSortingAndFiltering
        , update
        , view
        )

{-| This module provides a feature-rich and semantic table for Elm apps. It is currently under development and should NOT be used.

# Types and Aliases
@docs Column, Fixed, State, Msg

# Functions
@docs initialState, defaultColumn, withHeaderCell, withFooterCell, withHeaderAndFooterCells, withSorting, withFiltering, withSortingAndFiltering, update, view

-}

import Dict exposing (..)
import Dom exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode


-- MODEL --


type alias Point =
    ( Int, Int )


type SortDirection
    = Asc
    | Desc
    | Unsorted


type alias PopoverState =
    { visible : Bool
    , position : Point
    }


{-| Holds the internal Table state. You need to hold this state in your application's model.
-}
type alias State datum =
    { scrollPos : Point
    , sorter : Dict String SortDirection
    , popovers : Dict String PopoverState
    , filters : Dict String (datum -> Bool)
    }


{-| Creates the initial state for a Table.
-}
initialState : State datum
initialState =
    { scrollPos = ( 0, 0 )
    , sorter = Dict.empty
    , popovers = Dict.empty
    , filters = Dict.empty
    }



-- UPDATE --


{-| Messages used internally by the Table module. You need to map the return from the view function to a message in your app and pass the
received sub-message to the Table module's update function. See the example including with the view function below.
-}
type Msg datum
    = SetScrollPos Point
    | SortData String (datum -> datum -> Order) SortDirection
    | ClosePopovers
    | SetVisiblePopover String String Bool Point
    | FilterData String (datum -> String) String
    | NoOp


type alias Result datum msg =
    { data : List datum
    , state : State datum
    , command : Maybe (Cmd msg)
    }


closePopovers : Dict String PopoverState -> Dict String PopoverState
closePopovers popovers =
    Dict.map (\_ filter -> { filter | visible = False }) popovers


sortData : (datum -> datum -> Order) -> SortDirection -> List datum -> List datum -> List datum
sortData sorter direction originalData currentData =
    case direction of
        Unsorted ->
            List.filter ((flip List.member) currentData) originalData

        Asc ->
            List.sortWith sorter currentData

        Desc ->
            List.sortWith sorter currentData
                |> List.reverse


filterData : Dict String (a -> Bool) -> a -> Bool
filterData filters datum =
    List.all (\filter -> filter datum) (Dict.values filters)


nextFilters : String -> String -> (a -> String) -> Dict String (a -> Bool) -> Dict String (a -> Bool)
nextFilters id text accessor filters =
    if String.isEmpty text then
        Dict.remove id filters
    else
        Dict.insert id (\datum -> String.contains text (accessor datum)) filters


nextPopovers : String -> Bool -> Point -> Dict String PopoverState -> Dict String PopoverState
nextPopovers id visible point currentPopovers =
    Dict.map (\_ popover -> { popover | visible = False }) currentPopovers
        |> Dict.insert id (PopoverState visible point)


{-| Handles the messages defined above. This function must be called from your application's update function with the Table.Msg, the
original data set, the current data set (stored in your model), the current Table.State (stored in your model), and your application's
NoOp Msg.
-}
update : Msg datum -> List datum -> List datum -> State datum -> msg -> Result datum msg
update msg originalData currentData state noOpMsg =
    case msg of
        SetScrollPos position ->
            { data = currentData
            , state =
                { state
                    | scrollPos = position
                    , popovers = (closePopovers state.popovers)
                }
            , command = Maybe.Nothing
            }

        SortData id sorter direction ->
            { data = (sortData sorter direction originalData currentData)
            , state = { state | sorter = Dict.singleton id direction }
            , command = Maybe.Nothing
            }

        ClosePopovers ->
            { data = currentData
            , state = { state | popovers = closePopovers state.popovers }
            , command = Maybe.Nothing
            }

        SetVisiblePopover id node visible point ->
            let
                task : Cmd msg
                task =
                    Task.attempt (\_ -> noOpMsg) (Dom.focus node)
            in
                { data = currentData
                , state = { state | popovers = nextPopovers id visible point state.popovers }
                , command = Maybe.Just task
                }

        FilterData id accessor text ->
            let
                activeFilters : Dict String (datum -> Bool)
                activeFilters =
                    nextFilters id text accessor state.filters
            in
                { data = (List.filter (filterData activeFilters) originalData)
                , state = { state | filters = activeFilters }
                , command = Maybe.Nothing
                }

        NoOp ->
            Result currentData state Maybe.Nothing



-- VIEW --


{-| Provides types for fixed columns. This feature is not yet implemented.
-}
type Fixed
    = Right
    | Left
    | None


type alias Filtering datum =
    { accessor : datum -> String
    , label : String
    }


{-| The shape of a column for a particular set of data. See the examples provided within this repository for usage details.
-}
type alias Column datum =
    { id : String
    , bodyCell : datum -> String
    , fixed : Maybe Fixed
    , sorting : Maybe (datum -> datum -> Order)
    , filtering : Maybe (Filtering datum)
    , headerCell : Maybe String
    , footerCell : Maybe (List datum -> String)
    }


{-| Used to quickly create a basic Column with no frills

    defaultColumn "name" .name =
        { id = "name"
        , bodyCell = "Jane Doe"
        , fixed = Maybe.Nothing
        , sorting = Maybe.Nothing
        , filtering = Maybe.Nothing
        , headerCell = Maybe.Nothing
        , footerCell = Maybe.Nothing
        }
-}
defaultColumn : String -> (datum -> String) -> Column datum
defaultColumn id bodyCell =
    { id = id
    , bodyCell = bodyCell
    , fixed = Maybe.Nothing
    , sorting = Maybe.Nothing
    , filtering = Maybe.Nothing
    , headerCell = Maybe.Nothing
    , footerCell = Maybe.Nothing
    }


{-| Adds a headerCell to a Column

    defaultColumn "name" .name |> withHeaderCell "Name" =
        { id = "name"
        , bodyCell = "Jane Doe"
        , fixed = Maybe.Nothing
        , sorting = Maybe.Nothing
        , filtering = Maybe.Nothing
        , headerCell = Maybe.Just "Name"
        , footerCell = Maybe.Nothing
        }
-}
withHeaderCell : String -> Column datum -> Column datum
withHeaderCell headerCell column =
    { column | headerCell = Maybe.Just headerCell }


{-| Adds a footerCell to a Column

    defaultColumn "name" .name |> withFooterCell (List.sum >> toString) =
        { id = "name"
        , bodyCell = "Jane Doe"
        , fixed = Maybe.Nothing
        , sorting = Maybe.Nothing
        , filtering = Maybe.Nothing
        , headerCell = Maybe.Nothing
        , footerCell = Maybe.Just "42"
        }
-}
withFooterCell : (List datum -> String) -> Column datum -> Column datum
withFooterCell footerCell column =
    { column | footerCell = Maybe.Just footerCell }


{-| Adds a headerCell and footerCell to a Column

    defaultColumn "name" .name |> withHeaderAndFooterCells "Name" (List.sum >> toString) =
        { id = "name"
        , bodyCell = "Jane Doe"
        , fixed = Maybe.Nothing
        , sorting = Maybe.Nothing
        , filtering = Maybe.Nothing
        , headerCell = Maybe.Just "Name"
        , footerCell = Maybe.Just "42"
        }
-}
withHeaderAndFooterCells : String -> (List datum -> String) -> Column datum -> Column datum
withHeaderAndFooterCells headerCell footerCell column =
    (withHeaderCell headerCell column) |> withFooterCell footerCell


{-| Adds sorting to a Column

    defaultColumn "name" .name |> withSorting (\x y -> compare x.name y.name) =
        { id = "name"
        , bodyCell = "Jane Doe"
        , fixed = Maybe.Nothing
        , sorting = Maybe.Just (\x y -> compare x.name y.name)
        , filtering = Maybe.Nothing
        , headerCell = Maybe.Nothing
        , footerCell = Maybe.Nothing
        }
-}
withSorting : (datum -> datum -> Order) -> Column datum -> Column datum
withSorting sorting column =
    { column | sorting = Maybe.Just sorting }


{-| Adds filtering to a Column

    defaultColumn "name" .name |> withFiltering { accessor = .name, label = "Find Name" } =
        { id = "name"
        , bodyCell = "Jane Doe"
        , fixed = Maybe.Nothing
        , sorting = Maybe.Nothing
        , filtering = Maybe.Just { accessor = .name, label = "Find Name" }
        , headerCell = Maybe.Nothing
        , footerCell = Maybe.Nothing
        }
-}
withFiltering : Filtering datum -> Column datum -> Column datum
withFiltering filtering column =
    { column | filtering = Maybe.Just filtering }


{-| Adds sorting and filtering to a Column

    defaultColumn "name" .name |> withSortingAndFiltering (\x y -> compare x.name y.name) { accessor = .name, label = "Find Name" } =
        { id = "name"
        , bodyCell = "Jane Doe"
        , fixed = Maybe.Nothing
        , sorting = Maybe.Just (\x y -> compare x.name y.name)
        , filtering = Maybe.Just { accessor = .name, label = "Find Name" }
        , headerCell = Maybe.Nothing
        , footerCell = Maybe.Nothing
        }
-}
withSortingAndFiltering : (datum -> datum -> Order) -> Filtering datum -> Column datum -> Column datum
withSortingAndFiltering sorting filtering column =
    (withSorting sorting column) |> withFiltering filtering


type alias Config datum =
    { columns : List (Column datum)
    , data : List datum
    , classPrefix : String
    , isScrollable : Bool
    }


isJust : Maybe a -> Bool
isJust m =
    case m of
        Nothing ->
            False

        Just _ ->
            True


emptyNode : Html (Msg datum)
emptyNode =
    Html.span [] []


ignoreClick : Html.Attribute (Msg datum)
ignoreClick =
    Html.Events.onWithOptions "click" { stopPropagation = True, preventDefault = False } (Decode.succeed NoOp)


onScroll : (Point -> Msg datum) -> Html.Attribute (Msg datum)
onScroll tagger =
    Html.Events.on "scroll" (Decode.map tagger scrollPointParser)


scrollPointParser : Decode.Decoder Point
scrollPointParser =
    Decode.map2 (,)
        (Decode.at [ "target", "scrollLeft" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] Decode.int)


colgroup : List (Column datum) -> String -> Html (Msg datum)
colgroup columns classPrefix =
    Html.colgroup [ Html.Attributes.class (classPrefix ++ "-table-colgroup") ]
        (List.map
            (\{ id } ->
                Html.col
                    [ Html.Attributes.classList [ ( classPrefix ++ "-table-col", True ), ( classPrefix ++ "-table-col__" ++ id, True ) ] ]
                    []
            )
            columns
        )


popover : String -> (String -> Msg datum) -> String -> PopoverState -> String -> Html (Msg datum)
popover id inputMsg label popoverState classPrefix =
    Html.div
        [ Html.Attributes.classList
            [ ( classPrefix ++ "-table-filter-popover", True )
            , ( classPrefix ++ "-table-filter-popover__visible", popoverState.visible )
            ]
        , Html.Attributes.style
            [ ( "left", toString ((Tuple.first popoverState.position)) ++ "px" )
            , ( "top", toString ((Tuple.second popoverState.position)) ++ "px" )
            ]
        , ignoreClick
        ]
        [ Html.label [ Html.Attributes.class (classPrefix ++ "-table-filter-popover-label") ] [ text label ]
        , Html.input
            [ Html.Attributes.type_ "text"
            , Html.Attributes.id (id ++ "-popover-input")
            , Html.Attributes.class (classPrefix ++ "-table-filter-popover-input")
            , ignoreClick
            , Html.Events.onInput inputMsg
            ]
            []
        ]


filterIcon : String -> String -> Filtering datum -> Dict String PopoverState -> Html (Msg datum)
filterIcon id classPrefix filtering currentFilters =
    let
        decoder : String -> Decode.Decoder Int
        decoder prop =
            Decode.at [ "target", "parentElement", "parentElement", "parentElement", prop ] Decode.int

        parser : Decode.Decoder Point
        parser =
            Decode.map2 (\x y -> ( x + 15, y + 20 ))
                (decoder "offsetLeft")
                (decoder "offsetTop")

        onClick : (Point -> Msg datum) -> Html.Attribute (Msg datum)
        onClick tagger =
            Html.Events.onWithOptions "click" { stopPropagation = True, preventDefault = False } (Decode.map tagger parser)

        popoverState : PopoverState
        popoverState =
            Dict.get id currentFilters |> Maybe.withDefault (PopoverState False ( 0, 0 ))
    in
        Html.div [ Html.Attributes.class (classPrefix ++ "-table-filter") ]
            [ Html.span []
                [ Html.i
                    [ Html.Attributes.classList
                        [ ( "fa", True )
                        , ( "fa-filter", True )
                        , ( classPrefix ++ "-table-icon", True )
                        , ( classPrefix ++ "-table-filter-icon", True )
                        , ( classPrefix ++ "-table-filter-icon__active", popoverState.visible )
                        ]
                    , onClick (SetVisiblePopover id (id ++ "-popover-input") (not popoverState.visible))
                    ]
                    []
                ]
            , popover id (FilterData id filtering.accessor) filtering.label popoverState classPrefix
            ]


sortIcons : String -> Dict String SortDirection -> String -> Html (Msg datum)
sortIcons id sortState classPrefix =
    let
        isActive : SortDirection -> Bool
        isActive direction =
            Dict.map (\stateId stateDirection -> stateId == id && stateDirection == direction) sortState
                |> Dict.get id
                |> Maybe.withDefault False
    in
        Html.div [ Html.Attributes.class (classPrefix ++ "-table-sorter") ]
            [ Html.span []
                [ Html.i
                    [ Html.Attributes.classList
                        [ ( "fa", True )
                        , ( "fa-sort-asc", True )
                        , ( classPrefix ++ "-table-icon", True )
                        , ( classPrefix ++ "-table-sorter-asc", True )
                        , ( classPrefix ++ "-table-icon__active", isActive Asc )
                        ]
                    ]
                    []
                ]
            , Html.span []
                [ Html.i
                    [ Html.Attributes.classList
                        [ ( "fa", True )
                        , ( "fa-sort-desc", True )
                        , ( classPrefix ++ "-table-icon", True )
                        , ( classPrefix ++ "-table-sorter-desc", True )
                        , ( classPrefix ++ "-table-icon__active", isActive Desc )
                        ]
                    ]
                    []
                ]
            ]


nextDirection : SortDirection -> SortDirection
nextDirection direction =
    case direction of
        Unsorted ->
            Asc

        Asc ->
            Desc

        Desc ->
            Unsorted


directionOrElse : String -> Dict String SortDirection -> SortDirection
directionOrElse id sorter =
    Maybe.map nextDirection (Dict.get id sorter)
        |> Maybe.withDefault Asc


headerCell : String -> State datum -> Column datum -> Html (Msg datum)
headerCell classPrefix state ({ id, sorting, filtering } as column) =
    let
        onClickEvent =
            sorting
                |> Maybe.map (\f -> SortData id f (directionOrElse id state.sorter))
                |> Maybe.withDefault NoOp
    in
        Html.th
            [ Html.Attributes.class (classPrefix ++ "-table-th")
            , Html.Events.onClick onClickEvent
            ]
            [ filtering
                |> Maybe.map (\f -> filterIcon id classPrefix f state.popovers)
                |> Maybe.withDefault emptyNode
            , column.headerCell |> Maybe.withDefault "" |> text
            , if isJust sorting then
                sortIcons id state.sorter classPrefix
              else
                emptyNode
            ]


header : List (Column datum) -> String -> State datum -> Html (Msg datum)
header columns classPrefix state =
    Html.thead [ Html.Attributes.class (classPrefix ++ "-table-thead") ]
        [ Html.tr [ Html.Attributes.class (classPrefix ++ "-table-tr") ] (List.map (headerCell classPrefix state) columns) ]


body : List (Column datum) -> List datum -> String -> Html (Msg datum)
body columns data classPrefix =
    Html.tbody [ Html.Attributes.class (classPrefix ++ "-table-tbody") ]
        (List.map
            (\datum ->
                Html.tr [ Html.Attributes.class (classPrefix ++ "-table-tr") ]
                    (List.map
                        (\{ bodyCell } ->
                            Html.td [ Html.Attributes.class (classPrefix ++ "-table-td") ] [ bodyCell datum |> text ]
                        )
                        columns
                    )
            )
            data
        )


footerCell : String -> List datum -> Column datum -> Html (Msg datum)
footerCell classPrefix data column =
    Html.th
        [ Html.Attributes.class (classPrefix ++ "-table-tf") ]
        [ column.footerCell
            |> Maybe.map (\f -> f data)
            |> Maybe.withDefault ""
            |> text
        ]


footer : List (Column datum) -> List datum -> String -> Html (Msg datum)
footer columns data classPrefix =
    Html.tfoot [ Html.Attributes.class (classPrefix ++ "-table-tfoot") ]
        [ Html.tr [ Html.Attributes.class (classPrefix ++ "-table-tr") ]
            (List.map (footerCell classPrefix data) columns)
        ]


staticTable : List (Column datum) -> List datum -> String -> State datum -> Html (Msg datum)
staticTable columns data classPrefix state =
    div [ Html.Attributes.classList [ ( classPrefix ++ "-table-static", True ), ( "table-static", True ) ] ]
        [ Html.table [ Html.Attributes.class (classPrefix ++ "-table-fixed") ]
            [ colgroup columns classPrefix
            , if List.any (.headerCell >> isJust) columns then
                header columns classPrefix state
              else
                emptyNode
            , body columns data classPrefix
            , if List.any (.footerCell >> isJust) columns then
                footer columns data classPrefix
              else
                emptyNode
            ]
        ]


scrollTable : List (Column datum) -> List datum -> String -> State datum -> Html (Msg datum)
scrollTable columns data classPrefix state =
    div [ Html.Attributes.classList [ ( classPrefix ++ "-table-scroll", True ), ( "table-scroll", True ) ] ]
        [ if List.any (.headerCell >> isJust) columns then
            div
                [ Html.Attributes.classList
                    [ ( classPrefix ++ "-table-header", True )
                    , ( "table-header-fixed", True )
                    ]
                , Html.Attributes.property "scrollLeft" (Encode.int (Tuple.first state.scrollPos))
                ]
                [ Html.table
                    [ Html.Attributes.class (classPrefix ++ "-table-fixed") ]
                    [ colgroup columns classPrefix
                    , header columns classPrefix state
                    ]
                ]
          else
            emptyNode
        , div
            [ Html.Attributes.classList
                [ ( classPrefix ++ "-table-body", True )
                , ( "table-scroll-body", True )
                ]
            , onScroll SetScrollPos
            ]
            [ Html.table
                [ Html.Attributes.class (classPrefix ++ "-table-fixed") ]
                [ colgroup columns classPrefix
                , body columns data classPrefix
                ]
            ]
        , if List.any (.footerCell >> isJust) columns then
            div
                [ Html.Attributes.classList
                    [ ( classPrefix ++ "-table-footer", True )
                    , ( "table-footer-fixed", True )
                    ]
                , Html.Attributes.property "scrollLeft" (Encode.int (Tuple.first state.scrollPos))
                ]
                [ Html.table
                    [ Html.Attributes.class (classPrefix ++ "-table-fixed") ]
                    [ colgroup columns classPrefix
                    , footer columns data classPrefix
                    ]
                ]
          else
            emptyNode
        ]


{-| The entry point for the Table module. You will need to call this from your application's view function. See the examples provided
within this repository for usage details.
-}
view : Config datum -> State datum -> Html (Msg datum)
view { columns, data, classPrefix, isScrollable } state =
    Html.div
        [ Html.Attributes.class (classPrefix ++ "-table-wrapper")
        , Html.Events.onClick
            (if List.any (.filtering >> isJust) columns then
                ClosePopovers
             else
                NoOp
            )
        ]
        [ div [ Html.Attributes.class (classPrefix ++ "-table") ]
            [ div [ Html.Attributes.class (classPrefix ++ "-table-content") ]
                [ if isScrollable then
                    scrollTable columns data classPrefix state
                  else
                    staticTable columns data classPrefix state
                ]
            ]
        ]
