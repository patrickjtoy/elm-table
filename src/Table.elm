module Table exposing (Column, Fixed(Right, Left, None), State, Msg, initialState, update, view)

{-| This module provides a feature-rich and semantic table for Elm apps. It is currently under development and should NOT be used.

# Types and Aliases
@docs Column, Fixed, State, Msg

# Functions
@docs initialState, update, view

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
type alias State a =
    { scrollPos : Point
    , sorter : Dict String SortDirection
    , popovers : Dict String PopoverState
    , filters : Dict String (a -> Bool)
    }


{-| Creates the initial state for a Table.
-}
initialState : State a
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
type Msg a
    = SetScrollPos Point
    | SortData String (a -> a -> Order) SortDirection
    | ClosePopovers
    | SetVisiblePopover String String Bool Point
    | FilterData String (a -> String) String
    | NoOp


type alias Result a msg =
    { data : List a
    , state : State a
    , command : Maybe (Cmd msg)
    }


closePopovers : Dict String PopoverState -> Dict String PopoverState
closePopovers popovers =
    Dict.map (\_ filter -> { filter | visible = False }) popovers


sortData : (a -> a -> Order) -> SortDirection -> List a -> List a -> List a
sortData sorter direction originalData currentData =
    case direction of
        Unsorted ->
            List.filter ((flip List.member) currentData) originalData

        Asc ->
            List.sortWith sorter (currentData)

        Desc ->
            List.sortWith sorter (currentData)
                |> List.reverse


filterData : Dict String (a -> Bool) -> a -> Bool
filterData filters datum =
    List.all (\filter -> filter datum) (Dict.values filters)


activeFilters : String -> String -> (a -> String) -> Dict String (a -> Bool) -> Dict String (a -> Bool)
activeFilters id text accessor filters =
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
update : Msg a -> List a -> List a -> State a -> msg -> Result a msg
update msg originalData currentData state noOpMsg =
    case msg of
        SetScrollPos position ->
            { data = currentData
            , state = { state | scrollPos = position, popovers = (closePopovers state.popovers) }
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
                task =
                    Task.attempt (\_ -> noOpMsg) (Dom.focus node)
            in
                { data = currentData
                , state = { state | popovers = nextPopovers id visible point state.popovers }
                , command = Maybe.Just task
                }

        FilterData id accessor text ->
            let
                nextFilters =
                    activeFilters id text accessor state.filters
            in
                { data = (List.filter (filterData nextFilters) originalData)
                , state = { state | filters = nextFilters }
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


type alias Filtering a =
    { filterable : Bool
    , accessor : a -> String
    , label : String
    }


type alias Sorting a =
    { sortable : Bool
    , sorter : a -> a -> Order
    }


{-| The shape of a column for a particular set of data. See the examples provided within this repository for usage details.
-}
type alias Column a =
    { id : String
    , fixed : Fixed
    , sorting : Sorting a
    , filtering : Filtering a
    , header : Html (Msg a)
    , body : a -> Html (Msg a)
    , footer : List a -> Html (Msg a)
    }


type alias Props a =
    { columns : List (Column a)
    , data : List a
    , classPrefix : String
    , isScrollable : Bool
    }


onScroll : (Point -> Msg a) -> Html.Attribute (Msg a)
onScroll tagger =
    Html.Events.on "scroll" (Decode.map tagger scrollPointParser)


scrollPointParser : Decode.Decoder Point
scrollPointParser =
    Decode.map2 (,)
        (Decode.at [ "target", "scrollLeft" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] Decode.int)


colgroup : List (Column a) -> String -> Html (Msg a)
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


popover : String -> (a -> String) -> String -> PopoverState -> String -> Html (Msg a)
popover id accessor label popoverState classPrefix =
    let
        ignoreClick : Html.Attribute (Msg a)
        ignoreClick =
            Html.Events.onWithOptions "click" { stopPropagation = True, preventDefault = False } (Decode.succeed NoOp)
    in
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
                , Html.Events.onInput (FilterData id accessor)
                ]
                []
            ]


filterIcon : String -> (a -> String) -> String -> Dict String PopoverState -> String -> Html (Msg a)
filterIcon id accessor label filters classPrefix =
    let
        decoder : String -> Decode.Decoder Int
        decoder prop =
            Decode.at [ "target", "parentElement", "parentElement", "parentElement", prop ] Decode.int

        parser : Decode.Decoder Point
        parser =
            Decode.map2 (\x y -> ( x + 15, y + 20 ))
                (decoder "offsetLeft")
                (decoder "offsetTop")

        onClick : (Point -> Msg a) -> Html.Attribute (Msg a)
        onClick tagger =
            Html.Events.onWithOptions "click" { stopPropagation = True, preventDefault = False } (Decode.map tagger parser)

        popoverState : PopoverState
        popoverState =
            Dict.get id filters |> Maybe.withDefault (PopoverState False ( 0, 0 ))
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
            , popover id accessor label popoverState classPrefix
            ]


sortIcons : String -> Dict String SortDirection -> String -> Html (Msg a)
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


header : List (Column a) -> State a -> String -> Html (Msg a)
header columns state classPrefix =
    let
        nextDirection : SortDirection -> SortDirection
        nextDirection dir =
            case dir of
                Unsorted ->
                    Asc

                Asc ->
                    Desc

                Desc ->
                    Unsorted

        directionOrElse : String -> SortDirection
        directionOrElse id =
            Maybe.map nextDirection (Dict.get id state.sorter)
                |> Maybe.withDefault Asc

        contents : Column a -> Html (Msg a)
        contents { id, filtering, header, sorting } =
            Html.th
                [ Html.Attributes.class (classPrefix ++ "-table-th")
                , Html.Events.onClick (SortData id sorting.sorter (directionOrElse id))
                ]
                [ if filtering.filterable then
                    filterIcon id filtering.accessor filtering.label state.popovers classPrefix
                  else
                    Html.span [] []
                , header
                , if sorting.sortable then
                    sortIcons id state.sorter classPrefix
                  else
                    Html.span [] []
                ]
    in
        Html.thead [ Html.Attributes.class (classPrefix ++ "-table-thead") ]
            [ Html.tr [ Html.Attributes.class (classPrefix ++ "-table-tr") ] (List.map contents columns) ]


body : List a -> List (Column a) -> String -> Html (Msg a)
body data columns classPrefix =
    Html.tbody [ Html.Attributes.class (classPrefix ++ "-table-tbody") ]
        (List.map
            (\datum ->
                Html.tr [ Html.Attributes.class (classPrefix ++ "-table-tr") ]
                    (List.map (\{ body } -> Html.td [ Html.Attributes.class (classPrefix ++ "-table-td") ] [ body datum ]) columns)
            )
            data
        )


footer : List a -> List (Column a) -> String -> Html (Msg a)
footer data columns classPrefix =
    Html.tfoot [ Html.Attributes.class (classPrefix ++ "-table-tfoot") ]
        [ Html.tr [ Html.Attributes.class (classPrefix ++ "-table-tr") ]
            (List.map (\{ footer } -> Html.th [ Html.Attributes.class (classPrefix ++ "-table-tf") ] [ footer data ]) columns)
        ]


scroll : List a -> List (Column a) -> String -> State a -> Html (Msg a)
scroll data columns classPrefix state =
    div [ Html.Attributes.classList [ ( classPrefix ++ "-table-scroll", True ), ( "table-scroll", True ) ] ]
        [ div
            [ Html.Attributes.classList
                [ ( classPrefix ++ "-table-header", True )
                , ( "table-header-fixed", True )
                ]
            , Html.Attributes.property "scrollLeft" (Encode.int (Tuple.first state.scrollPos))
            ]
            [ Html.table
                [ Html.Attributes.class (classPrefix ++ "-table-fixed") ]
                [ colgroup columns classPrefix
                , header columns state classPrefix
                ]
            ]
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
                , body data columns classPrefix
                ]
            ]
        , div
            [ Html.Attributes.classList
                [ ( classPrefix ++ "-table-footer", True )
                , ( "table-footer-fixed", True )
                ]
            , Html.Attributes.property "scrollLeft" (Encode.int (Tuple.first state.scrollPos))
            ]
            [ Html.table
                [ Html.Attributes.class (classPrefix ++ "-table-fixed") ]
                [ colgroup columns classPrefix
                , footer data columns classPrefix
                ]
            ]
        ]


{-| The entry point for the Table module. You will need to call this from your application's view function. See the examples provided
within this repository for usage details.
-}
view : Props a -> State a -> Html (Msg a)
view { columns, data, classPrefix, isScrollable } state =
    Html.div
        [ Html.Attributes.class (classPrefix ++ "-table-wrapper")
        , Html.Events.onClick ClosePopovers
        ]
        [ div [ Html.Attributes.class (classPrefix ++ "-table") ]
            [ div [ Html.Attributes.class (classPrefix ++ "-table-content") ]
                [ if isScrollable then
                    scroll data columns classPrefix state
                  else
                    Html.table []
                        [ colgroup columns classPrefix
                        , header columns state classPrefix
                        , body data columns classPrefix
                        , footer data columns classPrefix
                        ]
                ]
            ]
        ]
