module Main exposing (..)

import Html exposing (Html, div, h1, text)
import Table


main : Program Never Model Msg
main =
    Html.program
        { init = init presidents
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { people : List Person
    , tableState : Table.State Person
    }


init : List Person -> ( Model, Cmd Msg )
init people =
    let
        model =
            { people = people
            , tableState = Table.initialState
            }
    in
        ( model, Cmd.none )



-- UPDATE


type Msg
    = TableEvent (Table.Msg Person)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TableEvent subMsg ->
            let
                result =
                    Table.update subMsg presidents model.people model.tableState NoOp
            in
                ( { model | people = result.data, tableState = result.state }, Maybe.withDefault Cmd.none result.command )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { people, tableState } =
    let
        props =
            { columns = columns
            , data = people
            , classPrefix = "presidents"
            , isScrollable = False
            }
    in
        div []
            [ h1 [] [ text "Birthplaces of U.S. Presidents" ]
            , Html.map TableEvent (Table.view props tableState)
            ]



-- CONFIG


render : String -> Html msg
render value =
    Html.span [] [ Html.text value ]


sorter : (Person -> comparable) -> Person -> Person -> Order
sorter prop x y =
    compare (prop x) (prop y)


columns : List (Table.Column Person)
columns =
    [ { id = "name"
      , fixed = Table.None
      , sorting =
            { sortable = False
            , sorter = sorter .name
            }
      , filtering =
            { filterable = False
            , accessor = .name
            , label = "Find Name"
            }
      , header = render "Name"
      , body = .name >> render
      , footer = (\_ -> "--") >> render
      }
    , { id = "year"
      , fixed = Table.None
      , sorting =
            { sortable = False
            , sorter = sorter .year
            }
      , filtering =
            { filterable = False
            , accessor = .year >> toString
            , label = "Find Year"
            }
      , header = render "Year"
      , body = .year >> toString >> render
      , footer = (\_ -> "--") >> render
      }
    , { id = "city"
      , fixed = Table.None
      , sorting =
            { sortable = False
            , sorter = sorter .city
            }
      , filtering =
            { filterable = False
            , accessor = .city
            , label = "Find City"
            }
      , header = render "City"
      , body = .city >> render
      , footer = (\_ -> "--") >> render
      }
    , { id = "state"
      , fixed = Table.None
      , sorting =
            { sortable = False
            , sorter = sorter .state
            }
      , filtering =
            { filterable = False
            , accessor = .state
            , label = "Find State"
            }
      , header = render "State"
      , body = .state >> render
      , footer = (\_ -> "--") >> render
      }
    ]


type alias Person =
    { name : String
    , year : Int
    , city : String
    , state : String
    }


presidents : List Person
presidents =
    [ Person "George Washington" 1732 "Westmoreland County" "Virginia"
    , Person "John Adams" 1735 "Braintree" "Massachusetts"
    , Person "Thomas Jefferson" 1743 "Shadwell" "Virginia"
    , Person "James Madison" 1751 "Port Conway" "Virginia"
    , Person "James Monroe" 1758 "Monroe Hall" "Virginia"
    , Person "Andrew Jackson" 1767 "Waxhaws Region" "South/North Carolina"
    , Person "John Quincy Adams" 1767 "Braintree" "Massachusetts"
    , Person "William Henry Harrison" 1773 "Charles City County" "Virginia"
    , Person "Martin Van Buren" 1782 "Kinderhook" "New York"
    , Person "Zachary Taylor" 1784 "Barboursville" "Virginia"
    , Person "John Tyler" 1790 "Charles City County" "Virginia"
    , Person "James Buchanan" 1791 "Cove Gap" "Pennsylvania"
    , Person "James K. Polk" 1795 "Pineville" "North Carolina"
    , Person "Millard Fillmore" 1800 "Summerhill" "New York"
    , Person "Franklin Pierce" 1804 "Hillsborough" "New Hampshire"
    , Person "Andrew Johnson" 1808 "Raleigh" "North Carolina"
    , Person "Abraham Lincoln" 1809 "Sinking spring" "Kentucky"
    , Person "Ulysses S. Grant" 1822 "Point Pleasant" "Ohio"
    , Person "Rutherford B. Hayes" 1822 "Delaware" "Ohio"
    , Person "Chester A. Arthur" 1829 "Fairfield" "Vermont"
    , Person "James A. Garfield" 1831 "Moreland Hills" "Ohio"
    , Person "Benjamin Harrison" 1833 "North Bend" "Ohio"
    , Person "Grover Cleveland" 1837 "Caldwell" "New Jersey"
    , Person "William McKinley" 1843 "Niles" "Ohio"
    , Person "Woodrow Wilson" 1856 "Staunton" "Virginia"
    , Person "William Howard Taft" 1857 "Cincinnati" "Ohio"
    , Person "Theodore Roosevelt" 1858 "New York City" "New York"
    , Person "Warren G. Harding" 1865 "Blooming Grove" "Ohio"
    , Person "Calvin Coolidge" 1872 "Plymouth" "Vermont"
    , Person "Herbert Hoover" 1874 "West Branch" "Iowa"
    , Person "Franklin D. Roosevelt" 1882 "Hyde Park" "New York"
    , Person "Harry S. Truman" 1884 "Lamar" "Missouri"
    , Person "Dwight D. Eisenhower" 1890 "Denison" "Texas"
    , Person "Lyndon B. Johnson" 1908 "Stonewall" "Texas"
    , Person "Ronald Reagan" 1911 "Tampico" "Illinois"
    , Person "Richard M. Nixon" 1913 "Yorba Linda" "California"
    , Person "Gerald R. Ford" 1913 "Omaha" "Nebraska"
    , Person "John F. Kennedy" 1917 "Brookline" "Massachusetts"
    , Person "George H. W. Bush" 1924 "Milton" "Massachusetts"
    , Person "Jimmy Carter" 1924 "Plains" "Georgia"
    , Person "George W. Bush" 1946 "New Haven" "Connecticut"
    , Person "Bill Clinton" 1946 "Hope" "Arkansas"
    , Person "Barack Obama" 1961 "Honolulu" "Hawaii"
    ]
