module Main exposing (..)

import Browser exposing (application)
import Browser.Events
import Browser.Navigation as Nav
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.BritishPalette
import FlatColors.ChinesePalette exposing (white)
import FlatColors.FlatUIPalette exposing (..)
import Html exposing (Html)
import Length
import Markdown
import Random
import Random.List exposing (shuffle)
import String.Interpolate
import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)


type alias Model =
    { active : Maybe Entry
    , entries : List Entry
    , width : Int
    , height : Int
    , device : E.Device
    , sortBy : Sortation
    }


type Sortation
    = Randomised
    | ByDistance
    | ByClimbing
    | ByName


type alias Flags =
    ()


type Msg
    = SelectEntry (Maybe Entry)
    | Randomized (List Entry)
    | GotNewSize Int Int
    | SetSortation Sortation


defaultDevice =
    E.Device Desktop Landscape



--TODO: USE classifyDevice : { window | height : Int, width : Int } -> Device


init : ( Int, Int ) -> ( Model, Cmd Msg )
init ( width, height ) =
    ( { active = Nothing
      , entries = content
      , width = width
      , height = height
      , device = E.classifyDevice { height = height, width = width }
      , sortBy = Randomised
      }
    , Random.generate Randomized <| shuffle content
    )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectEntry entry ->
            ( { model | active = entry }
            , Cmd.none
            )

        Randomized entries ->
            ( { model | entries = entries }
            , Cmd.none
            )

        GotNewSize width height ->
            ( { model
                | width = width
                , height = height
                , device = E.classifyDevice { height = height, width = width }
              }
            , Cmd.none
            )

        SetSortation sortation ->
            let
                newModel =
                    { model
                        | sortBy = sortation
                        , entries =
                            case sortation of
                                Randomised ->
                                    model.entries

                                ByName ->
                                    List.sortBy .title model.entries

                                ByDistance ->
                                    List.sortBy (.distance >> Length.inKilometers) model.entries

                                ByClimbing ->
                                    List.sortBy (.climbing >> Length.inMeters) model.entries
                    }
            in
            ( newModel
            , if sortation == Randomised then
                Random.generate Randomized <| shuffle content

              else
                Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        entryDetail : Entry -> Element Msg
        entryDetail entry =
            let
                closeButton =
                    Input.button
                        [ Font.color FlatColors.BritishPalette.chainGangGrey
                        , alignRight
                        ]
                        { onPress = Just <| SelectEntry Nothing
                        , label =
                            html <|
                                FeatherIcons.toHtml [] <|
                                    FeatherIcons.withSize 36 <|
                                        FeatherIcons.x
                        }

                mapNarrow =
                    image
                        [ alignTop
                        , alignRight
                        , width <| px <| model.width - 40
                        ]
                        { src = entry.mapImage
                        , description = entry.title
                        }

                mapWide =
                    image
                        [ alignTop
                        , alignRight
                        , width <| px <| model.width // 4
                        ]
                        { src = entry.mapImage
                        , description = entry.title
                        }

                profileNarrow =
                    image
                        [ alignTop
                        , alignRight
                        , width <| px <| model.width - 40
                        ]
                        { src = entry.profileImage
                        , description = "profile"
                        }

                profileWide =
                    image
                        [ alignTop
                        , alignRight
                        , width <| px <| model.width // 4
                        ]
                        { src = entry.profileImage
                        , description = "profile"
                        }

                gpxButton =
                    downloadAs
                        [ Border.rounded 20
                        , Border.color FlatColors.FlatUIPalette.wisteria
                        , Border.width 2
                        , padding 5
                        , alignBottom
                        , centerX
                        , Background.color FlatColors.FlatUIPalette.emerald
                        , Font.color FlatColors.FlatUIPalette.wisteria
                        , Font.size 16
                        ]
                        { url = entry.gpx
                        , label = text "Download GPX file"
                        , filename = entry.gpx
                        }

                narrative =
                    paragraph
                        [ alignTop
                        , alignLeft
                        , paddingEach { left = 20, right = 20, top = 20, bottom = 0 }
                        , Font.size 16
                        ]
                    <|
                        [ html <| Markdown.toHtml [] (withPreamble entry) ]
            in
            if model.device.class == E.Tablet || model.device.class == E.Phone then
                -- Simple linear flow
                column
                    [ Border.color FlatColors.FlatUIPalette.clouds
                    , Border.width 10
                    , Border.rounded 20
                    , inFront closeButton
                    , Background.color FlatColors.BritishPalette.lynxWhite
                    , spacing 0
                    ]
                    [ narrative
                    , mapNarrow
                    , profileNarrow
                    , gpxButton
                    ]

            else
                -- Try to lay it out nicely with some flow for the text.
                row
                    [ Border.color FlatColors.FlatUIPalette.clouds
                    , Border.width 10
                    , Border.rounded 20
                    , inFront closeButton
                    , Background.color FlatColors.BritishPalette.lynxWhite
                    , spacing 0
                    , width <| px <| model.width // 2
                    ]
                    [ el [ width <| fillPortion 1 ] narrative
                    , column [ width <| fillPortion 1 ]
                        [ el [ alignRight, alignTop ] mapWide
                        , el [ alignRight, alignTop ] profileWide
                        , gpxButton
                        ]
                    ]

        sortOptions =
            Input.radioRow
                [ Border.rounded 6, centerX ]
                { onChange = SetSortation
                , selected = Just model.sortBy
                , label = Input.labelHidden "sort"
                , options =
                    [ Input.optionWith ByName <| button First "A-Z"
                    , Input.optionWith ByDistance <| button Mid "Distance"
                    , Input.optionWith ByClimbing <| button Mid "Climbing"
                    , Input.optionWith Randomised <| button Last "Randomize"
                    ]
                }
    in
    layout
        [ E.width <| px model.width
        , inFront <|
            case model.active of
                Just entry ->
                    el [ centerX, centerY, scrollbarY ] <|
                        entryDetail entry

                Nothing ->
                    none
        ]
    <|
        column [ width fill, spacing 20, padding 20 ]
            [ sortOptions
            , homeScreen model
            ]


type ButtonPosition
    = First
    | Mid
    | Last


button position label state =
    let
        borders =
            case position of
                First ->
                    { left = 2, right = 2, top = 2, bottom = 2 }

                Mid ->
                    { left = 0, right = 2, top = 2, bottom = 2 }

                Last ->
                    { left = 0, right = 2, top = 2, bottom = 2 }

        corners =
            case position of
                First ->
                    { topLeft = 6, bottomLeft = 6, topRight = 0, bottomRight = 0 }

                Mid ->
                    { topLeft = 0, bottomLeft = 0, topRight = 0, bottomRight = 0 }

                Last ->
                    { topLeft = 0, bottomLeft = 0, topRight = 6, bottomRight = 6 }
    in
    el
        [ paddingEach { left = 20, right = 20, top = 10, bottom = 10 }
        , Border.roundEach corners
        , Border.widthEach borders
        , Border.color FlatColors.FlatUIPalette.wisteria
        , Background.color <|
            if state == Input.Selected then
                FlatColors.FlatUIPalette.belizeHole

            else
                FlatColors.FlatUIPalette.clouds
        ]
    <|
        el [ centerX, centerY ] <|
            text label


type Route
    = Home
    | About
    | Evolve
    | NotFound


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.Parser.map Home top
        , Url.Parser.map Home (s "home")
        , Url.Parser.map About (s "about")
        , Url.Parser.map Evolve (s "evolve")
        ]


toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound (parse route url)


type alias Entry =
    { title : String
    , mapImage : String
    , profileImage : String
    , lunchStop : String
    , distance : Length.Length
    , climbing : Length.Length
    , gpx : String
    , narrative : String
    }


metrics =
    """

```
Distance {0}km, {1} miles.
Climbing {2}m, {3} feet.
```

"""


aldbury : Entry
aldbury =
    { title = "Aldbury"
    , mapImage = "images/aldbury-map.png"
    , profileImage = "images/aldbury-profile.png"
    , lunchStop = "The Musette"
    , distance = Length.miles 54
    , climbing = Length.meters 600
    , gpx = "gpx/aldbury.gpx"
    , narrative = """
One of the most popular Chiltern runs, not least because of the splendid location of The Musette,
and its menu. There are many ways there and many ways back, but this route is neither too direct
nor too convoluted, but uses some rather lovely and varied roads with enough up-and-down to keep
things interesting without being there just for the sake of it.

After the preliminaries of going
through Rickmansworth, we wend our way to Sarratt, Flaunden, and Hogpits Bottom. Then it's flat
past Bovingdon Green and Whelpley Hill before we duck and dive out to the western edge of
Berkhampstead, only to come back for the long ascent up to Wigginton and down to our stop.

The afternoon takes us up the slopes to Champneys and Hadden's Plantation towards Chesham,
only to veer away through Lye Green, south to Latimer and down to the Chess Valley before the
Chenies climb, then a main road blast back to Rickmansworth and then home again."""
    }


beaconsfield : Entry
beaconsfield =
    { title = "Beaconsfield"
    , mapImage = "images/BEACONSFIELD-map.png"
    , profileImage = "images/BEACONSFIELD-profile.png"
    , lunchStop = "Costa Coffee"
    , distance = Length.kilometers 70
    , climbing = Length.meters 744
    , gpx = "gpx/BEACONSFIELD.gpx"
    , narrative = """
This is a great step-up-from-Saturday ride, at 44 miles! We pootle out through Ricky and the 
Chalfonts, up Winchmore Hill to Penn Street and then we're practically at our lunch stop before 
25 miles is up. The afternoon takes us over the M40 which we then stay fairly close to on 
surprisingly quiet lanes before cruising  through Denham, up the Col de Harefield and back 
to Hatch End.
"""
    }


windsor : Entry
windsor =
    { title = "Windsor"
    , mapImage = "images/windsor-map.png"
    , profileImage = "images/windsor-profile.png"
    , lunchStop = "Cinnamon Cafe"
    , distance = Length.kilometers 97.8
    , climbing = Length.meters 509
    , gpx = "gpx/windsor.gpx"
    , narrative = """
Yield to your inner tourist. Visit Windsor, see the King. Eat a cinnamon bun at the eponymous cafe.

Heading westwards through Harefield and Maple Cross, we enjoy the lanes around the Chalfonts
and Seer Green before turning south through Beaconsfield and over the M40, crossing the Thames
in all its beauty at Cookham. A few small, scenic bumps take us down to the M4 and then a lovely
run along "Museeuw Lane" before we mingle with the muggles in Windsor.

It's a fairly short ride home through Denham, which would be lovely with no cars, and the ascent
of the Col de Harefield before finding ourselves back at Hatch End with time for a post-ride
espresso."""
    }


nonstopPizzaRide : Entry
nonstopPizzaRide =
    { title = "Non-stop Pizza ride"
    , mapImage = "images/pizza-map.png"
    , profileImage = "images/pizza-profile.png"
    , lunchStop = "Minori Pizzeria"
    , distance = Length.kilometers 68.5
    , climbing = Length.meters 410
    , gpx = "gpx/pizza.gpx"
    , narrative = """
You want to get home to watch the soccer, mow the lawn, cook the dinner? This is the perfect ride
for you. It's also the perfect ride if you want to relax post-ride and have a chat over pizza and
(optionally) a beer at the reknowned Minori Pizzeria. Pack an extra energy bar or two, as there's
no stopping until we stop.
"""
    }


content : List Entry
content =
    [ aldbury, windsor, beaconsfield, nonstopPizzaRide ]


withPreamble : Entry -> String
withPreamble entry =
    "# "
        ++ entry.title
        ++ " \n "
        ++ String.Interpolate.interpolate metrics
            [ String.fromInt <| truncate <| Length.inKilometers entry.distance
            , String.fromInt <| truncate <| Length.inMiles entry.distance
            , String.fromInt <| truncate <| Length.inMeters entry.climbing
            , String.fromInt <| truncate <| Length.inFeet entry.climbing
            ]
        ++ entry.narrative


homeScreen model =
    wrappedRow
        [ padding 20
        , spacing 20
        , alignLeft
        , alignTop
        ]
    <|
        List.map entryAsSmallCard model.entries


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> GotNewSize w h)


entryAsSmallCard : Entry -> Element Msg
entryAsSmallCard entry =
    let
        asDistance units label distance =
            (String.fromInt <| truncate <| units distance) ++ " " ++ label

        mileage =
            text <| asDistance Length.inMiles "mi" entry.distance

        km =
            text <| asDistance Length.inKilometers "km" entry.distance

        climbMetric =
            text <| asDistance Length.inMeters "m" entry.climbing

        climbImperial =
            text <| asDistance Length.inFeet "ft" entry.climbing

        leftInfo =
            column
                [ alignLeft
                , alignTop
                , Border.color FlatColors.FlatUIPalette.concrete
                , Font.color FlatColors.FlatUIPalette.wisteria
                , padding 5
                , spacing 10
                , Font.size 16
                ]
                [ mileage
                , climbImperial
                ]

        rightInfo =
            column
                [ alignRight
                , alignTop
                , Border.color FlatColors.FlatUIPalette.concrete
                , Font.color FlatColors.FlatUIPalette.wisteria
                , padding 5
                , spacing 10
                , Font.size 16
                ]
                [ km
                , climbMetric
                ]

        title =
            el
                [ alignBottom
                , centerX
                , Font.center
                , Font.glow FlatColors.FlatUIPalette.clouds 10
                , Font.color FlatColors.FlatUIPalette.wisteria
                ]
            <|
                text entry.title

        map =
            image [ alignTop, centerX, width <| px 250, height <| px 200 ]
                { src = entry.mapImage
                , description = entry.title
                }

        overlay =
            row [ width fill, height fill ]
                [ leftInfo, title, rightInfo ]
    in
    Input.button
        [ spacing 10
        , Border.rounded 5
        ]
        { onPress = Just <| SelectEntry (Just entry)
        , label =
            el
                [ clip
                , Border.rounded 10
                , inFront overlay
                ]
                map
        }
