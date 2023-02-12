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
import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)


type alias Model =
    { active : Maybe Entry
    , entries : List Entry
    , width : Int
    , height : Int
    , device : E.Device
    }


type alias Flags =
    ()


type Msg
    = SelectEntry (Maybe Entry)
    | Randomized (List Entry)
    | GotNewSize Int Int


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
                        [ html <| Markdown.toHtml [] entry.narrative ]
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
            [ homeScreen model ]


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


aldbury : Entry
aldbury =
    { title = "Aldbury"
    , mapImage = "images/aldbury-map.png"
    , profileImage = "images/aldbury-profile.png"
    , lunchStop = "The Musette"
    , distance = Length.miles 54
    , climbing = Length.meters 600
    , gpx = "gpx/aldbury.gpx"
    , narrative = """# Aldbury

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


windsor : Entry
windsor =
    { title = "Windsor"
    , mapImage = "images/windsor-map.png"
    , profileImage = "images/windsor-profile.png"
    , lunchStop = "Cinnamon Cafe"
    , distance = Length.kilometers 97.8
    , climbing = Length.meters 509
    , gpx = "gpx/windsor.gpx"
    , narrative = """# Windsor

Yield to your inner tourist. Visit Windsor, see the King. Eat a cinnamon bun at the eponymous cafe.

Heading westwards through Harefield and Maple Cross, we enjoy the lanes around the Chalfonts
and Seer Green before turning south through Beaconsfield and over the M40, crossing the Thames
in all its beauty at Cookham. A few small, scenic bumps take us down to the M4 and then a lovely
run along "Museeuw Lane" before we mingle with the muggles in Windsor.

It's a fairly short ride home through Denham, which would be lovely with no cars, and the ascent
of the Col de Harefield before finding ourselves back at Hatch End with time for a post-ride
espresso."""
    }


content : List Entry
content =
    [ aldbury, windsor ]


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
