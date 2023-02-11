module Main exposing (..)

import Browser exposing (application)
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
    }


type alias Flags =
    ()


type Msg
    = SelectEntry (Maybe Entry)
    | Randomized (List Entry)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { active = Nothing
      , entries = content
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


view : Model -> Html Msg
view model =
    layout
        [ E.width fill
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
    }


content : List Entry
content =
    [ aldbury ]


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
    Sub.none


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

        map =
            image [ alignTop, alignRight, width <| px 500 ]
                { src = entry.mapImage
                , description = entry.title
                }

        profile =
            image [ alignTop, alignRight, width <| px 500, height <| px 100 ]
                { src = entry.profileImage
                , description = "profile"
                }

        gpxButton =
            Input.button
                [ spacing 10
                , Border.rounded 20
                , height <| px 50
                , width <| px 50
                , padding 10
                , alignBottom
                , alignRight
                , Background.color FlatColors.FlatUIPalette.emerald
                , Font.color FlatColors.FlatUIPalette.clouds
                ]
                { onPress = Just <| SelectEntry (Just entry)
                , label =
                    html <|
                        FeatherIcons.toHtml [] <|
                            FeatherIcons.withSize 36 <|
                                FeatherIcons.download
                }
    in
    column
        [ width <| px 750

        --, height <| px 600
        , Border.color FlatColors.FlatUIPalette.clouds
        , Border.width 10
        , Border.rounded 20
        , inFront closeButton
        , Background.color FlatColors.BritishPalette.lynxWhite
        , spacing 0
        ]
        [ map, profile, gpxButton ]
