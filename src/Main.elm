module Main exposing (..)

import Browser exposing (application)
import Browser.Navigation as Nav
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Evolving exposing (evolving)
import FlatColors.BritishPalette
import FlatColors.ChinesePalette exposing (white)
import FlatColors.FlatUIPalette exposing (..)
import Msg exposing (Msg(..))
import Refinement exposing (stepwiseRefinement, viewMarkdown)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    }


type alias Flags =
    ()


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )


logoImage =
    image [ width <| px 300 ]
        { src = "images/white-roundel_background.png"
        , description = "Gregarios Superclub Ciclista"
        }


view : Model -> Browser.Document Msg
view model =
    { title = "Ride etiquette (Gregarios Superclub Ciclista)"
    , body =
        [ layout
            [ Background.color wetAsphalt
            , E.width fill
            ]
            (homeScreen model)
        ]
    }


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


sponsorMessage2 =
    el [ Font.size 24, Font.color orange, centerX, padding 50 ] <|
        text "peter at stepwiserefinement dot co dot uk"


homeScreen : Model -> Element Msg
homeScreen model =
    column
        [ paddingEach { left = 40, right = 40, top = 0, bottom = 40 }
        , E.width fill
        , spaceEvenly
        ]
        [ logoImage
        , contentSection model
        , buyMeACoffeeButton
        ]


buyMeACoffeeButton =
    newTabLink
        [ centerX ]
        { url = "https://www.buymeacoffee.com/Peterward"
        , label =
            image [ height (px 60), width (px 217) ]
                { src = "https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png"
                , description = "Buy Me A Coffee"
                }
        }



-- TODO Make content a data structure
-- TODO Base page displays grid of headings only
-- TODO Click on heading to display the text in overlay
-- TODO Click on overlay or other heading to hide or change.


contentSection model =
    wrappedRow [ alignTop, centerX, width fill, padding 20, spacing 20 ]
        [ oneEntry
            "Riding in Pairs"
            """Where space (and the Highway Code) allows we ride in two tidy lines.
               The front positions will change regularly so everyone gets a chance to ride
                on the front and share the pacemaking."""
            (Builder.relative [ "images", "pairs.png" ] [])
        , oneEntry
            "Stay close"
            """If you’re in the outside line keep close to the rider alongside and don’t stray into the
               middle of the road (for obvious reasons!!).
               Keeping “gruppo compatto” not only looks good, it’s safer,
               and saves energy by merely riding in the slipstream"""
            (Builder.relative [ "images", "pairs.png" ] [])
        , oneEntry
            "Stay on the tracks"
            """When riding double hold your line and don’t veer or wobble about.
            Just imagine you’re on railway tracks holding the same distance
            between the rider next to you."""
            (Builder.relative [ "images", "pairs.png" ] [])
        , oneEntry
            "Try not to switch lanes or positions"
            """If the group stops, comes to a junction or roundabout, or singles-out, 
            always try and resume your position. It’s all about keeping it predictable. 
            This also enables everyone to have the same opportunity to contribute a 
            turn on the front with the pacemaking, and no one is popping up anywhere unexpected."""
            (Builder.relative [ "images", "pairs.png" ] [])
        , oneEntry
            "Dummy"
            """Lorem ipsum"""
            (Builder.relative [ "images", "pairs.png" ] [])
        , oneEntry
            "Dummy"
            """Lorem ipsum"""
            (Builder.relative [ "images", "pairs.png" ] [])
        , oneEntry
            "Dummy"
            """Lorem ipsum"""
            (Builder.relative [ "images", "pairs.png" ] [])
        , oneEntry
            "Dummy"
            """Lorem ipsum"""
            (Builder.relative [ "images", "pairs.png" ] [])
        , oneEntry
            "Dummy"
            """Lorem ipsum"""
            (Builder.relative [ "images", "pairs.png" ] [])
        , oneEntry
            "Dummy"
            """Lorem ipsum"""
            (Builder.relative [ "images", "pairs.png" ] [])
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


commonStyles =
    [ padding 3
    , E.width <| px 212
    , E.height <| px 212
    , Border.color FlatColors.FlatUIPalette.asbestos
    , Border.width 5
    , Border.rounded 8
    ]


oneEntry : String -> String -> String -> Element Msg
oneEntry heading description imageUrl =
    column
        [ spacing 10
        , alignTop
        , width (fill |> minimum 400 |> maximum 500)
        , Border.color FlatColors.BritishPalette.skirretGreen
        , Border.width 5
        , Border.rounded 10
        , padding 10
        ]
        [ paragraph [ Font.size 32, Font.color white ] [ text heading ]
        , image
            [ E.width <| px 200
            , centerX
            ]
            { src = imageUrl
            , description = description
            }
        , paragraph
            [ Font.size 24
            , Font.color silver
            , centerX
            , E.width fill
            ]
            [ text description ]
        ]
