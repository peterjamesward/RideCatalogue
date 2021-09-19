module Main exposing (..)

import Browser exposing (application)
import Browser.Navigation as Nav
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Evolving exposing (evolving)
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
    link
        [ Font.size 64
        , Font.color concrete
        ]
        { url = Builder.relative [ "index.html" ] []
        , label = text "Stepwise Refinement Ltd"
        }


view : Model -> Browser.Document Msg
view model =
    { title = "Stepwise Refinement Ltd"
    , body =
        [ layout [ Background.color wetAsphalt, E.width fill ]
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
        ]
        [ logoImage
        , case toRoute model.url of
            Home ->
                contentSection model

            About ->
                viewMarkdown stepwiseRefinement

            Evolve ->
                viewMarkdown evolving

            NotFound ->
                contentSection model
        , sponsorMessage2
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


contentSection model =
    wrappedRow [ alignTop, centerX, padding 50, spacing 60 ]
        [ textButton "Stepwise Refinement"
            (Builder.relative [ "about" ] [])
        , imageButton "Chain Home"
            (Builder.relative [ "images", "ChainHome.png" ] [])
            (Builder.crossOrigin "http://storage.googleapis.com"
                [ "chainhome", "index.html" ]
                []
            )
            False
        , imageButton "GPXmagic"
            (Builder.relative [ "images", "GPXmagic.png" ] [])
            (Builder.crossOrigin "http://www.stepwiserefinement.co.uk"
                [ "GPXmagic", "index.html" ]
                []
            )
            False
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


commonStyles =
    [ padding 3
    , E.width <| px 212
    , E.height <| px 212
    , mouseOver [ alpha 0.7 ]
    , Border.color FlatColors.FlatUIPalette.asbestos
    , Border.width 5
    , Border.rounded 8
    ]


imageButton : String -> String -> String -> Bool -> Element Msg
imageButton description imageUrl linkUrl newWindow =
    let
        linkFn =
            if newWindow then
                E.newTabLink

            else
                E.link
    in
    column [ spacing 10, alignTop ]
        [ Input.button
            commonStyles
            { onPress = Nothing
            , label =
                -- The label can be any element, so for example, the button
                -- can contain an image
                el [ clip, Border.rounded 6 ] <|
                    linkFn []
                        { url = linkUrl
                        , label =
                            image
                                [ E.width <| px 200
                                , E.height <| px 200
                                , mouseOver [ alpha 0.7 ]
                                ]
                                { src = imageUrl
                                , description = description
                                }
                        }
            }
        , paragraph
            [ Font.size 20
            , Font.color silver
            , centerX
            , E.width (px 200)
            ]
            [ text description ]
        ]


textButton : String -> String -> Element Msg
textButton description linkUrl =
    column [ spacing 10, alignTop ]
        [ link
            commonStyles
            { url = linkUrl
            , label =
                paragraph
                    [ Font.color sunFlower
                    , Font.size 32
                    , padding 5
                    ]
                    [ el [ centerX, alignBottom ] <| text description ]
            }
        , paragraph
            [ Font.size 20
            , Font.color silver
            , centerX
            , E.width (px 200)
            ]
            [ text description ]
        ]
