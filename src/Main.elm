module Main exposing (..)

import Browser exposing (application)
import Browser.Navigation as Nav
import Element exposing (Element, alignRight, alpha, centerX, centerY, clip, column, el, fill, height, image, layout, link, mouseOver, padding, paragraph, px, rgb255, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FlatColors.FlatUIPalette exposing (amethyst, concrete, silver, sunFlower, wetAsphalt)
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
    --TODO: Make this the Home link.
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
        [ layout [ Background.color wetAsphalt, width fill ]
            (homeScreen model)
        ]
    }


type Route
    = Home
    | About
    | NotFound


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        , map Home (s "home")
        , map About (s "about")
        ]


toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound (parse route url)


homeScreen : Model -> Element Msg
homeScreen model =
    column [ padding 50, height fill, width fill ]
        [ logoImage
        , case toRoute model.url of
            Home ->
                contentSection model

            About ->
                viewMarkdown

            NotFound ->
                contentSection model
        , text <| "The current URL is: " ++ Url.toString model.url
        ]


contentSection model =
    wrappedRow [ centerY, centerX, padding 100, spacing 100 ]
        [ textButton "Stepwise Refinement"
            (Builder.relative [ "about" ] [])
        , imageButton "Chain Home"
            (Builder.relative [ "images", "ChainHome.png" ] [])
            (Builder.crossOrigin "http://storage.googleapis.com"
                [ "chainhome", "index.html" ]
                []
            )
        , imageButton "GPXmagic"
            (Builder.relative [ "images", "GPXmagic.png" ] [])
            (Builder.crossOrigin "http://www.stepwiserefinement.co.uk"
                [ "GPXmagic", "index.html" ]
                []
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


imageButton : String -> String -> String -> Element Msg
imageButton description imageUrl linkUrl =
    column [ spacing 10 ]
        [ Input.button
            [ padding 3
            , Border.rounded 9
            , Border.width 3
            , Border.color silver
            ]
            { onPress = Nothing
            , label =
                -- The label can be any element, so for example, the button
                -- can contain an image
                el [ clip, Border.rounded 6 ] <|
                    link []
                        { url = linkUrl
                        , label =
                            image
                                [ width <| px 200
                                , height <| px 200
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
            , width (px 200)
            ]
            [ text description ]
        ]


textButton : String -> String -> Element Msg
textButton description linkUrl =
    column [ spacing 10 ]
        [ link
            [ Border.rounded 9
            , Border.width 3
            , Border.color silver
            , Background.color amethyst
            , width <| px 212
            , height <| px 212
            , mouseOver [ alpha 0.7 ]
            ]
            { url = linkUrl
            , label =
                paragraph [ Font.color sunFlower, Font.size 32, padding 5 ]
                    [ el [ centerX ] <| text description ]
            }
        , paragraph
            [ Font.size 20
            , Font.color silver
            , centerX
            , width (px 200)
            ]
            [ text description ]
        ]
