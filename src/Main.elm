module Main exposing (..)

import Browser exposing (application)
import Element exposing (Element, alignRight, alpha, centerX, centerY, clip, column, el, fill, height, image, layout, link, mouseOver, padding, paragraph, px, rgb255, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FlatColors.FlatUIPalette exposing (concrete, silver, wetAsphalt)
import Url exposing (Url)
import Url.Builder as Builder


type alias Model =
    { title : String
    }


type alias Flags =
    ()


type Msg
    = Clicked


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { title = "Stepwise Refinement Ltd" }, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


logoImage =
    el
        [ Font.size 64
        , Font.color concrete
        ]
    <|
        text "Stepwise Refinement Ltd"


view : Model -> Browser.Document Msg
view model =
    { title = "Stepwise Refinement Ltd"
    , body =
        [ layout
            [ Background.color wetAsphalt
            , width fill
            ]
          <|
            column [ padding 50, height fill, width fill ]
                [ logoImage
                , wrappedRow [ centerY, centerX, padding 100, spacing 100 ]
                    [ imageButton "Chain Home"
                        "/images/ChainHome.png"
                        (Builder.crossOrigin "https://storage.googleapis.com"
                            [ "chainhome", "index.html" ]
                            []
                        )
                    , imageButton "GPXmagic"
                        "/images/GPXmagic.png"
                        (Builder.crossOrigin "https://www.stepwiserefinement.co.uk"
                            [ "GPXmagic", "index.html" ]
                            []
                        )
                    ]
                ]
        ]
    }


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
        , el
            [ Font.size 20
            , Font.color silver
            , centerX
            ]
            (text description)
        ]
