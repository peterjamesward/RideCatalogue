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

                        --, spacing 6
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
        , Background.tiled "images/context-map.png"
        ]
    <|
        column
            [ width fill, spacing 20, padding 20 ]
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


benson : Entry
benson =
    { title = "Benson"
    , mapImage = "images/Benson-map.png"
    , profileImage = "images/Benson-profile.png"
    , lunchStop = "Waterfront Cafe"
    , distance = Length.kilometers 153
    , climbing = Length.meters 1592
    , gpx = "gpx/Benson.gpx"
    , narrative = """
Oft-trodden path, Benson's ride, a quintessential treat,
Weighty it is, nearly an imperial ton, far from petite,
Unless Hatch End be thy home, a proper ride it shall be,
Homecoming plans, leave them be, for it's a long journey to see.

Majestic landscapes greet, as we traverse the main road,
From Denham to Fulmer, westward through Bourne End, we rode,
Marlow's charm and the long ascent to Frieth, we will climb,
Christmas Common we'll cross, in measured pace, passing by RAF Benson in time.

On Stoke Row, a climb awaits, by the Maharajah's Well,
A vast plateau to behold, before we to the river dwell,
Henley we'll pass, through Marlow once again, Winter Hill, we'll embrace,
To Cookham we'll arrive, buzzing through Burnham Beeches, optional coffee or ice cream to taste.

We'll (nearly) retrace our steps, tired yet content,
To Hatch End, we'll return, our energy all but spent.
"""
    }


hertford : Entry
hertford =
    { title = "Hertford"
    , mapImage = "images/Hertford-map.png"
    , profileImage = "images/Hertford-profile.png"
    , lunchStop = "bebo Cafe"
    , distance = Length.kilometers 87.6
    , climbing = Length.meters 600
    , gpx = "gpx/Hertford_summer.gpx"
    , narrative = """
County town of the "County of Opportunities", Hertford has several pleasant hostelries, so it's long
been a popular destination. It's also a gateway for some longer Herts rides, but that's another story.

This summer route takes in more of the pleasant lanes around Brickendon, with many ups and downs through
wooded places, but none too severe. The Forza route has more, and more, of the same.

The return route takes in the longish climb of Essendon, but not before you've got your legs working
again with a nice long run along the river valley. The "old A1" is ideal for some "through and off" work
later in the day, especially if the wind's against us.
"""
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
25 miles is up.

The afternoon takes us over the M40 which we then stay fairly close to on
surprisingly quiet lanes before cruising  through Denham, up the Col de Harefield and back 
to Hatch End. How we managed to fit in so much in such a short ride, shall forever remain a mystery.
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
Take up the call to adventure, my friend, and hie thee to Windsor's regal scene! And whilst there,
sample a cinnamon bun at the café that bears the same name.

Our path leads us west, through Harefield and Maple Cross, meandering along the picturesque
lanes of Chalfonts and Seer Green. Then, to the south we venture, crossing the M40 to witness
the splendour of the Thames at Cookham. A few gentle inclines and scenic byways carry us down
to the M4, where we journey with ease down "Museeuw Lane", and end our travels amidst the
bustling throng of Windsor's folk.

The way homeward shall be brief, through Denham, where without cars, its quaintness would
truly come alive. And then we shall ascend the Col de Harefield, before returning, satisfied
and contented, to Hatch End. And there, a rejuvenating espresso awaits us, restoring our
vigour and spirits.
"""
    }


wilstone : Entry
wilstone =
    { title = "Wilstone"
    , mapImage = "images/wilstone-map.png"
    , profileImage = "images/wilstone-profile.png"
    , lunchStop = "Cinnamon Cafe"
    , distance = Length.kilometers 93.3
    , climbing = Length.meters 900
    , gpx = "gpx/wilstone.gpx"
    , narrative = """
OK, it's not very different to the Aldbury route, but it's different enough, ok?
Out through Oxhey and Moor Park, picking up riders along the way, Chenies, Chesham,
Wigginton, down to Tring and thence Wilstone and the excellent P E Mead Farm Shop Cafe thing.
The route back is slightly to the east, taking in Berkhampstead, Bovingdon and Bucks Hill.
Elsewhere, you'll find the optional "Chain Gang Loop" which makes good use of the flat
landscape to put in a useful bit of practice riding in formation. Feels good.
"""
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
(optionally) a beer at the renowned Minori Pizzeria. Pack an extra energy bar or two, as there's
no stopping until we stop.
"""
    }


harpenden : Entry
harpenden =
    { title = "Harpenden"
    , mapImage = "images/Harpenden-map.png"
    , profileImage = "images/Harpenden-profile.png"
    , lunchStop = "Bebo Cafe"
    , distance = Length.kilometers 89.7
    , climbing = Length.meters 618
    , gpx = "gpx/Harpenden.gpx"
    , narrative = """
Hertfordshire is mainly fairly gentle rolling hills, with an occasional kick up. This route is
basically that. Boring, it is not. As ever, we find the best lanes with peaceful dells and
sweeping vistas (well, it's not Wyoming).

Have a chill and chat at the Artisan coffe house that is Brew and Cru, knowing that the greater
part of the day's exertions are behind you, but it ain't over 'til it's over.
"""
    }


gtMissenden : Entry
gtMissenden =
    { title = "Great Missenden"
    , mapImage = "images/Gt-Missenden-map.png"
    , profileImage = "images/Gt-Missenden-profile.png"
    , lunchStop = "Matilda's Bistro"
    , distance = Length.kilometers 68.3
    , climbing = Length.meters 418
    , gpx = "gpx/Gt-Missenden.gpx"
    , narrative = """
Let's head out through Watford, from where we soon cross the M25 and pass through the lovely Sarrat
with its church, pubs, coronation oak trees, and the bus shelter where you may find the mid-week
pros deciding where to go. Bovingdon, where they film "The Wheel", apparently. Dive down into
Chesham then haul yourself out the other side before descending again to lunch in Great Missenden, home
to the Roald Dahl museum.

It's shorter coming back but more countryside and historic towns await the passing of the blue train.
Soon, you'll be back at Hatch End, with a comfy 42 miles notched up.
"""
    }


content : List Entry
content =
    [ aldbury
    , windsor
    , beaconsfield
    , gtMissenden
    , hertford
    , harpenden
    , nonstopPizzaRide
    , benson
    , wilstone
    ]


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
        [ padding 10
        , spacing 20
        , alignLeft
        , alignTop
        , width fill
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
            text <|
                asDistance Length.inMiles "mi" entry.distance

        km =
            text <|
                asDistance Length.inKilometers "km" entry.distance

        climbMetric =
            text <|
                asDistance Length.inMeters "m" entry.climbing

        climbImperial =
            text <|
                asDistance Length.inFeet "ft" entry.climbing

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

                --, padding 5
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
                , centerY
                , Font.center
                , Font.bold
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
            row [ width fill ]
                [ leftInfo, rightInfo ]
    in
    Input.button
        [ Border.rounded 5
        , inFront overlay
        , inFront title
        ]
        { onPress = Just <| SelectEntry (Just entry)
        , label =
            el
                [ clip
                , Border.rounded 10
                ]
                map
        }
