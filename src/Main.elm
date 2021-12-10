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
import Markdown
import Random
import Random.List exposing (shuffle)
import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)



--TODO: See if we can host on WebMate.me


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


logoImage =
    image [ width <| px 300 ]
        { src = "images/white-roundel_background.png"
        , description = "Gregarios Superclub Ciclista"
        }


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
            [ homeScreen model
            , notes
            ]


notes =
    column [ spacing 10, centerX, padding 20 ] <|
        List.map (paragraph [] << List.singleton)
            [ text "Most cycling clubs now feature “half-wheeling” as the Number One thing not to do on a group ride. "
            , text "The Gregarios were one of the very first clubs to feature a Ride Etiquette Page on a club website. "
            , text "Much of our Etiquette content has been adopted by other clubs up and down the country."
            , text "Our Paceline and Chaingang instructions have been copied by many clubs."
            ]


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


type alias Entry =
    { title : String
    , content : String

    --, picture : String
    }


content : List Entry
content =
    [ Entry
        "Riding in Pairs"
        """Where space allows we ride in two tidy lines. 
The front positions will change regularly so everyone gets a chance to ride on the front
and share the pace-making.

[This video](https://www.youtube.com/watch?v=-9upR9r2_uQ) has a great explanation of how we rotate positions.
        """
    , Entry
        "Don’t be all over the road"
        """If you’re in the outside line keep close to the rider alongside and don’t stray into the 
middle of the road (for obvious reasons!!).
Keeping a tight group or _gruppo compatto_ not only looks good,
it’s safer, and it saves you a big chunk of energy by merely riding in the slipstream ."""
    , Entry
        "Stay on the tracks"
        """When riding double hold your line and don’t veer or wobble about.
Just imagine you’re on railway tracks holding the same distance between the rider next to you."""
    , Entry
        "Try not to switch lanes or positions"
        """If the group stops, comes to a junction or roundabout, or singles out, always try and resume your position.
It’s all about keeping it predictable. This also enables everyone to have the same opportunity to
contribute a turn on the front with the pace-making, and no one is popping up anywhere unexpected."""
    , Entry
        "Don’t be a half-wheeler"
        """When riding at the front, try and make sure you ride level with rider next to you, and at the same pace.
Riding slightly in front of your opposite rider will push your partner to “catch up”,
increasing the speed of the whole group.
This is termed “half-wheeling” and it disrupts the rhythm of the group.
A great way to check whether you’re riding level is to ride brake hood to brake hood.
If you look to the side and there’s no-one there then you need to ease back!"""
    , Entry
        "Junctions"
        """Members of the group who have negotiated a junction should be aware that other members
may have had to wait and need time to rejoin so should proceed slowly and check back
before getting back on pace. Lead riders can indicate whether it is clear of traffic,
but everyone is responsible for their own safety and must take individual care."""
    , Entry
        "Out of the saddle"
        """As a hill gets steeper you may wish to get out of the saddle.
Make sure you don’t kickback as you get out of the saddle.
Best bet is to change up a gear or two (to give yourself something to push against)
and make sure your pedal is at a 2pm position so that you can stand on it,
then just keep the movements smooth. It’s worth practicing this on your own."""
    , Entry
        "Communicate potholes and hazards"
        """Communication, anticipation, and awareness are important for safe group rides.
The front riders should look well ahead and give a clear warning about potholes
and other hazards well before we pass them.
Front riders in the outside lane should also be looking out for potholes in front
of the inside rider - anticipate and react if the rider on your inside may need to swing out.
Approaching a pothole plan a shallow line round it that the riders behind can easily follow -
try not to flick round them at the last minute!
Everyone is responsible for relaying forward any warnings through the group,
rather than having to rely on muffled shouts from the back or front."""
    , Entry
        "Cars coming past"
        """Yes, we already know there are cars on the road! It’s expected!
We don’t need to be yelling just for the sake of it.
As long as everyone holds their line, there should be no need to yell every time a
car comes past unless for some reason they are endangering the group.
If there is a queue of cars behind, we should and usually do single out.
The terms “car front” and “car back” work well when shouting might be necessary."""
    , Entry
        "Pacing the ride"
        """We aim to ride at a pace that is comfortable for all riders in the group.
The front riders will be working a bit harder… as they’re on the front,
and they should ride on effort, not speed.

The effort level should be as constant as possible.
This means that the speed will be faster on the flat with appropriate gear changes,
and slower on the hills and rises again with appropriate use of your gears.
We’re not trying to maintain a constant speed, because the terrain will change and also the wind conditions.

The two front riders set and maintain the pace, but the rest of the group are
responsible for indicating if this pace exceeds the comfort threshold of any member.
If you notice that the rider next to you is struggling to keep up,
or a rider at the back has fallen behind the ride, a shout of “mile off” or
“half a mile off” will let the front riders know that they need to drop the pace slightly.

The skill of setting the pace is to ensure the group isn’t splitting every time
there’s a rise in the road, but we may need to regroup at the top of any significant hill -
especially if some riders are going for hilltop points.
This should not be happening on every little rise."""
    , Entry
        "Light on the brakes"
        """For a safe enjoyable ride it’s important to ride smoothly, and try and avoid hard
braking or swerving as much as possible. Try and slow by feathering the brakes
lightly or even just moving out into the wind to take off a little speed.

**Stay alert** at all times and keep “reading” the dynamics of the group. """
    , Entry
        "Hold your line"
        """This means swerving as little as possible.
Always ride in a straight line, and keep your pace as even as possible with shoulders and arms relaxed.
Don’t ride in the middle of the road unless it’s a small country lane.
If you’re on a small country lane it’s best to single out before going into a blind corner!"""
    , Entry
        "Don’t overlap wheels"
        """Overlapping, is putting your front wheel next to someone’s rear wheel.
Try to always be behind the bike in front unless you’re passing.
It’s a good idea to be very slightly offset to left or right,
then if the rider in front stops you will still have a safety margin and wont ride directly into them."""
    , Entry
        "Relax!"
        """Your bike will be more stable and easy to control when you’re relaxed.
Use a relaxed grip on the handlebars, keep your shoulders down (not up against your neck)
and bend your elbows slightly.
These steps will help you stay relaxed, which allows quicker reaction time and prevents
tension in the neck and shoulders that can lead to fatigue and sloppy riding."""
    , Entry
        "Focus ahead"
        """Look up towards the front of the group and beyond so you can see what’s going on
at the head of the group - and in front of the group.
It’s termed “soft focus” - you’re looking to the front of the group yet
you will still be aware of what’s happening directly in front of you.
It prepares you for any sudden changes."""
    , Entry
        "Anticipate"
        """Anticipate when the group might be slowing for a junction, bend in the road, traffic lights, etc.
Anticipate when you might need to change gear."""
    , Entry
        "Keep it smooth"
        """Pedal smoothly and evenly to save energy and make it easier for the riders behind you."""
    , Entry
        "Don’t look back"
        """(unless it’s really necessary) Focus ahead where you are going."""
    , Entry
        "In the wet"
        """Proper mudguards with long flaps will win you friends in the peloton!
Moderate the speed. Just knock it back and be safe.
Try to avoid going over white painted road markings while turning as they can be  slippery ...
especially white domed roundabouts!!!
The same applies to metalwork on the road ... OK in straight line but if possible try not to
be turning and leaning the bike on paintwork or metal covers.
Make sure your mudflap is long enough to protect the rider behind you."""
    , Entry
        "Horses"
        """When overtaking horses either solo or in a group the lead riders should shout a clear
audible warning like “coming through” while slowing down while and also giving them plenty of room.
Both rider and horse will appreciate this!"""
    , Entry
        "Downhill"
        """Descend on the drops if it’s a significant or proper hill.
This lowers your centre of gravity, gives you more control, improved pull on the brakes,
and it makes you faster!  At speed being on the drops will keep you in control of the
bike if you hit a pothole ... on the brake hoods any impact could cause you to lose your grip.
You will catch riders in front if you’re in their slipstream,
so just move out into the wind to take off a bit of speed.

**Downhill cornering check list**
- Weight on the outside foot.
- Head up… like a periscope while the bike leans.
- Look towards your exit point. Relax.

**Downhill Fast Straights**

Bumpy or uneven surface? Try this ... cranks horizontal, weight
increased on the pedals so you’re almost standing on them.
Experiment with these techniques."""
    , Entry
        "Awareness"
        """Always pay attention to what’s happening on the road ahead of the group,
and also the general group dynamics. Knock it back a bit through busy urban areas."""
    , Entry
        "Pre Ride"
        """Inspect tyres and remove any small flints that are embedded."""
    , Entry
        "At the start"
        """You’re rocking up to the start and see a rider or two that are not in club kit.
Probably it’s their first ride or they’re fairly new members.
It’s a Gregarios’ tradition to give them the same friendly welcome that was extended to you.
Please don’t pretend not to notice them, that’s just not cool."""
    , Entry
        "The three most important things to remember"
        """- be predictable
- be predictable!
- be predictable!!"""
    ]


homeScreen model =
    wrappedRow
        [ padding 20
        , spacing 20
        , alignLeft
        , alignTop
        ]
    <|
        List.map entryAsHeading model.entries


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


entryAsHeading : Entry -> Element Msg
entryAsHeading entry =
    tricolore 2 <|
        Input.button
            [ spacing 10
            , width (fill |> minimum 300 |> maximum 400)
            , padding 10
            ]
            { onPress = Just <| SelectEntry (Just entry)
            , label =
                paragraph
                    [ Font.size 20
                    , Font.bold
                    , Font.color FlatColors.BritishPalette.electromagnetic
                    ]
                    [ text entry.title ]
            }


withBorder colour width =
    el
        [ Border.color colour
        , Border.width width
        , Border.rounded (width * 4)
        , Background.color white
        ]


tricolore width =
    identity
        << withBorder FlatColors.BritishPalette.downloadProgress width
        << withBorder FlatColors.BritishPalette.lynxWhite width
        << withBorder FlatColors.BritishPalette.nasturcianFlower width


entryDetail : Entry -> Element Msg
entryDetail entry =
    -- Try to get tricolore border.
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
    in
    tricolore 5 <|
        column
            [ spacing 10, width (fill |> maximum 500), padding 10 ]
            [ row [ width fill ]
                [ paragraph
                    [ Font.size 24
                    , Font.color FlatColors.BritishPalette.electromagnetic
                    , Font.bold
                    , padding 10
                    ]
                    [ text entry.title ]
                , closeButton
                ]
            , paragraph
                [ Font.size 20
                , padding 10
                , Font.color FlatColors.BritishPalette.blueNights
                ]
                [ html <| Markdown.toHtml [] entry.content ]
            ]
