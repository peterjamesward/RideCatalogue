module Main exposing (..)

import Browser exposing (application)
import Browser.Navigation as Nav
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FlatColors.BritishPalette
import FlatColors.ChinesePalette exposing (white)
import FlatColors.FlatUIPalette exposing (..)
import Html exposing (Html)
import Markdown
import Random
import Random.List exposing (shuffle)
import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)



--TODO: Change to use simpler Elm model.
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
        [ Background.color wetAsphalt
        , E.width fill
        , inFront <|
            case model.active of
                Just entry ->
                    entryDetail entry

                Nothing ->
                    none
        ]
        (homeScreen model)


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
        , E.height fill
        , spaceEvenly
        ]
        [ row [ width fill, padding 10, spacing 10 ]
            [ logoImage
            , el [ Font.size 32, Font.color white, centerX, padding 50 ] <|
                text "The Gregarios' guide to ride etiquette"
            ]
        , contentSection model.entries
        , el [ alignBottom, centerX ] buyMeACoffeeButton
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


type alias Entry =
    { title : String
    , content : String
    , picture : String
    }


content : List Entry
content =
    [ Entry
        "Riding in Pairs"
        """Where space allows we ride in two tidy lines.
        The front positions will change regularly so everyone gets a chance to ride on the front and
        share the pacemaking. The video below has a great explanation of how we rotate positions."""
        "pairs.png"
    , Entry
        "Don’t be all over the road"
        """If you’re in the outside line keep close to the rider alongside and don’t stray
        into the middle of the road (for obvious reasons!!).
        Keeping _gruppo compatto_ not only looks good,
        it’s safer, and saves energy by merely riding in the slipstream"""
        "pairs.png"
    , Entry
        "Stay on the tracks"
        """When riding double hold your line and don’t veer or wobble about. Just imagine you’re 
        on railway tracks holding the same distance between the rider next to you."""
        "pairs.png"
    , Entry
        "Keep your place"
        """If the group stops, comes to a junction or roundabout, or singles-out, always 
        try and resume your position. It’s all about keeping it predictable. This also
        enables everyone to have the same opportunity to contribute a turn on the front
        with the pacemaking, and no one is popping up anywhere unexpected."""
        ""
    , Entry
        "Don’t be a half-wheeler"
        """When riding at the front, try and make sure you ride level with rider next to you, 
        and at the same pace.  Riding slightly in front of your opposite rider will push
        your partner to “catch up”, increasing the speed of the whole group. This is
        termed “half wheeling” and it disrupts the rhythm of the group.
        A great way to check whether you’re riding level is to ride brake hood to
        brake hood… also if you look to the side and there’s no-one there then you need to ease back!"""
        ""
    , Entry
        "Go easy after junctions"
        """Members of the group who have negotiated the junction should be aware that other 
        members may have had to wait and need time to rejoin so should proceed slowly
        and check back before getting back on pace."""
        ""
    , Entry
        "Out of the saddle"
        """As a hill gets steeper you may wish to get out of the saddle. Make sure you don’t 
        kickback as you get out of the saddle. Best bet is to change up a gear or two
        (to give yourself something to push against) and make sure your pedal is at a 2pm
        position so that you can stand on it, then just keep the movements smooth.
        It’s worth practicing this on your own."""
        ""
    , Entry
        "Communicate potholes and hazards"
        """Communication, anticipation, and awareness are important for safe group rides.
        The front riders should look well ahead and give a clear warning about potholes
        and other hazards well before we pass them.
        Front riders in the outside lane should also be looking out for potholes in
        front of the inside rider - anticipate and react if the inside rider needs to
        swing out. Approaching a pothole plan a shallow line round it that the riders
        behind can easily follow - zero points for flicking round them at the last minute!
        Everyone is responsible for relaying forward any warnings through the group -
        rather than having to rely on muffled shouts from the back or front."""
        ""
    , Entry
        "Cars coming past"
        """Yes we already know there are cars on the road! It’s expected! We don’t need
        to be yelling just for the sake of it. As long as everyone holds their line,
        there should be no need to yell every time a car comes past unless they are endangering the group.
        If there is a queue of cars behind, we should and usually do single out.
        The terms “car front” and “car back” work well on those rare occasions when shouting might be necessary."""
        ""
    , Entry
        "Pacing the ride"
        """The group aims to ride at a pace that is comfortable for all riders in the group.
        Ride on effort, not speed. The effort level should be as constant as possible.
        This means that the speed will be higher on the flat with appropriate gear changes,
        and lower on the hills and rises again with appropriate use of your gears.
        We’re not trying to maintain a constant speed - because the terrain will change and also the wind conditions.
        The two front riders set and maintain the pace, but the rest of the group are
        responsible for indicating if this pace exceeds the comfort threshold of any member.
        If you notice that the rider next to you is struggling to keep up, or a rider
        at the back has fallen behind the ride, a shout of “mile off” or “half a mile off”
        will let the front riders know that they need to drop the pace slightly.
        Either way the group may need to regroup at the top of a significant hill,
        but should not be splitting every time there’s a rise in the road."""
        ""
    , Entry
        "Be smoooth"
        """For a safe enjoyable ride it’s important to ride smoothly, and try and avoid
        hard braking or swerving as much as possible. Try and slow by feathering the
        brakes lightly or even just moving out into the wind to take off a little speed.
        Stay alert at all times and keep “reading” the dynamics of the group."""
        ""
    , Entry
        "Hold your line"
        """Avoid swerving.  Always ride in a straight line,
        and keep your pace as even as possible with shoulders and arms relaxed."""
        ""
    , Entry
        "Don’t overlap wheels"
        """Overlapping is putting your front wheel next to someone’s rear wheel.
        Try to always be behind the bike in front unless you’re passing.
        Be very slightly offset to left or right, then if the rider in front stops
        you will still have a safety margin and wont ride directly into them."""
        ""
    , Entry
        "Relax!"
        """Your bike will be much more stable and easy to control when you’re relaxed.
        Use a relaxed grip on the handlebars, keep your shoulders down (not up
        against your neck) and bend your elbows slightly. These steps will help
        you stay relaxed, which allows quicker reaction time and prevents tension
        in the neck and shoulders that can lead to fatigue and sloppy riding."""
        ""
    , Entry
        "Focus ahead"
        """Don’t make the common mistake of focusing on the back wheel in front of you.
        Look up towards the front of the group and beyond so you can see what’s
        going on at the head of the group - and in front of the group.
        It’s sometimes called “soft focus” where you’re looking to the front of
        the group yet you will still automatically be aware of what’s happening
        directly in front of you. It prepares you for any sudden changes. Stay relaxed.
        Don’t look back (unless it’s really necessary), focus ahead where you are going"""
        ""
    , Entry
        "Anticipate"
        """Anticipate when the group might be slowing for junction, bend in the road,
        traffic lights etc. Anticipate when you might need to change gear."""
        ""
    , Entry
        "Downhill on the drops"
        """Always descend on the drops if it’s a significant or proper hill.
        This lowers your centre of gravity, gives you more control of the bike,
        improved pull on the brakes, and it makes you faster!
        At speed being on the drops will keep your hands in control of the bike
        if you hit a pothole with force… on the brake hoods any impact could
        cause you to lose your grip. If you’re catching the riders in front
        (which you will if you’re in their slipstream),
        just move out into the wind to take off a bit of speed."""
        ""
    , Entry
        "Horses"
        """Great care should be taken when overtaking horses either solo or in a group
        as they’re nervous animals and are easily spooked. Shouting a warning ie
        “coming through”, slowing down, and also giving them plenty of room are
        the key elements in passing. Both rider and horse will appreciate this!"""
        ""
    , Entry
        "In the wet"
        """Moderate your speed and try to avoid going over white painted road markings
        especially while turning, they can be very slippery…. especially white domed roundabouts!!!
        The same applies to metalwork on the road… OK in straight line but try not to be
        turning and leaning the bike on metal covers or white paint surfaces."""
        ""
    , Entry
        "Awareness and Anticipation"
        """Always pay attention to what’s happening on the road ahead of the group,
        and also the general group dynamics. As a general rule of thumb it’s not
        a great idea to be going full bat through busy urban areas in a group."""
        ""
    , Entry
        "The three most important things to remember…"
        """- be predictable
- be predictable!
- be predictable!!

Here is a useful video about Group Riding
> https://www.youtube.com/watch?v=WL3RmtAlVM0

And another on general group riding etiquette
> https://www.youtube.com/watch?v=ODmB9LyYzKM&t"""
        ""
    ]


contentSection entries =
    wrappedRow [ alignTop, centerX, width fill, padding 20, spacing 20 ] <|
        List.map entryAsHeading entries


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
    Input.button
        [ spacing 10
        , alignTop
        , width (fill |> minimum 400 |> maximum 500)
        , Border.color FlatColors.BritishPalette.skirretGreen
        , Border.width 5
        , Border.rounded 10
        , padding 10
        ]
        { onPress = Just <| SelectEntry (Just entry)
        , label =
            paragraph [ Font.size 20, Font.color white ]
                [ text entry.title ]
        }


entryDetail : Entry -> Element Msg
entryDetail entry =
    Input.button
        [ centerX
        , centerY
        , width (fill |> minimum 400 |> maximum 500)
        , Border.color FlatColors.BritishPalette.skirretGreen
        , Border.width 5
        , Border.rounded 20
        , Background.color FlatColors.BritishPalette.blueberrySoda
        ]
        { onPress = Just <| SelectEntry Nothing
        , label =
            column [ spacing 10 ]
                [ paragraph
                    [ Font.size 24
                    , Font.color FlatColors.BritishPalette.nasturcianFlower
                    , Font.bold
                    , padding 10
                    , Background.color FlatColors.BritishPalette.skirretGreen
                    ]
                    [ text entry.title ]
                , paragraph
                    [ Font.size 20
                    , padding 10
                    , Font.color white
                    ]
                    [ html <| Markdown.toHtml [] entry.content ]
                ]
        }
