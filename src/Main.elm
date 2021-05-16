module Main exposing (main)

import Angle exposing (..)
import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode as Json
import Position exposing (..)
import WorldObject exposing (..)


type alias Tank =
    { hp : Int }


type alias Projectile =
    { damage : Int }


type alias Model =
    { tankA : WorldObject Tank
    , tankB : WorldObject Tank
    , projectiles : List (WorldObject Projectile)
    , currentMovement : Maybe KeyCommand
    }


type alias Flags =
    {}


height =
    480


width =
    640


init : Flags -> ( Model, Cmd msg )
init _ =
    ( { tankA = WorldObject { x = 0, y = height / 2 - tankHeight / 2 } (Degrees 0) { hp = 100 }
      , tankB = WorldObject { x = width - tankWidth, y = height / 2 - tankHeight / 2 } (Degrees 180) { hp = 100 }
      , projectiles = []
      , currentMovement = Nothing
      }
    , Cmd.none
    )


type Msg
    = AnimationFrame Float
    | KeyDown Int
    | KeyUp Int


type KeyCommand
    = Forward
    | Reverse
    | TurnLeft
    | TurnRight
    | Shoot


keyCommandFromCode : Int -> Maybe KeyCommand
keyCommandFromCode keyCode =
    case keyCode of
        32 ->
            Just Shoot

        37 ->
            Just TurnLeft

        38 ->
            Just Forward

        39 ->
            Just TurnRight

        40 ->
            Just Reverse

        _ ->
            Nothing


handleKeyCommand : Model -> KeyCommand -> ( Model, Cmd Msg )
handleKeyCommand model kcmd =
    case kcmd of
        Forward ->
            ( { model | currentMovement = Just Forward }, Cmd.none )

        Reverse ->
            ( { model | currentMovement = Just Reverse }, Cmd.none )

        TurnRight ->
            ( { model | currentMovement = Just TurnRight }, Cmd.none )

        TurnLeft ->
            ( { model | currentMovement = Just TurnLeft }, Cmd.none )

        Shoot ->
            -- TODO: Add projectile
            ( model, Cmd.none )


tankSpeed =
    -- TODO: what are the units here? pixels per second?
    0.1


tankRotateSpeed =
    0.05


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame d ->
            ( case model.currentMovement of
                Just Forward ->
                    { model | tankA = WorldObject.moveObject model.tankA (WorldObject.direction model.tankA) d tankSpeed }

                Just Reverse ->
                    { model | tankA = WorldObject.moveObject model.tankA (oppositeDirection (WorldObject.direction model.tankA)) d tankSpeed }

                Just TurnLeft ->
                    { model | tankA = WorldObject.rotateAnticlockwise model.tankA d tankRotateSpeed }

                Just TurnRight ->
                    { model | tankA = WorldObject.rotateClockwise model.tankA d tankRotateSpeed }

                _ ->
                    model
            , Cmd.none
            )

        KeyDown keyCode ->
            case keyCommandFromCode keyCode of
                Just keyCommand ->
                    handleKeyCommand model keyCommand

                Nothing ->
                    ( model, Cmd.none )

        KeyUp keyCode ->
            ( { model | currentMovement = Nothing }, Cmd.none )


subscriptions : model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta AnimationFrame


view : Model -> Browser.Document Msg
view model =
    { title = "Tanks!"
    , body =
        [ div []
            [ h1 []
                [ text "Tanks!"
                , input [ type_ "text", autofocus True, onKeyDown KeyDown, onKeyUp KeyUp ] []
                , div
                    [ style "border" "1px solid black"
                    , style "width" (String.fromInt width ++ "px")
                    , style "height" (String.fromInt height ++ "px")
                    , style "position" "relative"
                    ]
                    (List.map tankView [ model.tankA, model.tankB ] ++ List.map projectileView model.projectiles)
                ]
            ]
        ]
    }


tankWidth =
    50


tankHeight =
    50


tankView : WorldObject Tank -> Html Msg
tankView (WorldObject position direction tank) =
    div
        [ style "height" (String.fromInt tankWidth ++ "px")
        , style "width" (String.fromInt tankHeight ++ "px")
        , style "border" "1px solid darkgreen"
        , style "background" "green"
        , style "position" "absolute"
        , style "top" (String.fromFloat position.y ++ "px")
        , style "left" (String.fromFloat position.x ++ "px")
        , style "transform" ("rotate(" ++ String.fromInt (Angle.toDegrees direction) ++ "deg)")
        ]
        [ div
            [ style "background" "green"
            , style "border" "1px solid darkgreen"
            , style "width" (String.fromInt tankWidth ++ "px")
            , style "height" "10px"
            , style "position" "absolute"
            , style "top" "20px"
            , style "left" "25px"
            ]
            []
        ]


projectileView : WorldObject Projectile -> Html Msg
projectileView (WorldObject position direction p) =
    div [] [ text "TODO projectileView" ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    Html.Events.on "keydown" (Json.map tagger Html.Events.keyCode)


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    Html.Events.on "keyup" (Json.map tagger Html.Events.keyCode)


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
