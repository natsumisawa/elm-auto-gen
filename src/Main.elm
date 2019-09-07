module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input
import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (Html, a, h1, img, li, main_, section, text, ul)
import Html.Attributes exposing (href, src, style, target)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { text : String
    , textForm : String
    , errorMessageMaybe : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeText String
    | ConvertText


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { text, textForm } =
            model
    in
    case msg of
        ChangeText input ->
            ( { model | textForm = input }, Cmd.none )

        ConvertText ->
            case Decoder.run decoder textForm of
                Ok t ->
                    ( { model | text = t }, Cmd.none )

                Err err ->
                    ( { model | errorMessageMaybe = toErrorMessage err }, Cmd.none )


type Error
    = TextEmpty


toErrorMessage : List Error -> Maybe String
toErrorMessage errorList =
    if List.member TextEmpty errorList then
        Just "未入力です"

    else
        Nothing


decoder : Decoder String Error String
decoder =
    Decoder.identity
        |> Decoder.assert (Decoder.minLength TextEmpty 1)



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column
            []
        <|
            [ Element.table [ padding 10, spacing 30 ]
                { data = [ model ]
                , columns =
                    [ { header = Element.text ""
                      , width = px 300
                      , view =
                            \m ->
                                Element.column [ spacing 10 ] <|
                                    List.append
                                        [ Element.Input.text
                                            [ width <| px 100
                                            , htmlAttribute <| Html.Attributes.autofocus True
                                            ]
                                            { onChange = ChangeText
                                            , text = model.textForm
                                            , placeholder = Nothing
                                            , label = Element.Input.labelHidden "text"
                                            }
                                        ]
                                        (case model.errorMessageMaybe of
                                            Just em ->
                                                [ Element.el [ Font.color (Element.rgb 255 0 0), Font.size 12 ] (Element.text em) ]

                                            Nothing ->
                                                []
                                        )
                      }
                    , { header = Element.text "Json"
                      , width = px 300
                      , view =
                            \m ->
                                Element.column [ spacing 10 ]
                                    [ Element.text
                                        model.text
                                    ]
                      }
                    ]
                }
            , el [ centerX ] <|
                Element.Input.button
                    [ Background.color <| Element.rgb255 102 102 255
                    , padding 5
                    , Element.focused
                        [ Background.color <| Element.rgb255 102 102 255 ]
                    ]
                    { onPress = Just ConvertText
                    , label = Element.text "Convert"
                    }
            ]
