module Main exposing (main)

import Array
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input
import Form.Decoder as Decoder exposing (Decoder)
import Html exposing (Html, a, h1, img, li, main_, section, text, ul)
import Html.Attributes exposing (href, src, style, target)
import List.Extra as ListExtra


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
    { textJson : String
    , textFormList : List String
    , errorMessageMaybe : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" [ "" ] Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeText String Int
    | ConvertText
    | AddInput Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { textJson, textFormList } =
            model
    in
    case msg of
        ChangeText input index ->
            ( { model | textFormList = ListExtra.setAt index input textFormList }, Cmd.none )

        ConvertText ->
            case Decoder.run (Decoder.array decoder) <| Array.fromList textFormList of
                Ok textList ->
                    ( { model | textJson = convertToJson <| Array.toList textList }, Cmd.none )

                Err err ->
                    ( { model | errorMessageMaybe = toErrorMessage err }, Cmd.none )

        AddInput index ->
            ( { model | textFormList = List.append textFormList [ "" ] }, Cmd.none )


type Error
    = TextEmpty


convertToJson : List String -> String
convertToJson textList =
    let
        jsonObjList =
            List.map
                (\text ->
                    "\t\"" ++ text ++ "\" -> ???.asJson,\n"
                )
                textList
    in
    String.replace ",\n)" "\n)" ("Json.obj(\n" ++ String.concat jsonObjList ++ ")")


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
    let
        { textJson, textFormList, errorMessageMaybe } =
            model
    in
    Element.layout [] <|
        Element.column
            []
        <|
            [ Element.indexedTable [ padding 10, spacing 30 ]
                { data = textFormList
                , columns =
                    [ { header = Element.text ""
                      , width = px 300
                      , view =
                            \index textForm ->
                                Element.column [ spacing 10 ] <|
                                    [ inputView textForm
                                        index
                                    , errorMessageView errorMessageMaybe
                                    ]
                      }
                    , { header = Element.text "Json"
                      , width = px 300
                      , view =
                            \index textForm ->
                                if index == 0 then
                                    Element.column [ spacing 10 ]
                                        [ Element.text
                                            textJson
                                        ]

                                else
                                    Element.none
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
                    { onPress = Just <| AddInput (List.length textFormList + 1)
                    , label = Element.text "AddInput"
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


inputView : String -> Int -> Element Msg
inputView textForm index =
    Element.Input.text
        [ width <| px 100
        , htmlAttribute <| Html.Attributes.autofocus True
        ]
        { onChange = \i -> ChangeText i index
        , text = textForm
        , placeholder = Nothing
        , label = Element.Input.labelHidden "text"
        }


errorMessageView : Maybe String -> Element Msg
errorMessageView errorMessageMaybe =
    case errorMessageMaybe of
        Just em ->
            Element.el [ Font.color (Element.rgb 255 0 0), Font.size 12 ] (Element.text em)

        Nothing ->
            Element.none
