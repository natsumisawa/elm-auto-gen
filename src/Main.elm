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
    , textFormList : List TextForm
    , errorMessageMaybe : Maybe String
    }


type alias TextForm =
    { text : String
    , index : Int
    , parentIndexMaybe : Maybe Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" [ TextForm "" 0 Nothing ] Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeText String Int
    | ConvertText
    | AddInput Int
    | AddNestedInput Int (Maybe Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { textJson, textFormList } =
            model
    in
    case msg of
        ChangeText input index ->
            let
                changeTextFormMaybe =
                    ListExtra.getAt index textFormList
            in
            case changeTextFormMaybe of
                Just ctf ->
                    ( { model | textFormList = ListExtra.setAt index { ctf | text = input } textFormList }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ConvertText ->
            case Decoder.run (Decoder.array decoder) <| Array.fromList <| List.map (\tf -> tf.text) textFormList of
                Ok textList ->
                    ( { model | textJson = convertToJson <| Array.toList textList }, Cmd.none )

                Err err ->
                    ( { model | errorMessageMaybe = toErrorMessage err }, Cmd.none )

        AddInput nextIndex ->
            ( { model | textFormList = List.append textFormList [ TextForm "" nextIndex Nothing ] }, Cmd.none )

        AddNestedInput nextIndex parentIndexMaybe ->
            ( { model | textFormList = List.append textFormList [ TextForm "" nextIndex parentIndexMaybe ] }, Cmd.none )


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
        Just "未入力のフォームがあります"

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
        column [] <|
            [ el [ centerX ] <|
                row [ padding 10 ]
                    [ el [] <|
                        column [ spacing 10 ]
                            [ inputTableView textFormList
                            , errorMessageView errorMessageMaybe
                            ]
                    , el [] <|
                        Element.text
                            textJson
                    ]
            , el [ centerX ] convertButtonView
            ]


inputTableView : List TextForm -> Element Msg
inputTableView textFormList =
    indexedTable [ padding 10, spacing 30 ]
        { data = textFormList
        , columns =
            [ { header = Element.text ""
              , width = px 300
              , view =
                    \index textForm ->
                        row [ spacing 10 ] <|
                            inputView textForm.text
                                index
                                :: (if List.length textFormList == index + 1 then
                                        [ addNestedButtonView (index + 1) textForm.parentIndexMaybe
                                        , addButtonView (index + 1)
                                        ]

                                    else
                                        []
                                   )
              }
            ]
        }


inputView : String -> Int -> Element Msg
inputView text index =
    Element.Input.text
        [ width <| px 100
        , htmlAttribute <| Html.Attributes.autofocus True
        ]
        { onChange = \i -> ChangeText i index
        , text = text
        , placeholder = Nothing
        , label = Element.Input.labelHidden "text"
        }


addButtonView : Int -> Element Msg
addButtonView nextIndex =
    Element.Input.button
        [ Background.color <| Element.rgb255 102 102 255
        , padding 5
        , Element.focused
            [ Background.color <| Element.rgb255 102 102 255 ]
        ]
        { onPress = Just <| AddInput nextIndex
        , label = Element.text "AddInput"
        }


addNestedButtonView : Int -> Maybe Int -> Element Msg
addNestedButtonView nextIndex parentIndexMaybe =
    Element.Input.button
        [ Background.color <| Element.rgb255 102 102 255
        , padding 5
        , Element.focused
            [ Background.color <| Element.rgb255 102 102 255 ]
        ]
        { onPress = Just <| AddNestedInput nextIndex parentIndexMaybe
        , label = Element.text "AddNestedInput"
        }


errorMessageView : Maybe String -> Element Msg
errorMessageView errorMessageMaybe =
    case errorMessageMaybe of
        Just em ->
            Element.el [ Font.color (Element.rgb 255 0 0), Font.size 12 ] (Element.text em)

        Nothing ->
            Element.none


convertButtonView : Element Msg
convertButtonView =
    Element.Input.button
        [ Background.color <| Element.rgb255 102 102 255
        , padding 5
        , Element.focused
            [ Background.color <| Element.rgb255 102 102 255 ]
        ]
        { onPress = Just ConvertText
        , label = Element.text "Convert"
        }
