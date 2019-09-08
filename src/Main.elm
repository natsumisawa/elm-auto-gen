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
    , hasChild : Bool
    , parentIndexMaybe : Maybe Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" [ TextForm "" 0 False Nothing ] Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeText String Int
    | ConvertText
    | AddInput Int
    | AddNestedInput TextForm


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
            case Decoder.run (Decoder.array decoder) <| Array.fromList textFormList of
                Ok tfl ->
                    ( { model | textJson = convertToJsonObj <| Array.toList tfl }, Cmd.none )

                Err err ->
                    ( { model | errorMessageMaybe = toErrorMessage err }, Cmd.none )

        AddInput nextIndex ->
            ( { model | textFormList = List.append textFormList [ TextForm "" nextIndex False Nothing ] }, Cmd.none )

        -- ネスト1回しかしないようにしているのは直す
        AddNestedInput parentForm ->
            let
                settedFormList =
                    case parentForm.parentIndexMaybe of
                        Just parentIndex ->
                            textFormList

                        Nothing ->
                            ListExtra.setAt parentForm.index { parentForm | hasChild = True } textFormList

                newForm =
                    TextForm "" (parentForm.index + 1) False (Just parentForm.index)
            in
            ( { model | textFormList = List.append settedFormList [ newForm ] }, Cmd.none )


type Error
    = TextEmpty


convertToJsonObj : List TextForm -> String
convertToJsonObj textFormList =
    let
        jsonObjList =
            List.map
                -- 厳しい。ばぐってる。concatMapにして中で細かく生成、最後に微調整すればいけそう？
                (\form ->
                    if form.hasChild then
                        "\t\""
                            ++ form.text
                            ++ "\" -> Json.obj(\n"

                    else
                        "\t\"" ++ form.text ++ "\" -> ???.asJson),\n"
                )
                textFormList
    in
    String.replace "),\n)" "\n\t)\n)" ("Json.obj(\n" ++ String.concat jsonObjList ++ ")") |> String.replace ")," ","


toErrorMessage : List Error -> Maybe String
toErrorMessage errorList =
    if List.member TextEmpty errorList then
        Just "未入力のフォームがあります"

    else
        Nothing


decoder : Decoder TextForm Error TextForm
decoder =
    Decoder.identity
        |> Decoder.assert minLengthValidator


minLengthValidator : Decoder.Validator TextForm Error
minLengthValidator =
    Decoder.custom <|
        \textForm ->
            if String.length textForm.text > 0 then
                Ok ()

            else
                Err [ TextEmpty ]



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
                        column [ spacing 10 ] <|
                            (case textForm.parentIndexMaybe of
                                Just parentIndex ->
                                    el [ alignRight ] <|
                                        inputView textForm.text
                                            index

                                Nothing ->
                                    el [ alignLeft ] <|
                                        inputView textForm.text
                                            index
                            )
                                :: (if List.length textFormList == index + 1 then
                                        case textForm.parentIndexMaybe of
                                            Just parentIndex ->
                                                [ el [ centerX ] <| addButtonView (index + 1)
                                                , el [ centerX ] <| addNestedButtonView textForm
                                                ]

                                            Nothing ->
                                                [ el [ centerX ] <| addNestedButtonView textForm
                                                , el [ centerX ] <| addButtonView (index + 1)
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
        [ width <| px 150
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
        [ Background.color <| Element.rgb255 245 255 250
        , padding 5
        , Element.focused
            [ Background.color <| Element.rgb255 0 0 0 ]
        ]
        { onPress = Just <| AddInput nextIndex
        , label = Element.text "Add!"
        }


addNestedButtonView : TextForm -> Element Msg
addNestedButtonView parentForm =
    Element.Input.button
        [ Background.color <| Element.rgb255 245 255 250
        , padding 5
        , Element.focused
            [ Background.color <| Element.rgb255 0 0 0 ]
        ]
        { onPress = Just <| AddNestedInput parentForm
        , label = Element.text "AddNest!"
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
        [ Background.color <| Element.rgb255 244 164 96
        , padding 5
        , Element.focused
            [ Background.color <| Element.rgb255 0 0 0 ]
        ]
        { onPress = Just ConvertText
        , label = Element.text "Convert!"
        }
