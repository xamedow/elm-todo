module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode exposing (Decoder, bool, field, list, map4, string)



-- MODEL


type alias Todo =
    { id : String
    , title : String
    , isEditable : Bool
    , status : Bool
    }


type alias DataModel =
    { newTodoTitle : Maybe String
    , todos : List Todo
    , changedTodoValue : String
    }


type Model
    = Loading
    | Failure
    | Success (List Todo)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getTodos )



-- HTTP


getTodos : Cmd Msg
getTodos =
    Http.get { url = "http://localhost:3005/todo", expect = Http.expectJson GotTodos todoListDecoder }



-- DECODERS


todoDecoder : Decoder Todo
todoDecoder =
    map4 Todo
        (field "id" string)
        (field "title" string)
        (field "isEditable" bool)
        (field "status" bool)


todoListDecoder : Decoder (List Todo)
todoListDecoder =
    list todoDecoder



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = GotTodos (Result Http.Error (List Todo))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotTodos result ->
            case result of
                Ok todoList ->
                    ( Success todoList, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- VIEW


viewTodo : Todo -> Html Msg
viewTodo todo =
    li []
        [ label [] [ text todo.title ]
        , input [ type_ "checkbox", checked todo.status ] []
        , button [] [ text "X" ]
        ]


viewTodos : Model -> Html Msg
viewTodos model =
    case model of
        Failure ->
            div [] [ text "No todos found, so go ahead and just add one!" ]

        Success todos ->
            ul [] (List.map viewTodo todos)

        Loading ->
            div [] [ text "Loading..." ]


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text "What needs to be done" ]
        , viewTodos model
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
