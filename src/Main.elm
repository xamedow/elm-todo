module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode exposing (Decoder, bool, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)



-- MODEL


type alias Todo =
    { id : String
    , title : String
    , isEditable : Bool
    , status : Bool
    }


type Model
    = Loading
    | Failure String
    | Success (List Todo)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getTodoList )



-- HTTP


getTodoList : Cmd Msg
getTodoList =
    Http.get { url = "http://localhost:3005/todo", expect = Http.expectJson GotTodoList todoListDecoder }



-- DECODERS


todoDecoder : Decoder Todo
todoDecoder =
    succeed Todo
        |> required "id" string
        |> required "title" string
        |> optional "status" bool False
        |> hardcoded False


todoListDecoder : Decoder (List Todo)
todoListDecoder =
    list todoDecoder



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = GotTodoList (Result Http.Error (List Todo))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotTodoList (Ok todoList) ->
            ( Success todoList, Cmd.none )

        GotTodoList (Err error) ->
            ( Failure (Debug.toString error), Cmd.none )



-- VIEW


viewTodo : Todo -> Html Msg
viewTodo todo =
    li []
        [ label [] [ text todo.title ]
        , input [ type_ "checkbox", checked todo.status ] []
        , button [] [ text "X" ]
        ]


viewTodoList : Model -> Html Msg
viewTodoList model =
    case model of
        Failure message ->
            div [] [ text ("There was an error during your request: " ++ message) ]

        Success todoList ->
            ul [] (List.map viewTodo todoList)

        Loading ->
            div [] [ text "Loading..." ]


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text "What needs to be done" ]
        , viewTodoList model
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
