module Main exposing (Model, Msg(..), initialModel, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MODEL


type alias Todo =
    { status : String
    , title : String
    }


type alias Model =
    { newTodoTitle : String
    , todos : List Todo
    }


initialModel : Model
initialModel =
    { newTodoTitle = ""
    , todos = []
    }



-- UPDATE


type Msg
    = NewTitleChange String
    | NewTitleAdd
    | StatusChange Todo


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewTitleChange value ->
            { model | newTodoTitle = value }

        NewTitleAdd ->
            { model | todos = { title = model.newTodoTitle, status = "todo" } :: model.todos }

        StatusChange todo ->
            { model | todos = todo :: model.todos }



-- VIEW


viewTodo : Int -> Todo -> Html Msg
viewTodo idx todo =
    li [] [ label [] [ text todo.title ], input [ type_ "checkbox", onClick StatusChange ] [] ]


viewTodos : Model -> Html Msg
viewTodos model =
    ul [] (List.indexedMap viewTodo model.todos)


view : Model -> Html Msg
view model =
    section []
        [ label [ for "new-todo-input" ] [ text "Enter todo title" ]
        , input [ id "new-todo-input", type_ "text", onInput NewTitleChange ] []
        , button [ onClick NewTitleAdd ] [ text "Add todo" ]
        , viewTodos model
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
