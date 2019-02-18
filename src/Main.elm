module Main exposing (Model, Msg(..), initialModel, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MODEL


type alias Todo =
    { id : Int
    , status : Bool
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
    | StatusChange Int Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewTitleChange value ->
            { model | newTodoTitle = value }

        NewTitleAdd ->
            { model
             | newTodoTitle = ""
             , todos = { title = model.newTodoTitle, status = False, id = List.length model.todos + 1 } :: model.todos }

        StatusChange id status ->
            let
                updateTodo todo =
                    if todo.id == id then
                        { todo | status = not status }
                    else
                        todo
            in
            { model | todos = List.map updateTodo model.todos }



-- VIEW
viewTodoLabel : Todo -> Html Msg
viewTodoLabel todo =
    if todo.status then
        s [] [ text todo.title ]
    else
        span [] [ text todo.title ]



viewTodo : Todo -> Html Msg
viewTodo todo =
    li [] [ label [] [ viewTodoLabel todo ]
    , input [ type_ "checkbox", checked todo.status, onClick (StatusChange todo.id todo.status) ] []
     ]


viewTodos : Model -> Html Msg
viewTodos model =
    case model.todos of
        [] -> div [] [ text "No todos found, so go ahead and just add one!"]
        _ -> ul [] (List.map viewTodo model.todos)


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text "My favorite todo list" ]
        , label [ for "new-todo-input" ] [ text "Enter todo title" ]
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
