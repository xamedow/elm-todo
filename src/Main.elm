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
    { newTodoTitle : Maybe String
    , todos : List Todo
    }


initialModel : Model
initialModel =
    { newTodoTitle = Nothing
    , todos = []
    }



-- UPDATE


type Msg
    = NewTitleChange String
    | AddTodo
    | ChangeTodoStatus Int Bool
    | DeleteTodo Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewTitleChange value ->
            if (String.length value) > 0 then
                    { model | newTodoTitle = Just value }
                else
                    { model | newTodoTitle = Nothing }

        AddTodo ->
            case model.newTodoTitle of
                Nothing ->
                    model
                Just title ->
                    { model | newTodoTitle = Nothing , todos = { title = title, status = False, id = List.length model.todos + 1 } :: model.todos }

        DeleteTodo id ->
            let
                deleteTodo todo =
                    not (todo.id == id)
            in
            { model | todos = List.filter deleteTodo model.todos }

        ChangeTodoStatus id status ->
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
    , input [ type_ "checkbox", checked todo.status, onClick (ChangeTodoStatus todo.id todo.status) ] []
    , button [ onClick (DeleteTodo todo.id) ] [ text "X" ]
     ]


viewTodos : Model -> Html Msg
viewTodos model =
    case model.todos of
        [] -> div [] [ text "No todos found, so go ahead and just add one!"]
        _ -> ul [] (List.map viewTodo model.todos)


isNewTitleEmpty : Model -> Bool
isNewTitleEmpty model =
    case model.newTodoTitle of
        Nothing ->
            False
        Just _ ->
            True

viewAddTodo : Model -> Html Msg
viewAddTodo model =
    section [] [ label [ for "new-todo-input" ] [ text "Enter todo title" ]
        , input [ id "new-todo-input", type_ "text", onInput NewTitleChange ] []
        , button [ disabled (isNewTitleEmpty model), onClick AddTodo ] [ text "Add todo" ]
        ]

view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text "My favorite todo list" ]
        , viewAddTodo model
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
