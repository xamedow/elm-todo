module View exposing (view, viewTodo, viewTodoList)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (Model(..), Msg(..), Todo)


viewTodo : Todo -> Html Msg
viewTodo todo =
    li []
        [ label [] [ text todo.title ]
        , input [ type_ "checkbox", checked todo.status, onClick (UpdateTodoStatus todo) ] []
        , button [ onClick (DeleteTodo todo.id) ] [ text "X" ]
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
