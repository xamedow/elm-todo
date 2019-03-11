module Model exposing (Model(..), Msg(..), Todo, init, update)

import Http
import Json.Decode exposing (Decoder, bool, list, string, succeed)
import Json.Decode.Pipeline as P
import Json.Encode as JE


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



-- UPDATE


type Msg
    = GotTodoList (Result Http.Error (List Todo))
    | DeleteTodo String
    | DeletedTodo (Result Http.Error ())
    | UpdateTodoStatus Todo
    | UpdatedTodoStatus (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotTodoList (Ok todoList) ->
            ( Success todoList, Cmd.none )

        GotTodoList (Err error) ->
            ( Failure (Debug.toString error), Cmd.none )

        DeleteTodo id ->
            ( Loading, deleteTodo id )

        DeletedTodo (Ok ()) ->
            ( Loading, getTodoList )

        DeletedTodo (Err error) ->
            ( Failure (Debug.toString error), Cmd.none )

        UpdateTodoStatus todo ->
            ( Loading, updateTodoStatus todo )

        UpdatedTodoStatus (Ok ()) ->
            ( Loading, getTodoList )

        UpdatedTodoStatus (Err error) ->
            ( Failure (Debug.toString error), Cmd.none )



-- HTTP


endpoint =
    "http://localhost:3005/todo/"


getTodoList : Cmd Msg
getTodoList =
    Http.get { url = endpoint, expect = Http.expectJson GotTodoList todoListDecoder }


deleteTodo : String -> Cmd Msg
deleteTodo id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = endpoint ++ id
        , body = Http.emptyBody
        , expect = Http.expectWhatever DeletedTodo
        , timeout = Nothing
        , tracker = Nothing
        }


updateTodoStatus : Todo -> Cmd Msg
updateTodoStatus todo =
    let
        nextTodo =
            { todo | status = not todo.status }
    in
    Http.request
        { method = "PUT"
        , headers = []
        , url = endpoint ++ todo.id
        , body = Http.jsonBody (todoEncode nextTodo)
        , expect = Http.expectWhatever UpdatedTodoStatus
        , timeout = Nothing
        , tracker = Nothing
        }


todoEncode : Todo -> JE.Value
todoEncode todo =
    JE.object
        [ ( "id", JE.string todo.id )
        , ( "title", JE.string todo.title )
        , ( "status", JE.bool todo.status )
        ]


todoDecoder : Decoder Todo
todoDecoder =
    succeed Todo
        |> P.required "id" string
        |> P.required "title" string
        |> P.optional "status" bool False
        |> P.hardcoded False


todoListDecoder : Decoder (List Todo)
todoListDecoder =
    list todoDecoder
