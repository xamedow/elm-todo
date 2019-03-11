module Model exposing (Model(..), Msg(..), Todo, init, update)

import Http
import Json.Decode exposing (Decoder, bool, list, string, succeed)
import Json.Decode.Pipeline as P


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



-- HTTP


getTodoList : Cmd Msg
getTodoList =
    Http.get { url = "http://localhost:3005/todo", expect = Http.expectJson GotTodoList todoListDecoder }


deleteTodo : String -> Cmd Msg
deleteTodo id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:3005/todo/" ++ id
        , body = Http.emptyBody
        , expect = Http.expectWhatever DeletedTodo
        , timeout = Nothing
        , tracker = Nothing
        }


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
