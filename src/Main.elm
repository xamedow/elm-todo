module Main exposing (main)

import Browser
import Model exposing (Model(..), Msg(..), Todo, init, update)
import View exposing (view)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
