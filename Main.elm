module Main exposing (..)

import Html
import Html.App as HtmlApp
import Verb


type alias Model = {
    verb: Verb.Verb
}

model : Model
model = let verb = {
        declension = Verb.FirstDeclension,
        preverb = "გა",
        version = "ა",
        root = "კეთ",
        suffix = "ებ" }
    in {
        verb = verb
    }

type alias Msg = {}

update: Msg -> Model -> Model
update _ m = m

view : Model -> Html.Html Msg
view model =
      Html.div [] [ 
          Html.div [] [Html.text "Hello"],
          Html.div [] [Html.text ( Verb.conjugate Verb.Present {person=Verb.FirstPerson, plural=True} model.verb )] 
      ]

main = HtmlApp.beginnerProgram { model = model, view = view, update = update }
